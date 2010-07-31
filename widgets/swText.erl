-module(swText).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([make/7]).

-import(ex11_lib, [xCreateSimpleWindow/10,
		   ePolyText8/5,
		   rpc/2,
		   xAddAction/2,
		   xAddHandler/2,
		   xClearArea/1,
		   xColor/2,
		   xDo/2,
		   xFlush/1,
		   xGC/2,
		   xGetVar/2,
		   xSendMeAllEvents/2,
		   xSetVar/3,
		   xVar/2]).

-import(dict, [store/3]).
-include("sw.hrl").

-import(lists, [reverse/1, reverse/2, map/2]).

-define(TextColor, ?DarkBlue).

%% State0 = {file,F} of {str,S}
%% State1 = {Draw,Set,TextBuff1,MaxHt,0}
%%          Set (last value of Set command)

make(Parent, X, Y, Width, Ht, Color, F) ->
    spawn_link(fun() -> init(Parent, X, Y, Width, Ht, Color, F) end).

init(Parent, X, Y, Width, Ht, Color, File) ->
    Display = rpc(Parent, display),
    Attach = rpc(Parent, mountPoint),
    Wargs = #win{x=X,y=Y,width=Width, ht=Ht, parent=Attach,
		 color=Color, type=text,
		 mask = ?EVENT_EXPOSURE},
    Wargs1 = sw:mkWindow(Display, Parent, Wargs),
    Text = Wargs1#win.win, 
    MaxWidth = trunc((Width-7)/9),
    MaxHt = trunc(Ht/15),
    TextBuff = file2textBuff(File, MaxWidth),
    {Strs, TextBuff1} = get_screen(MaxHt, 0, TextBuff),
    GC = xVar(Display, sysFontId),
    B = add_text(Text, GC, Strs),
    Draw = fun() -> xDo(Display, B), xFlush(Display) end,
    Draw(),  
    loop(Display, Draw,0,TextBuff1,MaxHt,0,Wargs1).


loop(Display, Draw, Set, TextBuff, MaxHt, K0, Wargs) ->
    receive
	{event,_,expose,_} ->
	    Draw(),
	    loop(Display, Draw, Set, TextBuff, MaxHt, K0, Wargs);
	{addStr, Str} ->
	    TextBuff1 = add_str_tail(Str, TextBuff),
	    self() ! {set, Set},
	    loop(Display, Draw,Set,TextBuff1,MaxHt,K0, Wargs);
	{set,N} ->
	    #win{ht=Ht,win=Text}=Wargs,
	    case refresh(Display, Text, N, MaxHt,Ht,TextBuff,void) of
		noChange ->
		    loop(Display, Draw, Set, TextBuff, MaxHt, K0, Wargs);
		{TextBuff1, Refresh1, K1} ->
		    loop(Display, Refresh1, N, TextBuff1,MaxHt,K1, Wargs)
	    end;
	{From, size} ->
	    From ! {self(), {MaxHt, sizeOf(TextBuff)}},
	    loop(Display, Draw, Set, TextBuff, MaxHt, K0, Wargs);
	Any ->
	    Wargs1 = sw:generic(Any, Display, Wargs),
	    loop(Display, Draw, Set, TextBuff, MaxHt, K0, Wargs1)
    end.

file2textBuff({string,Str}, Width) ->
    reformat_file(binary_to_list(Str), Width);
file2textBuff({file,File}, Width) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    reformat_file(binary_to_list(Bin), Width);
	{error, _} ->
	    reformat_file("**** missing file " ++ File ++ "****", Width)
    end.

%% K = A*N + B
%% 0 = A*0 + B
%% Nlines-MaxHt = A*Ht + B
%%  A = (Nlines-MaxHt)/Ht.

refresh(Display, Text, N,MaxHt, Ht, TextBuff, K0) ->
%   A = (Nlines - MaxHt) / Ht,
 %  K = trunc(A*N),
    K = N,
    if
	K == K0 ->
	    noChange;
	true ->
	    xDo(Display, xClearArea(Text)),
	    {Strs, TextBuff1} = get_screen(MaxHt, K, TextBuff),
	    GC = xVar(Display, sysFontId),
	    B = add_text(Text, GC, Strs),
	    Refresh = fun() -> xDo(Display, B), xFlush(Display) end,
	    Refresh(),
	    {TextBuff1, Refresh, K}
    end.


add_text(Widget, GC, Strs) ->
    add_text(Widget,GC,20,20,Strs).

add_text(Widget, GC, X, Y, []) ->
    [];
add_text(Widget, GC, X, Y,	[Str|T]) ->
    %% io:format("Str=~p~n",[Str]),
    B = ePolyText8(Widget, GC, X, Y, Str),
    [B|add_text(Widget, GC, X, Y+15, T)].

get_screen(MaxHt, N, TextBuff) ->
    %% Make a screenfull of data of length MaxHt
    %% Skipping N lines at the start of the file
    TextBuff1 = adjust(N, MaxHt, TextBuff),
    After = take_n_from_after(MaxHt, TextBuff1),
    {After, TextBuff1}.

%% read_text(File, Max) ->
%%  {ok, lines()} | {error, Err}
%%  +type lines() = [{start,Str},{cont,C1},{cont,C2}...]
%%  
%%  each lines is stored as a sequence of one of more blocks
%%  starting {start,...} followed by one or more {continue, Str} blocks

reformat_file(Str, Max) ->
    {text, Max, [], reformat(Str, Max)}.


reformat(Chars, Max) ->
    Lines = break_into_lines(Chars, []),
    split_lines(Lines, Max, []).

break_into_lines([], L) ->
    reverse(L);
break_into_lines(Chars, L) ->
    {Line, Chars1} = get_line(Chars, []),
    break_into_lines(Chars1, [expand_tabs(Line)|L]).

get_line([$\n|T], L) -> {reverse(L), T};
get_line([], L)      -> {reverse(L), []};
get_line([H|T], L)   -> get_line(T, [H|L]).
    
split_lines([], _, L) ->
    reverse(L);
split_lines([H|T], Max, L) ->
    L1 = split_line(H, Max, L),
    split_lines(T, Max, L1).
 
split_line([], Max, L) -> [{start, []}|L];  %% empty lines
split_line(H, Max, L)  -> split_line(start, H, Max, L).

split_line(_, [], Max, L) ->
    L;
split_line(Tag, Str, Max, L) ->
    {Line1, Str1} = split(Str, Max, []),
    split_line(cont, Str1, Max, [{Tag,Line1}|L]).

split([], _, L)    -> {reverse(L), []};
split(Str, 0, L)   -> {reverse(L), Str};
split([H|T], N, L) -> split(T, N-1, [H|L]).

%% text buffers
%% {text, Width, Before, After}
%%    text in Before is in reverse order

%% Adjust the screen so there are N lines in the buffer
%% off the top of the screen (as in the diagram)
%% Hscreen = Ht of the screen

adjust(N, Hscreen, {text, Width, Before, After}) ->
    Hbuff = length(Before) + length(After),
    Skip = skip(Hbuff, Hscreen, N),
    {B,A} = adjust_before(length(Before), Skip, Before, After),
    {text, Width, B, A}.


%%       
%%        +---+              +---+---+---+         +---+
%%        |   |              |   |   |   |         |   |
%%        |   |              |   |   |   |         |   |
%%        |   |              |   |   |   |         |   |
%%   +----|---|----+         +---+---+---+         |   |
%%   |    |   |    |             |   |             |   |
%%   |    |   |    |             |   |          +--+---+---+
%%   |    |   |    |             |   |          |  |   |   |
%%   +----|- -|----+             |   |          |  |   |   |
%%        |   |                  |   |          |  |   |   |
%%        +---+                  +---+          +--+---+---+
%%
%%        set 3                  set 0          set K K > 5

%%  Hbuff = Ht of buffer          (9 in the example)
%%  Hscreen = Ht of screen buffer (3 in the example)
%%  N = desired set parameter
%%  Free = #lines at the bottom

skip(Hbuff, Hscreen, N) when Hbuff =< Hscreen  -> 0;
skip(Hbuff, Hscreen, N) ->
    Free = Hbuff - N - Hscreen,
    if 
	Free >= 0 ->
	    N;       %% diagrams 1 and 2
	true ->
	    Hbuff- Hscreen
    end.


adjust_before(Len, Want, [H|B], A) when Len > Want ->
    %% before is too big
    adjust_before(Len-1, Want, B, [H|A]);
adjust_before(Len, Want, B, [H|A]) when Len < Want ->
    %% too short
    adjust_before(Len+1, Want, [H|B], A);
adjust_before(_, _, B, A) -> 
    {B,A}.

sizeOf({text,_,B,A}) ->
    length(B) + length(A).

%% beginning_text({text,_,B,A}) ->
%%   map(fun({_,Str}) -> Str end, reverse(B)).

take_n_from_after(N, {text, _, Before, After}) ->
    take_n_from_after1(N, After).

take_n_from_after1(0, _) -> [];
take_n_from_after1(N, [{Tag,Str}|T]) ->
    [Str|take_n_from_after1(N-1, T)];
take_n_from_after1(_, _) ->
    [].

expand_tabs(Str) ->
    expand_tabs(Str, 0).

expand_tabs([$\t|T], N) ->
    %% need to add some characters
    Extra = case N rem 8 of
		K -> 8 - K
	    end,
    blanks(Extra) ++ expand_tabs(T, N+Extra);
expand_tabs([H|T], N) ->
    [H|expand_tabs(T, N+1)];
expand_tabs([], _) ->
    [].

blanks(0) -> [];
blanks(N) -> [$\s|blanks(N-1)].

add_str_tail(Str, {text,Width, Before, After}) ->    
    Extra = reformat(Str, Width),
    Before1 = reverse(After, Before),
    {text, Width, Before1, Extra}.











