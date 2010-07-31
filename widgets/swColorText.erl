-module(swColorText).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([make/7]).

%% Protocol
%%   C !  {blink, X, Y}
%%   C !  {setSize, Width, Ht}
%%   C !  {mkPen, Name, Fg, Bg}
%%   C !  {display, X, Y, PenName, String}
%%   C !! read -> [{X,Y,Pen,Str}]
%%   C !  clearPage

-include("sw.hrl").

-compile(export_all).

-import(ex11_lib, [eImageText8/5,
		   ePolyText8/5, rpc/2, sleep/1, 
		   xClearArea/1,
		   xCreateGC/2,
		   xColor/2,
		   xDo/2, 
		   xEnsureFont/2,
		   xFlush/1,
		   xVar/2]).

-import(lists, [duplicate/2, foreach/2, reverse/1]).

make(Parent, X, Y, W, H, Border, Color) -> 
    %% W H in columns
    spawn_link(fun() -> init(Parent,  X, Y, W, H, Border, Color) end).


%% Screen State = {Width, Ht, Tuple} (size of Tuple = Ht)
%%   each line in the box is a list
%%   [{StartCol, GC, Str}]

init(Parent, X, Y, W, H, Border, Color) ->
    {Width, Ht} = sw:sizeInCols2pixels(W, H),
    Display = rpc(Parent, display),
    PWin = rpc(Parent, mountPoint),
    Wargs = #win{x=X, y=Y, parent=PWin,
		 border=Border,width=Width,ht=Ht,color=Color, 
		 type=label, mask = ?EVENT_EXPOSURE bor ?EVENT_BUTTON_PRESS
		 bor ?EVENT_KEY_PRESS},
    Wargs1 = sw:mkWindow(Display, Parent, Wargs),
    Win = Wargs1#win.win,
    %% Make a GC that gives us a red foreground on a white background ...
    GC1  = mkFace(Display, "9x15", ?red, ?white),
    GC2  = mkFace(Display, "9x15", ?white, ?black),
    GC3  = mkFace(Display, "9x15", ?black, ?white),
    Bin1 = eImageText8(Win, GC1, 10, 18, "hello (1)"),
    Bin2 = eImageText8(Win, GC2, 40, 50, "joe"),
    F = fun(_) -> void end,
    %% Make the initial matrix
    S = mk_page(W, H, GC3),
    io:format("S=~p~n",[S]),
    Font  = xEnsureFont(Display, "9x15"),
    %% Set up a couple of GC's for blinking
    Blink = {disabled, 10, 10, 
	     mkFace(Display,"9x15",?yellow,?red),
	     mkFace(Display,"9x15",Color,Color)},
    loop(Blink, F, F, Display, Win, dict:new(), Font, S, Wargs1).


clear_page({W,H,_}, GC) ->
    mk_page(W, H, GC).

mk_page(W, H, GC) ->
    Str = duplicate(W, 32),
    Line = [{1,GC,Str}],
    Lines = duplicate(H, Line),
    Tmp = list_to_tuple(Lines),
    {W,H,Tmp}.

blink(Display, Win, {off,I,J,Blink,Background}, S) ->
    C = char_under_blinker(I, J, S),
    %% color with Blink
    plot(Display, Win, I, J, Blink,[C]),
    xFlush(Display),
    {on,I,J,Blink,Background};
blink(Display, Win, {on,I,J,Blink,Background}, S) ->
    case get_char(I, J, S) of
	{GC, Char} ->
	    plot(Display, Win, I, J, GC,[Char]);
	none ->
	    plot(Display, Win, I, J, Background,[$\s])
    end,
    xFlush(Display),
    {off,I,J,Blink,Background};
blink(Display, Win, Disabled, S) ->
    Disabled.

move_blinker(I, J, Display, Win, Blink, S) ->
    case valid(I, J, S) of
	true ->
	    Blink1 = case element(1, Blink) of
			 on ->
			     blink(Display, Win, Blink, S);
			 off ->
			     Blink;
			 disabled ->
			     Blink
		     end,
	    %% Now it's off or disabled
	    {_,_,_,G1,G2} = Blink,
	    Blink2 = {off,I,J,G1,G2},
	    %% turn it on
	    blink(Display, Win, Blink2,S);
	false ->
	    Blink
    end.

valid(I, J, {W,H,_}) when I > 0, I =< W, J > 0, J =< H ->
    true;
valid(_,_,_) -> false.

char_under_blinker(I, J, S) ->
    case get_char(I, J, S) of
	none -> $\s;
	{_,Val} -> Val
    end.
	    
mkFace(Display, FontName, Fg, Bg) ->
    Font   = xEnsureFont(Display, FontName),
    xCreateGC(Display,  [{font, Font},
			 {background, xColor(Display, Bg)},
			 {foreground, xColor(Display, Fg)}]).

%% The display structure
%% {Line1, Line2, ....}
%%   Line = [{X,Gc,Str}]


loop(Blink, Fb, Fk, Display, Win, Pens, Font, S, Wargs) ->
    receive
	{clearPage, Name} ->
	    case dict:find(Name, Pens) of
		{ok, GC} ->
		    S1 = clear_page(S, GC),
		    display(Display, Win, S1),
		    loop(Blink, Fb, Fk, Display, Win, Pens, Font, S1, Wargs);
		error ->
		    exit({noPen,Name})
	    end;
	{blink, I, J} ->
	    Blink1 = move_blinker(I, J, Display, Win, Blink, S),
	    loop(Blink1, Fb, Fk, Display, Win, Pens, Font, S, Wargs);
	{onClick, F1} ->
	    loop(Blink, F1, Fk, Display, Win, Pens, Font, S, Wargs);
	{onKey, F1} ->
	    loop(Blink, Fb, F1, Display, Win, Pens, Font, S, Wargs);
	{event,_,buttonPress,{_,X,Y,_,_}} ->
	    {W,H,_} = S,
	    {X1, Y1} = sw:xyInPixels2cols(X, Y, W, H),
	    Fb({X1, Y1}),
	    loop(Blink, Fb, Fk, Display, Win, Pens, Font, S, Wargs);
	{event,_, keyPress, Args} ->
	    Fk(Args),
	    loop(Blink, Fb, Fk, Display, Win, Pens, Font, S, Wargs);
	{newPen, Name, Fg, Bg} ->
	    GC = xCreateGC(Display,  
			   [{font, Font},
			    {background, xColor(Display, Bg)},
			    {foreground, xColor(Display, Fg)}]),
	    Pens1 = dict:store(Name, GC, Pens),
	    loop(Blink, Fb, Fk, Display, Win, Pens1, Font, S, Wargs);
	{event,_,expose, _} ->
	    display(Display, Win, S),
	    loop(Blink, Fb, Fk, Display, Win, Pens, Font, S, Wargs);
	{display, X, Y, Name, Str} ->
	    Str1 = trim(X, Str, S),
	    case dict:find(Name, Pens) of
		{ok, GC} ->
		    plot(Display, Win, X, Y, GC, Str1),
		    xFlush(Display),
		    S1 = update(X, Y, GC, Str1, S),
		    loop(Blink, Fb, Fk, Display, Win, Pens, Font, S1, Wargs);
		error ->
		    exit({noPen,Name})
	    end;
	{'EXIT', Pid, Why} ->
	    true;
	Any ->
	    %% Now we call the generic operators 
	    Wargs1 = sw:generic(Any, Display, Wargs),
	    loop(Blink, Fb, Fk, Display, Win, Pens, Font, S, Wargs1)
    after 600 ->
	    Blink1 = blink(Display, Win, Blink, S),
	    loop(Blink1, Fb, Fk, Display, Win, Pens, Font, S, Wargs)
    end.

trim(X, Str, {W,_,_}) ->
    Last = length(Str) + X - 1,
    if Last >  W ->
	    reverse(remove(Last-W, reverse(Str)));
       true ->
	    Str
    end.

remove(0, Str) -> Str;
remove(N, [H|T]) -> remove(N-1, T).

display(Display, Win, {W,H,Tup}) ->
    %% statistics(runtime),
    display(Display, Win, H, 1, Tup).
    %% {_,Time} = statistics(runtime),
    %% io:format("Time=~p~n",[Time]).
	
display(Display, Win, 0, _, _) -> 
    xFlush(Display);
display(Display, Win, N, Y, Tup) ->
    E = element(Y, Tup),
    foreach(fun({X1,GC,Str}) ->
		    plot(Display, Win, X1, Y, GC, Str)
	    end, E),
    display(Display, Win, N-1, Y+1, Tup).

plot(Display, Win, X, Y, GC, Str) ->    
    {XX,YY} =  sw:xyInCols2pixels(X, Y),
    Bin = eImageText8(Win, GC, XX, YY, Str),
    xDo(Display, Bin).

update(X, Y, GC, Str, {W,H,Tup}) ->
    if
	Y > 0, Y =< H ->
	    E0 = element(Y, Tup),
	    % io:format("update X=~p Str=~p~n",[X,Str]),
	    % io:format("E0=~p~n",[E0]),
	    E1 = join({X,GC,Str}, E0),
	    % io:format("E1=~p~n",[E1]),
	    Tup1 = setelement(Y, Tup, E1),
	    {W, H, Tup1};
	true ->
	    exit({yOutOfRange, Y})
    end.

test() ->	    
    [{2,a,"abc567890"}] = join({2,a,"abc"}, [{3,a,"34567890"}]),
    [{3,a,"3"},{4,b,"abc"},{7,a,"7890"}] = 
	join({4,b,"abc"}, [{3,a,"34567890"}]).

%%----------------------------------------------------------------------
%% 

join(X, L) ->
    combine(merge(expand([X]), expand(L))).

expand(X) -> expand(X, []).

expand([{X,Tag,Str}|T], L) -> expand(T, expand(Str, X, Tag, L));
expand([], L)              -> L.

expand([H|T], X, Tag, L) -> expand(T, X+1, Tag, [{X,Tag,H}|L]);
expand([], _, _, L)      -> L.

merge(A, B) -> merge(A, B, []).

merge([{X,Tag,Val}=H1|T1], [{X,_,_}|T2], L) ->
    merge(T1, T2, [H1|L]);
merge([{X1,_,_}=H1|T1]=A1, [{X2,_,_}=H2|T2]=A2, L) ->
    if
	X1 > X2 ->
	    merge(T1, A2, [H1|L]);
	X1 < X2 ->
	    merge(A1, T2, [H2|L])
    end;
merge([], [H|T], L) ->
    merge([], T, [H|L]);
merge([H|T], [], L) ->
    merge([], T, [H|L]);
merge([], [], L) ->
    L.

combine(L) -> combine(L, []).

combine([], L) ->
    reverse(L);
combine([{X,Tag,C}|T], L) ->
    {Seq, T1} = get_seq(X+1,Tag,T,[C]),
    combine(T1, [{X,Tag,Seq}|L]).

get_seq(X,Tag,[{X,Tag,Char}|T], L) -> get_seq(X+1,Tag,T,[Char|L]);
get_seq(X, Tag, T, L)              -> {reverse(L), T}.

%%--- end join ---

%%----------------------------------------------------------------------
%% get_char(X,Y,S) -> none | {CG, Char}

get_char(X, Y, {_W,_H,Tup}) ->
    %% return the character at (X, Y) 
    %% => none, {Color, Value}
    get_char(X, element(Y, Tup)).

get_char(X, [{Start,GC,Str}|T]) when Start > X ->
    none;
get_char(X, [{Start,GC,Str}|T]) ->
    End = Start + length(Str) - 1,
    if
	X >= Start, X =< End ->
	    Skip = X - Start,
	    {GC, skip(Skip, Str)};
	true ->
	    get_char(X, T)
    end;
get_char(X, []) ->
    none.

skip(0, [H|_]) -> H;
skip(N, [_|T]) -> skip(N-1, T).

%%---- end get_char
    
