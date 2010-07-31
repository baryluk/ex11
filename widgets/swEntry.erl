-module(swEntry).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([make/5]).

-include("sw.hrl").

-import(ex11_lib,[eCopyArea/9,
		  eFreeGC/1,
		  eMapWindow/1,
		  ePolyFillRectangle/3,
		  ePolyText8/5,
		  mkRectangle/4,
		  rpc/2,
		  xClearArea/1,
		  xColor/2,
		  xCreateGC/2,
		  xCreatePixmap/4,
		  xCreateSimpleWindow/10,
		  xDo/2,
		  xFlush/1,
		  xGC/2,
		  xGetVar/2,
		  xSendMeAllEvents/2,
		  xSetInputFocus/1,
		  xSetVar/3,
		  xVar/2]).

-import(dict, [store/3]).
-import(lists, [reverse/2, seq/2]).

-record(e,{max,str,n,width,ht}).

%% entries
%%   There is only ONE place per window where input can be
%%   accepted
%%   There is ONE blinking inpout area per window per window
%%   Variables 
%%      {blinker,Win} => Pid
%%      {entry, E} => {Txt,Width,N}


make(Parent, X, Y, Width, Str) -> 
     spawn_link(fun() -> init(Parent, X, Y, Width, Str) end).

init(Parent, X, Y, Width, Str) ->
    Display = rpc(Parent, display),
    Attach = rpc(Parent, mountPoint),
    swBlinker:ensure_blinker(Display),
    Wargs = #win{x=X, y=Y, parent=Attach,border=0,width=Width,ht=28,
		 color=?white, type=button, 
		 mask = ?EVENT_EXPOSURE bor ?EVENT_BUTTON_PRESS bor
		 ?EVENT_KEY_PRESS},
    Wargs1 = sw:mkWindow(Display, Parent, Wargs),
    #win{width=Width,ht=Ht, win=Entry} = Wargs1,
    Max = (trunc((Width-4) div 9)),
    Data = #e{max=Max,str=Str,n=length(Str),width=Width,ht=Ht},
    B = [ePolyText8(Entry, xGC(Display, "text"), 4, 15, Str)|
	 sw:sunken_frame(Display, Entry, Width, Ht)],
    Draw = fun() -> xDo(Display, B), xFlush(Display) end,
    Draw(),
    %% setup some handlers
    loop(Display, Wargs1, Draw, Data, fun(_) -> void end).

loop(Display, Wargs, Draw, Data, Fun) ->
    receive
	{event,_,buttonPress, {_,X,Y,_,_}} ->
	    %% If there was a blinker running in another window then kill it
	    #win{win=Entry} = Wargs,
	    io:format("We have entered entry:~p at coords ~p ~p ~p ~n",
		      [Entry, X, Y, Data]),
	    #e{str=Str} = Data,
	    xDo(Display, xSetInputFocus(Entry)),
	    I = nearest_character(X, length(Str)),
	    XX = 9*I+5, YY = 4,
	    swBlinker:blink(Display, Entry, XX, YY),
	    Data1 = Data#e{n=I},
	    loop(Display, Wargs, Draw, Data1, Fun);
	{event,_,expose, _} ->
	    Draw(),
	    loop(Display, Wargs, Draw, Data, Fun);
	{event, _, keyPress, X} ->
	    io:format("Here X=~p~n",[X]),
	    Cmd = ex11_lib_keyboard_driver:analyse(X),
	    #win{win=Entry} = Wargs,
	    #e{str=Str,n=N,max=Max} = Data,
	    io:format("Cmd=~p~n",[Cmd]),
	    case do_cmd(Cmd, Str, N, Max, Fun) of
		{N1, Str1} ->
		    %% io:format("Here {N1,Str1}=~p~n",[{N1,Str1}]),
		    Data1 = Data#e{n=N1,str=Str1},
		    Draw1 = fix_entry(Display, Entry, Data1, Fun),
		    loop(Display, Wargs, Draw1, Data1, Fun);
		nothing ->
		    loop(Display, Wargs, Draw, Data, Fun)
	    end;
	{onReturn, Fun1} ->
	    loop(Display, Wargs, Draw, Data, Fun1);
	{set, Str} ->
	    #win{win=Entry} = Wargs,
	    Data1 = Data#e{str=Str,n=length(Str)},
	    Draw1 = fix_entry(Display, Entry, Data1, Fun),
	    loop(Display, Wargs, Draw1, Data1, Fun);
	{From, read} ->
	    Str = Data#e.str,
	    From ! {self(), Str},
	    loop(Display, Wargs, Draw, Data, Fun);
	{'EXIT', _, _} ->
	    true;
	Other ->
	    Wargs1 = sw:generic(Other, Display, Wargs),
	    loop(Display, Wargs1, Draw, Data, Fun)
    end.

fix_entry(Display, Entry, Data, Fun) ->
    #e{width=Width, n=N, ht=Ht, str=Str} = Data,
    B = [ePolyText8(Entry, xGC(Display, "text"), 4, 15, Str)|
	 sw:sunken_frame(Display, Entry, Width, Ht)],
    Draw = fun() -> xDo(Display, B) end,
    xDo(Display, xClearArea(Entry)),
    Draw(),
    XX = 9*N+5, YY = 4,
    swBlinker:blink(Display, Entry, XX, YY),
    Draw.


%%----------------------------------------------------------------------
%% keypress handler

do_cmd({_,_,_,{char,13}}, Str, N, Max, Fun) ->
    spawn(fun() -> Fun(Str) end),
    nothing;
do_cmd({_,_,_,{char,8}}, Str, 0, Max,_) -> % backspace
    nothing;
do_cmd({_,_,_,{char,8}}, Str, N, Max,_) ->    % backspace
    {[_|Before], After} = split(Str, N, []),
    {N-1, reverse(Before, After)};
do_cmd({_,_,Tag,{char,C}}, Str, N, Max, _) when Tag==none; Tag == shift->
    insert_char(C, Str, N, Max);
do_cmd({_,_,_, {cmd,left}}, Str, N, Max, _) when N > 0 ->
    {N-1, Str};
do_cmd({_,_,_, {keypad,left}}, Str, N, Max, _) when N > 0 ->
    {N-1, Str};
do_cmd({_,_,_, {cmd,right}}, Str, N, Max, _) when N < length(Str), N < Max ->
    {N+1, Str};
do_cmd({_,_,_, {keypad,right}}, Str, N, Max, _) when N < length(Str), N < Max ->
    {N+1, Str};
do_cmd(C, _, _, _, _) ->
    %% io:format("dropping:~p~n",[C]),
    nothing.

insert_char(C, Str, N, Max) when length(Str) < Max ->
    {Before,After} = split(Str, N, []),
    Str1 = reverse(Before, [C|After]),
    {N+1, Str1};
insert_char(C, Str, N, Max) ->
    {N, Str}.

split(Str, 0, L)   -> {L, Str};
split([H|T], N, L) -> split(T, N-1, [H|L]).

nearest_character(X, Max) when X < 0 -> 0;
nearest_character(X, Max) ->
    N = (X-4) div 9,
    if 
	N > Max -> Max;
	true    -> N
    end.

