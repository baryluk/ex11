-module(swEdText).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% Started 2004-01-25 by joe@sics.se joe armstrong

-export([make/7]).

-include("sw.hrl").

-import(ex11_lib, [ePolyText8/5, reply/2, rpc/2, sleep/1, 
		   xClearArea/1,
		   xDo/2, xFlush/1,
		   xVar/2]).
-import(lists, [mapfoldl/3]).

%% make(Parent, X, Y, Width, Ht, Border, Color) 

make(Parent, X, Y, Width, Ht, Border, Color) -> 
    spawn_link(fun() -> init(Parent,  X, Y, Width, Ht, Border, Color) end).

init(Parent, X, Y, Width, Ht, Border, Color) ->
    Display = rpc(Parent, display),
    swBlinker:ensure_blinker(Display),
    PWin = rpc(Parent, mountPoint),
    Wargs = #win{x=X, y=Y, parent=PWin,
		 border=Border,width=Width,ht=Ht,color=Color, 
		 type=label, 
		 mask = ?EVENT_EXPOSURE bor ?EVENT_BUTTON_PRESS
		 bor ?EVENT_KEY_PRESS},
    Wargs1 = sw:mkWindow(Display, Parent, Wargs),
    Win = Wargs1#win.win,
    GC =  xVar(Display, sysFontId),
    W = (Width - 20) div 9,
    H = (Ht - 20) div 18,
    loop(Display, Wargs1, GC, W, H, void(), void(), void()).

void() ->
    fun() -> void end.

loop(Display, Wargs, GC, W, H, F1, F2, F3) ->
    receive
	{event,_,buttonPress,{_,X,Y,_,_}} ->
	    {X1, Y1} = cursor_pos(X, Y, W, H),
	    F2({X1, Y1}),
	    loop(Display, Wargs, GC, W, H, F1, F2, F3);
	{event,_,expose, _} ->
	    F1(),
	    xFlush(Display),
	    loop(Display, Wargs, GC, W, H, F1, F2, F3);
	{event,_, keyPress, Args} ->
	    F3(Args),
	    loop(Display, Wargs, GC, W, H, F1, F2, F3);
	{show, Strs} ->
	    Win = Wargs#win.win, 
	    xDo(Display, xClearArea(Win)),
	    F = make_expose_fun(Strs, Win, GC, W, H, Display),
	    F(),
	    loop(Display, Wargs, GC, W, H, F, F2, F3);
	{blink, Col, Line} ->  
	    Win = Wargs#win.win, 
	    XX = 9*Col+2, YY = Line*18-14,
	    swBlinker:blink(Display, Win, XX, YY),
	    loop(Display, Wargs, GC, W, H, F1, F2, F3);
	{onClick, F} ->
	    loop(Display, Wargs, GC, W, H, F1, F, F3);
	{onKey, F} ->
	    loop(Display, Wargs, GC, W, H, F1, F2, F);
	{'EXIT', Pid, Why} ->
	    true;
	{From, size} ->
	    reply(From, {size, W, H}),
	    loop(Display, Wargs, GC, W, H, F1, F2, F3);
	{setWidthHt, Wn, Hn} = Cmd ->
	    %% This is slighly wrong must fix later
	    Wargs1 = sw:generic(Cmd, Display, Wargs),
	    W1 = (Wn-6) div 9,
	    H1 = Hn div 18,
	    loop(Display, Wargs1, GC, W1, H1, F1, F2, F3);
	Any ->
	    %% Now we call the generic operators 
	    Wargs1 = sw:generic(Any, Display, Wargs),
	    loop(Display, Wargs, GC, W, H, F1, F2, F3)
    end.

make_expose_fun(Strs, Win, GC, W, H, Display) ->
    io:format("Here W=~p H=~p~n",[W,H]),
    Cmds = paint_text(H, Strs, Win, GC, 18),
    fun() ->
	    xDo(Display, Cmds),
	    xFlush(Display)
    end.

paint_text(0, _,  _, _, _) -> [];
paint_text(_, [], _, _, _) -> [];
paint_text(H, [Str|T], Win, GC, Y) ->
    [ePolyText8(Win, GC, 10, Y, Str)|
     paint_text(H-1, T, Win, GC, Y+18)].

cursor_pos(X, Y, Width, Ht) ->
    Col  = nearest_character(X,Width),
    Line = nearest_line(Y,Ht),
    {Col, Line}.

nearest_character(X, Max) ->
    N = (X-1) div 9,
    if 
	N > Max -> Max;
	N < 1   -> 1;
	true -> N
    end.

nearest_line(Y, Max) ->
    N = (Y+14) div 18,
    if 
	N > Max -> Max;
	N < 1 -> 1;
	true -> N
    end.


