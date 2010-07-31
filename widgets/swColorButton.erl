-module(swColorButton).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%%% Create: 2004-01-01 by joe@sics.se

-export([make/7]).

-import(ex11_lib, [eChangeGC/2,
		   eConfigureWindow/2,reply/2, ePolyFillRectangle/3,
		   ePolyText8/5,eUnmapWindow/1,eMapWindow/1,
		   mkRectangle/4,rpc/2,sleep/1,xCreateGC/2,
		   xClearArea/1,xColor/2,
		   xDo/2,xFlush/1,xVar/2]).

-include("sw.hrl").

%% ColorButton
%%   onClick => Fun
%%   set,Color


%%  State = {Fun,Expose,In}

make(Parent, X, Y, Width, Ht, Border, C1) ->
    spawn_link(fun() -> init(Parent, X, Y, Width, Ht, Border, C1) 
	       end).

init(Parent, X, Y, Width, Ht, Border, C1) ->
    Display = rpc(Parent, display),
    Attach = rpc(Parent, mountPoint),
    Wargs = #win{x=X, y=Y, border=Border,width=Width,ht=Ht,color=C1, 
		 type=button, parent=Attach, 
		 mask = ?EVENT_EXPOSURE bor ?EVENT_BUTTON_PRESS
		},
    Wargs1 = sw:mkWindow(Display, Parent, Wargs),
    Win = Wargs1#win.win,
    C1x = xColor(Display, C1),
    GC = xCreateGC(Display, 
		   [{function,copy},
		    {line_width,1},
		    {line_style,solid},
		    {foreground, C1x}]),
    B = ePolyFillRectangle(Win, 
			   GC,
			   [mkRectangle(0,0,Width, Ht)]),
    Expose = 
	fun() ->
		xDo(Display, B),
		xFlush(Display)
	end,
    Click = fun(_) -> true end,
    loop(Display, Wargs1, Click, Expose, Win, C1, GC, Width, Ht).

loop(Display, Wargs, Fun, Expose, Win, Col, GC, Width, Ht) ->
    receive
	{set, Color} ->
	    Tmp = xColor(Display, Color),
	    xDo(Display, eChangeGC(GC, [{foreground,Tmp}])),
	    xFlush(Display), 
	    B = ePolyFillRectangle(Win, 
				   GC,
				   [mkRectangle(0,0,Width,Ht)]),
	    F = fun() ->
			xDo(Display, B),
			xFlush(Display)
		end,
	    self() ! {event,void,expose, void},
	    loop(Display, Wargs, Fun, F, Win, Color, GC, Width, Ht);
	{event,_,expose,_} ->
	    Expose(),
	    loop(Display, Wargs, Fun, Expose, Win, Col, GC, Width, Ht);
	{event, _, buttonPress, X} ->
	    Fun(X),
	    loop(Display, Wargs, Fun, Expose, Win, Col, GC, Width, Ht);
	{onClick, Fun1} ->
	    loop(Display, Wargs, Fun1, Expose, Win, Col, GC, Width, Ht);
	{From, read} ->
	    From ! {self(), Col},
	    loop(Display, Wargs, Fun, Expose, Win, Col, GC, Width, Ht);
	Other ->
	    Wargs1 = sw:generic(Other, Display, Wargs),
	    loop(Display, Wargs1, Fun, Expose, Win, Col, GC, Width, Ht)
    end.

    



