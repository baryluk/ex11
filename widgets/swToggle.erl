-module(swToggle).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%%% Create: 2004-01-01 by joe@sics.se

-export([make/9]).

-import(ex11_lib, [eChangeGC/2,
		   eConfigureWindow/2,reply/2, ePolyFillRectangle/3,
		   ePolyText8/5,eUnmapWindow/1,eMapWindow/1,
		   mkRectangle/4,rpc/2,sleep/1,xCreateGC/2,
		   xClearArea/1,xColor/2,
		   xDo/2,xFlush/1,xVar/2]).

-include("sw.hrl").

%%  State = {Fun,Expose,In}

make(Parent, X, Y, Width, Ht, Border, C1, C2) ->
    spawn_link(fun() -> init(Parent, X, Y, Width, Ht, Border, C1, C2) 
	       end).

init(Parent, X, Y, Width, Ht, Border, C1, C2, Str) ->
    Display = rpc(Parent, display),
    Attach = rpc(Parent, mountPoint),
    Wargs = #win{x=X, y=Y, border=Border,width=Width,ht=Ht,color=C1, 
		 type=button, parent=Attach, 
		 mask = ?EVENT_EXPOSURE bor ?EVENT_BUTTON_PRESS bor 
		 ?EVENT_ENTER_WINDOW bor ?EVENT_LEAVE_WINDOW
		},
    Wargs1 = sw:mkWindow(Display, Parent, Wargs),
    Win = Wargs1#win.win,
    C1x = xColor(Display, C1),
    C2x = xColor(Display, C2),
    GC = xCreateGC(Display, 
		   [{function,copy},
		    {line_width,1},
		    {line_style,solid},
		    {foreground, C1x}]),
    %% on expose  we just set the text
    B = [ePolyText8(Win, xVar(Display, sysFontId), 10, 18, Str)],
    %% On Enter we change to C2
    Enter = 
	fun() ->
		xDo(Display, eChangeGC(GC, [{foreground,C2x}])),
		xDo(Display, 
		    ePolyFillRectangle(Win, 
				       GC,
				       [mkRectangle(0,0,Width, Ht)])),
		xDo(Display, B),
		xFlush(Display)
	end,
    Leave = 
	fun() ->
		xDo(Display, eChangeGC(GC, [{foreground,C1x}])),
		xDo(Display, 
		    ePolyFillRectangle(Win, 
				       GC,
				       [mkRectangle(0,0,Width, Ht)])),
		xDo(Display, B),
		xFlush(Display)
	end,
    loop(Display, fun(_) -> void end,  fun() -> void end,  fun() -> void end, 
	 Enter,Leave, Wargs1).


loop(Display, Fun, FunE, FunL, Enter,Leave,Wargs) ->
    receive
	{event,_,expose,_} ->
	    Leave(),
	    loop(Display, Fun, FunE, FunL, Enter,Leave,Wargs);
	{event,_,enterNotify,_} ->
	    Enter(),
	    FunE(),
	    loop(Display, Fun, FunE, FunL, Enter,Leave,Wargs);
	{event,_,leaveNotify,_} ->
	    Leave(),
	    FunL(),
	    loop(Display, Fun, FunE, FunL, Enter,Leave,Wargs);
	{event, _, buttonPress, X} ->
	    flash(Display, Wargs#win.win,Leave),
	    Fun(X),
	    loop(Display, Fun, FunE, FunL, Enter,Leave,Wargs);
	{onClick, Fun1} ->
	    loop(Display, Fun1, FunE, FunL, Enter,Leave,Wargs);
	{onEnter, Fun1} ->
	    loop(Display, Fun, Fun1, FunL, Enter,Leave,Wargs);
	{onLeave, Fun1} ->
	    loop(Display, Fun, FunE, Fun1, Enter,Leave,Wargs);
	Other ->
	    Wargs1 = sw:generic(Other, Display, Wargs),
	    loop(Display, Fun, FunE, FunL, Enter,Leave,Wargs1)
    end.

flash(Display, Win, Draw) ->
    spawn(fun() ->
		  xDo(Display, xClearArea(Win)),
		  xFlush(Display),
		  sleep(200),
		  Draw(),
		  xFlush(Display)
	  end).

    

    



