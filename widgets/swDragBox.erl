-module(swDragBox).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([make/7]).

-import(ex11_lib,[
		  eConfigureWindow/2,
		  eMapWindow/1,
		  reply/2,
		  rpc/2,
		  xColor/2,
		  xCreateWindow/10,
		  xSendMeAllEvents/2,
		  xDo/2,
		  xFlush/1]).

-include("sw.hrl").

make(Parent, X, Y, W, H, B, Color) ->
    spawn_link(fun() -> init(Parent, X, Y, W, H, B, Color) end).

init(ParentPid, X, Y, Width, Ht, B, Color) ->
    Display = rpc(ParentPid, display),
    Attach = rpc(ParentPid, mountPoint),
    %% This is the outer frame
    Wargs = #win{x=X,y=Y,width=Width, ht=Ht, border=B, type=dragbox,
		 color= Color, parent=Attach,
		 mask = ?EVENT_BUTTON_PRESS bor ?EVENT_BUTTON1_MOTION},
    Wargs1 = sw:mkWindow(Display, ParentPid, Wargs),
    WW = rpc(ParentPid, winfo),
    io:format("WW=~p~n",[WW]),
    #win{width=ParentW, ht=ParentH} = WW, 
    Size = {ParentW-Width,ParentH-Ht},
    Win = Wargs1#win.win, 
    loop(Display, Wargs1, Win, Size, 0,0,0,0, fun(_,_) -> void end).

loop(Display,Wargs,Win, Size,X0,Y0,Xr,Yr, F) ->
    receive
	{onMove, F1} ->
	    loop(Display,Wargs,Win, Size,X0,Y0,Xr,Yr, F1);
	{event,_, buttonPress, {_,_,_,Xr1,Yr1}} ->
	    %% Note the origonal position
	    #win{x=X0_old,y=Y0_old}=Wargs,   
	    %% X0, Y0 = the origonal position of the window
	    %% Xr, Yr = origonal position of the pointer in the root frame
	    xDo(Display, eConfigureWindow(Win, [{stackMode, above}])),
	    xFlush(Display),
	    F(X0_old, Y0_old),
	    loop(Display,Wargs,Win,Size,X0_old,Y0_old,Xr1,Yr1, F);
	{event,_,motionNotify, {_,_,_,Xr1,Yr1}} ->
	    %% Xr1, Yr1 = New position of the pointer in the root frame
	    Dx = Xr1 - Xr,
	    Dy = Yr1 - Yr,
	    XX = X0 + Dx,
	    YY = Y0 + Dy,
	    case ok(XX,YY,Size) of
		true ->
		    xDo(Display, eConfigureWindow(Win, [{x,XX},{y,YY}])),
		    xFlush(Display),
		    Wargs1 = Wargs#win{x=XX, y=YY},
		    F(XX, YY),
		    loop(Display,Wargs1,Win, Size,X0,Y0,Xr,Yr, F);
		false ->
		    loop(Display,Wargs,Win, Size,X0,Y0,Xr,Yr, F)
	    end;
	Msg ->
	    Wargs1 = sw:generic(Msg, Display, Wargs),
	    loop(Display,Wargs1,Win, Size,X0,Y0,Xr,Yr, F)
    end.


ok(XX, YY, {MaxX, MaxY}) when XX > 0, YY > 0, XX < MaxX, YY < MaxY ->
    true;
ok(XX, YY,_) ->
    false.

