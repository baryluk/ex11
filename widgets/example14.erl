-module(example14).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([start/0]).

-import(sw, [xStart/1]).

-include("sw.hrl").

-define(bg, 16#ffffcc).

start() ->
    spawn(fun win/0).

win() ->
    Display = xStart("3.2"),
    Win  =  swTopLevel:make(Display,800, 600, ?bg),
    X0 = 360, Y0=100,
    Dx1 = -150, Dy1 = 125,
    Dx2 = 150, Dy2 = 200,
    Drag = swDragBox:make(Win, X0, Y0,20,20,0,?red),
    Button = swButton:make(Win, X0+Dx1, Y0+Dy1, 350, 30, ?green, 
			     "Drag the red dot or the blue frame"),
    Entry = swEntry:make(Win, X0+Dx2, Y0+Dy2, 220, "A draggable entry"),
    Drag ! {onMove, fun(X, Y) ->
			    Button ! {setXY, X+Dx1, Y+Dy1},
			    Entry  ! {setXY, X+Dx2, Y+Dy2},
			    Button ! raise,
			    Entry ! raise
		    end},
    XX = 100, YY=300,
    DragBar   = swDragBox  :make(Win, XX, YY,    260, 10, 1,?blue),
    Rectangle = swRectangle:make(Win, XX, YY+16, 260, 200,1,?white),
    DragBar ! {onMove, fun(X, Y) ->
			     Rectangle ! raise,
			     Rectangle ! {setXY, X, Y+16}
		     end},
    loop().

loop() ->
    receive
	Any ->
	    io:format("example14 received:~p~n",[Any]),
	    loop()
    end.



