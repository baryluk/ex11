-module(example13).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% Actually dragframes are redundant use DragBoxes instead
%% see example 14

-export([start/0]).

-import(sw, [xStart/1]).

-include("sw.hrl").

-define(bg, 16#ffffcc).

start() ->
    spawn(fun win/0).

win() ->
    Display = xStart("3.1"),
    Win  = swTopLevel:make(Display, 800, 600, ?bg),
    W1 = dragFrame(Win, 10, 30, 400, 200, ?blue),
    swButton:make(W1, 10, 14, 120, 30, ?green, "Erlang"),
    W2 = dragFrame(Win,100, 120, 450, 300, ?red),
    swButton:make(W2, 10, 14, 120, 30, ?yellow, "is"),
    W2a = dragFrame(W2, 100, 120, 250, 150, ?black),
    swButton:make(W2a, 10, 40, 120, 30, ?grey88, "recursive"),
    loop().

dragFrame(Win, X, Y, Width, Ht, Color) ->
    B    = swDragBox:make(Win, X, Y, Width, 10, 1, Color),
    Rect = swRectangle:make(Win, X, Y+10, Width, Ht-10,1,?white),
    B ! {onMove, fun(XX,YY) ->
			 Rect ! {setXY, XX, YY+10},
			 Rect ! raise
		 end},
    Rect.

loop() ->
    receive
	Any ->
	    io:format("example13 received:~p~n",[Any]),
	    loop()
    end.
