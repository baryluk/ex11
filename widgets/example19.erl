-module(example19).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% started 2004-01-22 by joe@sics.se (Joe Armstrong)

-export([start/0]).

-import(sw, [xStart/1]).

-include("sw.hrl").

-define(bg, 16#ffffcc).

start() -> spawn(fun win/0).

win() ->
    Display = xStart("3.2"),
    Win     = swTopLevel:make(Display, 400, 250, ?bg),
    XX = 10, YY=10,
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
	    io:format("example19 received:~p~n",[Any]),
	    loop()
    end.



