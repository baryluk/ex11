-module(example1).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([start/0]).

-include("sw.hrl").

-import(sw, [mkTopLevel/4,xStart/1]).

-define(bg, 16#ffffcc).

start() ->
    spawn_link(fun win/0).

win() ->
    Display = xStart("3.2"),
    Win     = swTopLevel:make(Display, 160, 50, ?bg),
    Button  = swButton:make(Win, 10, 10, 120, 30, ?grey88, "Hello"),
    Button  ! {onClick, fun(X) -> io:format("Click ~p~n",[X]) end},
    loop().

loop() ->
    receive
	Any ->
	    io:format("top level received:~p~n",[Any]),
	    loop()
    end.


