-module(example5).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([start/0]).

-include("sw.hrl").

-import(sw, [mkTopLevel/4,xStart/1,rpc/2]).

-define(bg, 16#ffffcc).

start() ->
    spawn_link(fun win/0).

win() ->
    Display = xStart("3.2"),
    Win    = swTopLevel:make(Display, 235, 200, ?bg),
    Win ! {onClick, fun({_,X,Y,_,_}) -> io:format("Click ~p,~p~n",[X,Y]) end},
    S1  = swScrollbar:make(Win, 10, 10, 200, 30, 1, ?grey90, ?red),
    S2  = swScrollbar:make(Win, 60, 50, 10, 120, 1, ?white, ?blue),
    Label1  = swLabel:make(Win, 120, 52, 50, 30, 0, ?grey90, "   "),
    Label2  = swLabel:make(Win, 95, 100, 50, 30, 0, ?grey90, "   "),
    S1 ! {onMove, fun(X) -> 
			 Label1 ! {set, integer_to_list(X)}
		 end},
    S2 ! {onMove, fun(X) -> 
			 Label2 ! {set, integer_to_list(X)}
		 end},
    loop().

loop() ->
    receive
	Any ->
	    io:format("top level received:~p~n",[Any]),
	    loop()
    end.


