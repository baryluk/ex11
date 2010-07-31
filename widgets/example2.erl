-module(example2).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([start/0]).

-include("sw.hrl").

-import(sw, [mkTopLevel/4,xStart/1]).
-import(lists, [flatten/1]).

-define(bg, 16#ffffcc).

start() ->
    spawn_link(fun win/0).

win() ->
    Display = xStart("3.2"),
    io:format("example2 Display=~p~n",[Display]),
    Win    = swTopLevel:make(Display, 240, 100, ?bg),
    Win ! {onClick, fun({_,X,Y,_,_}) -> io:format("Click ~p,~p~n",[X,Y]) end},
    Button = swButton:make(Win, 10, 10, 120, 30, ?grey88, "Hello"),
    Label  = swLabel:make(Win, 10, 50, 220, 40, 0, ?green, "Hi Joe"),
    Button ! {onClick, fun(X) -> 
			       Str = flatten(io_lib:format("CLICK:~p",[X])),
			       Label ! {set, Str}
		       end},
    loop().

loop() ->
    receive
	Any ->
	    io:format("top level received:~p~n",[Any]),
	    loop()
    end.


