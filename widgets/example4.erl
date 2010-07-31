-module(example4).

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
    Win    = swTopLevel:make(Display, 265, 100, ?bg),
    Win ! {onClick, fun({_,X,Y,_,_}) -> io:format("Click ~p,~p~n",[X,Y]) end},
    Label  = swLabel:make(Win, 10, 50, 250, 30, 0, ?grey90, ""),
    Entry  = swEntry:make(Win, 10, 10, 250, "> Type CR in the entry ..."),
    Entry ! {onReturn, fun(_) -> 
			       Val = rpc(Entry, read),
			       Label ! {set, Val},
			       Entry ! {set, "> "}
		       end},
    loop().

loop() ->
    receive
	Any ->
	    io:format("top level received:~p~n",[Any]),
	    loop()
    end.


