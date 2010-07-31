-module(example3).

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
    Win    = swTopLevel:make(Display, 350, 145, ?bg),
    Win ! {onClick, fun({_,X,Y,_,_}) -> io:format("Click ~p,~p~n",[X,Y]) end},
    _Label1  = swLabel:make(Win, 10, 10, 220, 30, 0, ?cornsilk, "First name:"),
    Entry1  = swEntry:make(Win, 140, 10, 120, "Peg leg"),
    _Label2  = swLabel:make(Win, 10, 60, 220, 30, 0, ?cornsilk, "Last name:"),
    Entry2  = swEntry:make(Win, 140, 60, 120, "Loombucket"),
    Button  = swButton:make(Win, 10, 100, 120, 30, ?grey88, "Swap"),
    Button ! {onClick, fun(_X) -> 
			       Val1 = rpc(Entry1, read),
			       Val2 = rpc(Entry2, read),
			       Entry1 ! {set, Val2},
			       Entry2 ! {set, Val1}
		       end},
    loop().

loop() ->
    receive
	Any ->
	    io:format("top level received:~p~n",[Any]),
	    loop()
    end.


