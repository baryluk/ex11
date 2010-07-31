-module(example30).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([start/0]).

-include("sw.hrl").

-import(sw, [mkTopLevel/4,rpc/2,xStart/1]).

-define(bg, 16#ffffcc).

start() ->
    spawn_link(fun win/0).

win() ->
    Display  = xStart("3.2"),
    Win      = swTopLevel:make(Display, 160, 130, ?bg),
    Button1  = swButton:make(Win, 10, 10, 120, 30, ?grey88, "blue"),
    Button2  = swButton:make(Win, 10, 40,120,30,?grey88, "green"),
    Button3  = swButton:make(Win, 10, 70,120,30,?grey88, "read"),
    C = swColorButton:make(Win, 10,110,10,10,1,?grey88),
    Button1  ! {onClick, fun(_) -> C ! {set,?blue} end},
    Button2  ! {onClick, fun(_) -> C ! {set,?green} end},
    C ! {onClick, fun(_) -> C ! {set, ?red} end},
    Button3 ! {onClick, fun(_) ->
				Val = rpc(C, read),
				io:format("Color=~p~n",[Val])
			end},
    loop().

loop() ->
    receive
	Any ->
	    io:format("top level received:~p~n",[Any]),
	    loop()
    end.


