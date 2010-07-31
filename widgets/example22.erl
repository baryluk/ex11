-module(example22).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% started 2004-02-28 by joe@sics.se (Joe Armstrong)
%% A basic canvas

%% started 2004-02-27 by joe@sics.se (Joe Armstrong)
%% This is a basic clock

-export([start/0]).

-import(sw, [xStart/1]).

-include("sw.hrl").

-define(bg, 16#ffffcc).

start() -> spawn(fun win/0).

win() ->
    Display = xStart("3.2"),
    Win     = swTopLevel:make(Display, 670, 520, ?bg),
    swClock:make(Win, 10,10,440,1,?white),
    swClock:make(Win, 460,10,200,1,?white),
    swClock:make(Win, 460,240,200,1,?white),
    for(10,650,60,
	fun(I) ->
		swClock:make(Win, I,460,50,1,?yellow)
	end),
    loop(Win).

for(I, Max, _Step, _F) when I >= Max ->
    true;
for(I, Max, Step, F) ->
    F(I),
    for(I+Step,Max,Step,F).

loop(Text) ->
    receive
	Any ->
	    io:format("example22 received:~p~n",[Any]),
	    loop(Text)
    end.










