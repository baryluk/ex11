-module(example17).

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
    Win  = swTopLevel:make(Display,800, 70, ?bg),
    Rect = swRectangle:make(Win, 10, 10, 50, 50, 1, ?red),
    loop(370, Rect).

loop(0,_) ->
    true;
loop(N, Rect) ->
    receive
	after 2 ->
		Rect ! {right,2},
		loop(N-1, Rect)
    end.



