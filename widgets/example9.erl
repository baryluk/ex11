-module(example9).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([start/0]).

-import(sw, [xStart/1]).
-import(example8, [join/3]).

-include("sw.hrl").

-define(bg, 16#ffffcc).

start() ->
    spawn(fun win/0).

win() ->
    show_file("intro.txt").

show_file(File) ->
    Display = xStart("3.1"),
    Width = 850,
    Ht = 650,
    Win  = swTopLevel:make(Display, Width, Ht, ?bg),
    Text = swText:make(Win, 10, 10, Width-40, Ht-20, ?AntiqueWhite, 
		       {file, File}),	 
    Sht = Ht - 20 + 2,
    Scrollbar = swScrollbar:make(Win, Width-30, 10, 20, Sht, 0, ?blue, ?white),
    join(Text, Scrollbar, Sht),
    loop().

loop() ->
    receive
	Any ->
	    io:format("top level received:~p~n",[Any]),
	    loop()
    end.



