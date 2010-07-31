-module(example32).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% started 2004-01-22 by joe@sics.se (Joe Armstrong)
%% This is a basic pico text editor with emacs-like bindings

%%3456789012345678901234567890123456789012345678901234567890123456789012345678
%%3456789012345678901234567890123456789012345678901234567890123456789012345

-export([start/0]).

-import(sw, [xStart/1]).
-import(ex11_lib, [rpc/2]).

-include("sw.hrl").

-define(bg, 16#ffffcc).

start() ->
    spawn(fun() -> win() end).
		  
win() ->
    Display = xStart("3.2"), 
    Win     = swTopLevel:make(Display, 490, 470, ?bg),
    Win ! {onClick, fun(X) -> io:format("~p~n",[X]) end},
    Emacs1   = swEmacs:make(Win, 10,10, 50, 20,1,?white),
    Emacs1 ! {edit, "todo.txt"},
    Emacs2   = swEmacs:make(Win, 10,375, 50, 1,1,?white),
    Emacs2 ! {edit, "one.txt"},
    Emacs3   = swEmacs:make(Win, 10,405, 20, 3,1,?white),
    Emacs3 ! {edit, "three.txt"},
    Emacs4   = swEmacs:make(Win, 211,405, 20, 3,1,?white),
    Emacs4 ! {edit, "three.txt"},
    loop().

loop() ->
    receive
	Any ->
	    io:format("top level received:~p~n",[Any]),
	    loop()
    end.
