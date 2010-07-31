-module(example31).

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

%% Note the resize code is buggy

-export([start/0, start/1]).

-import(sw, [xStart/1]).
-import(ex11_lib, [rpc/2]).
-import(lists, [duplicate/2,foldl/3,reverse/1, reverse/2]).

-include("sw.hrl").

-define(bg, 16#ffffcc).

start() -> start("emacs.erl").

start(File) ->
    spawn(fun() -> win(File) end).
		  
win(File) ->
    Display = xStart("3.2"), 
    WidthCols = 80, HtCols=10,
    {Width, Ht} = sw:sizeInCols2pixels(WidthCols, HtCols),
    Win     = swTopLevel:make(Display, Width+20, Ht+20, ?bg),
    Emacs   = swEmacs:make(Win, 10,10, WidthCols, HtCols,1,?white),
    Emacs ! {edit, File},
    loop().

loop() ->
    receive
	Any ->
	    io:format("top level received:~p~n",[Any]),
	    loop()
    end.
