-module(example20).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% started 2004-01-22 by joe@sics.se (Joe Armstrong)
%% This is a basic (empty) text editor

-export([start/0]).

-import(sw, [xStart/1]).

-include("sw.hrl").

-define(bg, 16#ffffcc).

start() -> spawn(fun win/0).

win() ->
    Display = xStart("3.2"),
    Win     = swTopLevel:make(Display, 400, 250, ?bg),
    Text    = swEdText:make(Win, 10,10,380,230,1,?white),
    S = self(),
    Text ! {onClick, fun(X) -> S ! {click, X} end},
    Text ! {onKey, fun(X) -> S ! {key, X} end},
    Text ! {show, ["012345678901234567890123456789",
		   "012345678901234567890123456789",
		   "click the mouse","","or","","type the keyboard"]},
    Text ! {blink,1,1},
    loop(Text).

loop(Text) ->
    receive
	{click, {X, Y}} ->
	    io:format("Clicked:~p ~p~n",[X,Y]),
	    Text ! {blink, X, Y},
	    loop(Text);
	{key, Args} -> 
	    Cmd = ex11_lib_keyboard_driver:analyse(Args),
	    Str = lists:flatten(io_lib:format("Pressed:~p=>~p",[Args,Cmd])),
	    Text ! {show,[Str]},
	    loop(Text);
	Any ->
	    io:format("example20 received:~p~n",[Any]),
	    loop(Text)
    end.










