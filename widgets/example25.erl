-module(example25).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% started 2004-03-01 by joe@sics.se (Joe Armstrong)
%% A fancy frame

-export([start/0]).

-import(sw, [xStart/1]).
-import(swCanvas, [newPen/4, draw/3, delete/2]).

-include("sw.hrl").

-define(bg, 16#ffffcc).

start() -> spawn(fun win/0).

win() ->
    Display = xStart("3.2"),
    Win     = swTopLevel:make(Display, 180, 80, ?bg),
    Self = self(),
    Win ! {onClick, fun({_,X,Y,_,_}) ->
			       Self ! {click, X, Y}
		       end},
    newPen(Win, black, ?black, 2),
    draw(Win, black, {lines, [{31,16},{20,16},{20,68},{168,68},
			      {168,16},{96,16}]}),
    draw(Win, black,   {text, 40,20, "A button"}),
    swButton:make(Win, 30, 30, 120, 30, ?grey88, "Hello"),
    loop().


loop() ->
    receive
	Any ->
	    io:format("top level received:~p~n",[Any]),
	    loop()
    end.





