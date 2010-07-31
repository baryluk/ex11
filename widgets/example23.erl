-module(example23).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% started 2004-02-28 by joe@sics.se (Joe Armstrong)
%% A basic canvas

-export([start/0]).

-import(sw, [xStart/1]).
-import(swCanvas, [newPen/4, draw/3, delete/2]).

-include("sw.hrl").

-define(bg, 16#ffffcc).

start() -> spawn(fun win/0).

win() ->
    Display = xStart("3.2"),
    Win     = swTopLevel:make(Display, 460, 460, ?bg),
    Canvas  = swCanvas:make(Win, 10,10,440,440,1,?gray88),
    Self = self(),
    Canvas ! {onClick, fun({_,X,Y,_,_}) ->
			       Self ! {click, X, Y}
		       end},
    newPen(Canvas, thin, ?green, 2),
    newPen(Canvas, red, ?red, 1),
    newPen(Canvas, blue, ?blue, 1),
    newPen(Canvas, green, ?green, 1),
    draw(Canvas, thin, {line, 10,10,100,100}),
    draw(Canvas, blue, {filledRectangle,10,30,100, 5}),
    draw(Canvas, blue, {filledArc, 180, 120, 80, 15*64, 90*64}),
    draw(Canvas, green, {filledCircle, 220, 160, 40}),
    Cs = for(40, 400, 50,
	     fun(I) ->
		     draw(Canvas, blue, {filledCircle, I, 350, 15})
	     end),
    io:format("Cs = ~p~n",[Cs]),
    draw(Canvas, red,   {rectangle, 180,120,80,80}),
    draw(Canvas, blue,  {arc, 280, 120, 80, 15*64, 90*64}),
    draw(Canvas, green, {circle, 320, 160, 40}),
    draw(Canvas, red,   {rectangle, 280,120,80,80}),
    draw(Canvas, red,   {lines, [{44,105},{120,153},{69,208},{80,146}]}),
    draw(Canvas, green, {filledPoly, 
			 [{44,205},{120,253},{69,308},{80,246}]}),
    draw(Canvas, red,   {text, 30, 400, "Click to remove the blue circles"}),
    loop(Canvas, Cs).

loop(Canvas, Cs) ->
    receive
	{click, X, Y} ->
	    io:format("Clicked at:~p ~p~n",[X,Y]),
	    case Cs of
		[H|T] ->
		    delete(Canvas, H),
		    loop(Canvas, T);
		[] ->
		    loop(Canvas, Cs)
	    end;
	Any ->
	    io:format("example22 received:~p~n",[Any]),
	    loop(Canvas, Cs)
    end.

for(I, Max, _Step, _F) when I >= Max ->
    [];
for(I, Max, Step, F) ->
    [F(I)|for(I+Step,Max,Step,F)].
