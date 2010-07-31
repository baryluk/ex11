-module(example26).

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
    XX = 40, YY=20,
    {Width, Ht} = sw:sizeInCols2pixels(XX, YY),
    Win     = swTopLevel:make(Display, Width+20, Ht+20, ?bg),
    Rect    = swColorText:make(Win, 10,10, XX,YY,1,?grey88),
    S = self(),
    Rect ! {onClick, fun(X) -> S ! {click, X} end},
    Rect ! {onKey, fun(X)   -> S ! {key,   X} end},
    Rect ! {newPen, normal, ?black, ?white},
    Rect ! {newPen, rev, ?white, ?black},
    Rect ! {newPen, button, ?white, ?red},
    Rect ! {display, 1,1,normal,"This is normal text"},
    Rect ! {display, 2,2,rev,"This is reversed"},
    Rect ! {display, 3,3,button,"A button"},
    Rect ! {display, 6,1,button,"is"},
    Rect ! {display, 7,2,button,"is"},
    Rect ! {display, 10,2,normal,"rev"},
    Rect ! {display, 20,20,button,"click - 1"},
    Rect ! {display, XX-2,YY,normal,"endXYZ"},
    loop(Rect).

loop(Rect) ->
    receive
	{click,{X,Y}} ->
	    Rect ! {blink, X,Y},
	    loop(Rect);
	Any ->
	    io:format("top level received:~p~n",[Any]),
	    loop(Rect)
    end.








