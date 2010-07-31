-module(example29).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([start/0]).

-include("sw.hrl").

-import(swCanvas, [newPen/4, draw/3]).
-import(sw, [mkTopLevel/4,xStart/1,rpc/2]).
-import(lists, [foldl/3,map/2,seq/2]).

-define(bg, 16#ffffcc).

%% start returns after eveything is ready

start() ->
    spawn_link(fun() -> win() end).

win() ->
    Display = xStart("3.2"),
    P       = swLifts:make(),
    S = self(),
    P ! {onClick, fun(X) -> S ! X end},
    Win    = swTopLevel:make(Display, 380, 160, ?bg),
    Win ! {onClick, fun({_,X,Y,_,_}) -> io:format("Click ~p,~p~n",[X,Y]) end},
    Pos1     = swScrollbar:make(Win, 30, 10, 20, 100, 1, ?white, ?blue),
    Width1   = swScrollbar:make(Win, 10, 120, 100, 20, 1,  ?grey90, ?red),
    Pos1 ! {onMove, fun(X) -> P ! {setPos, 1, X} end},
    Width1 ! {onMove, fun(X) -> P ! {setDoor, 1, X} end},
    Pos2     = swScrollbar:make(Win, 140, 10, 20, 100, 1, ?white, ?blue),
    Width2   = swScrollbar:make(Win, 120, 120, 100, 20, 1,  ?grey90, ?red),
    Pos2 ! {onMove, fun(X) -> P ! {setPos, 2, X} end},
    Width2 ! {onMove, fun(X) -> P ! {setDoor, 2, X} end},
    Pos3     = swScrollbar:make(Win, 250, 10, 20, 100, 1, ?white, ?blue),
    Width3   = swScrollbar:make(Win, 230, 120, 100, 20, 1,  ?grey90, ?red),
    Pos3 ! {onMove, fun(X) -> P ! {setPos, 3, X} end},
    Width3 ! {onMove, fun(X) -> P ! {setDoor, 3, X} end},
    Pos4     = swScrollbar:make(Win, 350, 10, 20, 100, 1, ?white, ?blue),
    Pos4 ! {onMove, fun(X) -> P ! {setPos, 3, (X-50)*3} end},
    loop(P).

loop(P) ->
    receive
	{click, lift, Lift, Index} ->
	    P ! {setLiftLamp, Lift, Index, ?red};
	{click, floor, Floor, down} ->
	    P ! {setFloorLamp, 1, Floor, ?green};
	{click, floor, Floor, up} ->
	    P ! {setFloorLamp, 2, Floor, ?blue};
	Any ->
	    io:format("example29 received:~p~n",[Any])
    end,
    loop(P).

