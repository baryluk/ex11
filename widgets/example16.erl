-module(example16).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([start/0]).

-include("sw.hrl").
-define(bg, 16#ffffcc).

-import(ex11_lib, [xDo/2,eListFonts/2,
		   rpc/2,
		   xColor/2,
		   xCreateGC/2,
		   xEnsureFont/2,
		   xSetVar/3]).

-import(lists, [foldl/3, mapfoldl/3, max/1, map/2]).
-import(sw, [xStart/1]).

start() ->
    spawn(fun() -> win() end).

win() ->
    Display = xStart("3.1"),
    Win  =  swTopLevel:make(Display,400, 300, ?bg),
    Button = swButton:make(Win, 10, 10, 120, 30, ?yellow, "Hide Menu"),
    Pop = pop(Win, 10, 50, 120, ["one", "two", "three", "four","six",
				 "seven"]),
    B1 = swFlashButton:make(Win,150, 10, 120, 30, 0, ?grey88, ?white,
			    "Flash"),
    Label = swLabel:make(Win, 180, 45, 200, 65, 1, ?green, "Dynamic help"),
    Label ! unmap,
    B1 ! {onClick, fun(X) ->
			   io:format("clicked:~p~n",[X])
		   end},
    B1 ! {onEnter, fun() -> Label ! map end},
    B1 ! {onLeave, fun() -> Label ! unmap end},
    Self = self(),
    Button ! {onClick, fun(_) -> Self ! click end},
    loop(Button, Pop, true).

loop(Button, Pop, Mapped) ->
    receive
	click ->
	    case Mapped of
		true ->
		    Button ! {set, "Popup"},
		    Pop ! unmap,
		    loop(Button, Pop, false);
		false ->
		    Button ! {set, "Hide menu"},
		    Pop ! map,
		    loop(Button, Pop, true)
	    end
    end.

pop(Win, X, Y, Width, L) ->
    Ht = 30*length(L),
    C = swRectangle:make(Win, X, Y, Width, Ht,0, ?orange),
    foldl(fun(Str, I) ->
		  F = swFlashButton:make(C,0,30*(I-1)+0,Width,30,0,
					 color(I), ?red, Str),
		  F ! {onClick,
		       fun(_) -> io:format("pressed:~p~n",
					   [I])
		       end},
		  I+1
	  end, 1, L),
    C.

color(1) -> ?ivory;
color(2) -> ?linen;
color(3) -> ?seashell;
color(4) -> ?azure;
color(5) -> ?LightGrey;
color(6) -> ?lavender;
color(7) -> ?MistyRose;
color(8) -> ?snow;
color(9) -> ?gainsboro.








