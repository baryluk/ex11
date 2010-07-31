-module(example27).


%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% started 2004-03-10 by joe@sics.se (Joe Armstrong)


-export([start/0]).

-import(sw, [xStart/1]).
-import(swCanvas, [newPen/4, draw/3, delete/2]).
-import(lists, [foreach/2, map/2]).

-include("sw.hrl").

-define(bg, 16#ffffcc).

start() -> spawn(fun win/0).

%% Lat = -90 - +90
%% Long = -180 - + 180
%% So make box that is 360 wide 180 high

win() ->
    Display = xStart("3.2"),
    Width = 720,
    Ht = 360,
    Win     = swTopLevel:make(Display, Width+20,Ht+20, ?bg),
    Canvas  = swCanvas:make(Win, 10,10,Width,Ht,1,?gray88),
    Self = self(),
    Win    ! {onReconfigure, fun(X1) -> Self ! {reconfigure,X1} end},
    Canvas ! {onClick, fun({_,X,Y,_,_}) ->
			       Self ! {click, X, Y}
		       end},
    newPen(Canvas, blue, ?blue, 1),
    newPen(Canvas, black, ?black, 1),
    Map = get_map(),
    draw_map(Canvas, Width, Ht, Map),
    loop(Canvas, Map, Width, Ht, Width, Ht).
    
%% X = A*Long+B
%%   0 = A*-180+B
%%   W = A*180+B
%%     W = A*360 A=W/360, B=180*A
%% Y = A*Lat+B
%%   0 = A*90+B
%%   H = A*-90+B
%%     H = A*-180; A=-H/180; B = -90*A

ll2xy(Lat, Long, Width, Ht) ->
    Ax = Width/360,
    Bx = 180*Ax,
    X0 = trunc(Ax*Long+Bx),
    Ay = -Ht/180,
    By = -90*Ay,
    Y0 = trunc(Lat*Ay + By),
    X = if
	    X0 < 0 -> 0;
	    X0 > Width -> Width;
	    true -> X0
	end,
    Y = if
	    Y0 < 0 -> 0;
	    Y0 > Ht -> Ht;
	    true -> Y0
	end,
    {X, Y}.

draw_map(C, Width, Ht, Map) ->
    foreach(fun(L) ->
		    L1 = map(fun({X,Y}) -> ll2xy(X, Y, Width, Ht) 
			     end, L),
		    %% io:format("L1=~p~n",[L1]),
		    draw(C, black, {lines, L1})
	    end, Map).

get_map() ->
    {ok, Bin} = file:read_file("mapdata.bin"),
    Lines = binary_to_term(Bin),
    Scale = 1/1000,
    map(fun(L) ->
		map(fun({X,Y}) -> {X*Scale,Y*Scale} end, L)
	end, Lines).

f2i(Str) ->
    case (catch list_to_float(Str)) of
	{'EXIT', _} -> 0;
	F -> trunc(F)
    end.

loop(Canvas, Map, W, H, W1, H1) ->
    %% This is a bit sneaky
    %% I don't redraw on every refresh - this is too expensive
    receive
	{click, X, Y} ->
	    io:format("Clicked at:~p ~p~n",[X,Y]),
	    loop(Canvas, Map, W, H, W1, H1);
	{reconfigure, {W2, H2}} ->
	    loop(Canvas, Map, W, H, W2, H2);	    
	_Any ->
	    loop(Canvas, Map, W, H, W1, H1)
    after 250 ->
	    if W =/= W1, H =/= H1 ->
		    Canvas ! {resize,W1-20,H1-20},
		    Canvas ! erase,
		    draw_map(Canvas, W1-20,H1-20,Map),
		    loop(Canvas, Map, W1, H1, W1, H1);
	       true ->
		    loop(Canvas, Map, W, H, W1, H1)
	    end
    end.








