-module(example18).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% Color picker

%%% --------------------------------------------------------------------
%%% Created: 2004-01-30 by joe@sics.se

-export([start/0]).

-import(ex11_lib,[colors/0]).
-import(lists, [map/2,reverse/1,seq/2]).
-import(sw, [xStart/1]).
-include("sw.hrl").

-define(bg, 16#ffffcc).

start() -> spawn(fun() -> win() end).

win() ->
    Rows = 20,
    Cols = 35,
    Size=  18,
    Gap = 6, 
    Colors = colors(), 
    Display = xStart("3.1"),
    make_page(Display, Colors, Rows, Cols, Size, Gap),
    loop(Display).


make_page(Display, Buttons, Rows, Cols, Size, Gap) ->
    Width = Cols*(Size+Gap)+20, 
    Ht =  (Rows)*(Size+Gap) + 35, 
    Win = swTopLevel:make(Display, Width, Ht, ?bg),
    Label = swLabel:make(Win, 10, Ht-40, 200, 30, 0, ?bg, "Click on a color"),
    drawButtons(Win, Label, Buttons, 1, Cols, 10, 10, Size, Gap, Ht),
    loop(Display).

drawButtons(_Win, _Label, [], _, _, _, _, _, _, _) ->
    true;
drawButtons(Win, Label,[{Text,Val}|T], Max, Max, X, Y, Size, Gap, Ht) ->
    addButton(Win, Label, X, Y, Size, Val, Text),
    drawButtons(Win, Label, T, 1, Max, 10, Y+Size+Gap, Size, Gap, Ht);
drawButtons(Win, Label,[{Text,Val}|T], N, Max, X, Y, Size, Gap, Ht) ->
    addButton(Win, Label, X, Y, Size, Val, Text),
    drawButtons(Win, Label, T, N+1, Max, X+Size+Gap, Y, Size, Gap, Ht).

addButton(Win, Label, X, Y, Size, Val, Text) ->
    B = swFlashButton:make(Win, X, Y, Size, Size, 0, Val, ?white, ""),
    B ! {onClick, fun(_) ->
			  Label ! {set, Text}
		  end}.
loop(Display) ->
    receive
	Other ->
	    io:format("unexpected message=~p~n",[Other]),
	    loop(Display)
    end.








