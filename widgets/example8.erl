-module(example8).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([start/0, join/3]).

-import(sw, [xStart/1]).
-import(ex11_lib, [rpc/2]).

-include("sw.hrl").

-define(bg, 16#ffffcc).

start() ->
    spawn(fun win/0).

win() ->
    Display = xStart("3.1"),
    Win  = swTopLevel:make(Display, 740, 460, ?bg),
    Win ! {onClick, fun({_,X,Y,_,_}) -> io:format("Click ~p,~p~n",[X,Y]) end},
    Text = swText:make(Win, 10, 10, 680, 400, ?AntiqueWhite, 
		       {file, "intro.txt"}),	
    addButton(Win, 10, 420, "top",    fun(_) -> Text ! {set, 0} end),
    addButton(Win, 110, 420, "Middle", fun(_) -> Text ! {set, 20} end),
    addButton(Win, 210, 420, "End", fun(_) -> Text ! {set, 1000000} end),
    Scrollbar = swScrollbar:make(Win, 710, 10, 20, 400, 1, ?red, ?yellow),
    join(Text, Scrollbar, 400),
    loop().

loop() ->
    receive
	Any ->
	    io:format("top level received:~p~n",[Any]),
	    loop()
    end.


addButton(Win, X, Y, Text, Fun) ->
    Width = 9*length(Text) + 20,
    B = swButton:make(Win, X, Y, Width, 30, ?red, Text),
    B ! {onClick, Fun}.

join(Text, ScrollBar, Max) ->
    {MaxHt, MaxLines} = rpc(Text, size),
    %% When     I = 0    we want N = 0
    %% and when I = Max  we want N = MaxLine-MaxHt
    %% So let   I = A*N + B
    %% Solve
    %%          0            = A*0 + B
    %%         MaxLine-MaxHt = A*Max
    %%
    _B = 0,
    A = (MaxLines -  MaxHt)/Max,
    ScrollBar ! {onMove,
		 fun(I) ->
			 if 
			     I >= Max ->
				 Text ! {set, 1000000};
			     true ->
				 Text ! {set, trunc(A*I)}
			 end
		 end}.

