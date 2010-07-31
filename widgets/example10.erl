-module(example10).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% started 2004-01-22 by joe@sics.se (Joe Armstrong)

-export([start/0]).

-import(sw, [xStart/1, rpc/2]).

-include("sw.hrl").

-define(bg, 16#ffffcc).

start() -> spawn(fun shell/0).

shell() ->
    Display = xStart("3.1"),
    Width = 650,
    Ht    = 350,
    Win   = swTopLevel:make(Display, Width, Ht, ?bg),
    Text  = swText:make(Win, 10, 10, Width-50, Ht-70, ?AntiqueWhite, 
			{file, "shell.txt"}),	 
    Scroll  = swScrollbar:make(Win, Width-30, 10, 20, Max=Ht - 72,0,
			     ?blue,?white),
    Text  ! {set,100000},
    Entry = swEntry:make(Win, 10, Ht-40, Width-20, "Enter stuff here > "),
    Entry ! {onReturn,
	     fun(Str) ->
		     Text  ! {addStr,Str},
		     Entry ! {set, ">"}
	     end},
    join(Text, Scroll, Max),
    loop().

    %% When     I = 0    we want N = 0
    %% and when I = Max  we want N = MaxLine-MaxHt
    %% So let   I = A*N + B
    %% Solve
    %%          0            = A*0 + B
    %%         MaxLine-MaxHt = A*Max
    %%

join(Text, ScrollBar, Max) ->
    {MaxHt, MaxLines} = rpc(Text, size),
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

loop() ->
    receive
	Any ->
	    io:format("top level received:~p~n",[Any]),
	    loop()
    end.




