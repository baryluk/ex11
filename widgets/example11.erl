-module(example11).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([start/0]).

-import(sw, [xStart/1]).
-import(ex11_lib, [rpc/2]).

-include("sw.hrl").

-define(bg, 16#ffffcc).

start() ->
    spawn_link(fun win/0).

win() ->
    Display = xStart("3.1"),
    Width = 950,
    Ht = 350,
    Win  = swTopLevel:make(Display, Width, Ht, ?bg),
    Text = swText:make(Win,150, 45, Width-190, Ht-70, ?AntiqueWhite, 
		       {file, "visual.txt"}),	 
    S1   = swScrollbar:make(Win,Width-30,45,20,Max=Ht - 72, 0, ?green, ?white),
    example8:join(Text, S1, Max),
    Target = swRectangle:make(Win,380,240,100,50,1,?orange),
    LabelInfo =  swLabel:make(Win,10, 10, 700, 30, 2, ?PaleGreen, 
			      " WHERE IS it? "),
    button_col(Win,10, 45, 30, 3,
	       [{"Up", fun(_) -> Target ! {up,5} end},
		{"Down", fun(_)  -> Target ! {down, 5} end},
		{"Left", fun(_)  -> Target ! {left, 5} end},
		{"Right", fun(_) -> Target ! {right, 5} end},
		{"Wider", fun(_) -> Target ! {wider, 5} end},
		{"Narrower", fun(_) -> Target ! {narrower, 5} end},
		{"Taller", fun(_) -> Target ! {taller, 5} end},
		{"Shorter", fun(_) -> Target ! {shorter, 5} end},
		{"Where", fun(_) -> 
				  Winfo = rpc(Target,winfo),
				  %#win{x=XX,y=YY} = Winfo,
				  Str = lists:flatten(io_lib:format("~p",
								    [Winfo])),
				  LabelInfo ! {set, Str}
			  end}
	       ]),
    loop().

loop() ->
    receive
	Any ->
	    io:format("top level received:~p~n",[Any]),
	    loop()
    end.


button_col(Win, X, Y, Ht, Dy, [{Txt, Fun}|T]) ->
    Width = 9*length(Txt) + 20,
    B = swButton:make(Win, X, Y, Width, Ht, ?grey88, Txt),
    B ! {onClick, Fun},
    button_col(Win, X, Y+Ht+Dy, Ht, Dy, T);
button_col(_, _, _, _, _, []) ->
    [].
    

