-module(test1).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([start/0]).

-include("sw.hrl").

-import(sw, [xStart/1]).

-define(bg, 16#ffffcc).

start() ->
    spawn(fun win/0).

%% test error recovery
%% 

win() ->
    process_flag(trap_exit, true),
    Display = xStart("3.2"),
    Win    = swTopLevel:make(Display, 450, 170, ?bg),
    io:format("top win=~p~n",[Win]),
    Win ! {onClick, fun({_,X,Y,_,_}) -> io:format("Click ~p,~p~n",[X,Y]) end},
    Button1 = swButton:make(Win, 10, 10, 400, 30, ?grey88, "evaluate exit(Button2, die)"),
    Button2 = swButton:make(Win, 10, 50, 120, 30, ?red, "Hello"),
    Button3 = swButton:make(Win, 10, 90, 400, 30, ?orange, "top level process leaves control loop"),
    Button4 = swButton:make(Win, 10, 130, 400, 30, ?orange, "top level process exits with error"),
    Button1 ! {onClick,fun(_) -> exit(Button2, die) end},
    S = self(),
    Button3 ! {onClick,fun(_) -> S ! leave end},
    Button4 ! {onClick,fun(_) -> S ! evalBadCode end},
    loop().

loop() ->
    receive
	evalBadCode ->
	    1 = 2, %% gives us a badmatch
	    loop();
	leave ->
	    io:format("Process:~p terminating~n",[self()]),
	    true;
	Any ->
	    io:format("top level received:~p~n",[Any]),
	    loop()
    end.


