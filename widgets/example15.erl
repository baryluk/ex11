-module(example15).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([start/0]).

-import(sw, [xStart/1]).
-import(ex11_lib, [xDo/2, eGetKeyboardMapping/2]).
-import(lists, [foreach/2]).

-include("sw.hrl").

-define(bg, 16#ffffcc).

start() ->
    spawn(fun win/0).

win() ->
    Display = xStart("3.2"),
    {First,Last} = K = ex11_lib:get_display(Display, keycodes),
    io:format("K=~p~n",[K]),
    {ok, {keys, Val}} = xDo(Display, eGetKeyboardMapping(First,Last)),
    io:format("Val=~p~n",[Val]),
    foreach(fun({I,[I1,I2,I3,I4]}) ->
		    io:format("I=~p ~s ~s ~s ~s~n",[I,f(I1),f(I2),f(I3),f(I4)])
	    end, Val),
    _Win  =  swTopLevel:make(Display,800, 600, ?bg),
    loop().

loop() ->
    receive
	Any ->
	    io:format("example15 received:~p~n",[Any]),
	    loop()
    end.

f(0) -> "0";
f(I) when I < 256 ->
    [I];
f(I) ->
    ex11_lib_utils:int2hex(I).

