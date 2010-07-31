-module(example28).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% started 2004-04-22 by joe@sics.se (Joe Armstrong)
%% erlPoint

%%3456789012345678901234567890123456789012345678901234567890123456789012345678
%%3456789012345678901234567890123456789012345678901234567890123456789012345

-export([start/0, start/1]).

-import(sw, [xStart/1]).
-import(ex11_lib, [eListFonts/2,
		   rpc/2, xDo/2
		  ]).
-import(lists, [duplicate/2,foldl/3,reverse/1, reverse/2]).

-include("sw.hrl").

-define(bg, 16#ffffcc).

-record(e, {width,     % width
	    ht,        % height,
	    data,      % lines
	    cx,        % X pos of cursor
	    cy,        % Y pos of cursor
	    text,      % Pid of the text Widget
	    start,     % start line in lines of first row in buffer
	    kill = [], % kill ring
	    current,   % start of current line containg the cursor
	    col}).     % index before the current line

start() -> start("emacs.erl").

start(File) ->
    spawn(fun() -> win(File) end).
		  
win(File) ->
    Display = xStart("3.2"), 
    Width   = 500, Ht = 400,
    Win     = swTopLevel:make(Display, Width, Ht, ?bg),
    Text    = swErlPoint:make(Win, 10,10, Width-20, Ht-20,1,?azure),
    _Lines = read_file(File),
    %% -adobe-courier-bold-r-normal--12-120-75-75-m-70-iso8859-1
    Re = "-*-*-*-*-*-*-*-*-*-*-*-*-iso8859-1",
    {ok, F} = xDo(Display, eListFonts(100000, Re)),
    io:format("Len = ~p ~p~n",[length(F),F]),
    S = self(),
    Text ! {onClick, fun(X) -> S ! {click, X} end},
    Text ! {onKey,   fun(X) -> S ! {key, X}   end},
    Text ! {display, page()},
    loop(a).

page() ->
    [{pen,code,1,?wheat1},
     {face,small,?black,"9x15"},
     %% use xfontsel to get the names of the
     %% font selector in KDE
     {face, script, ?red, "*-urw-chancery l-*-*-*-*-80-*-*-*-*-*-*-*"},
     {text, small, 10,20,"hello"},
     {text, script, 10, 70, "Joe's power point"},
     {rectangle, code, 45,167,200,100}].

read_file(F) ->
    {ok, Bin} = file:read_file(F),
    {ok, Bin}.

loop(State) ->
    receive
	{click, {X, Y}} ->
	    io:format("Clicked:X=~p Y=~p~n", [X,Y]),
	    loop(State);
	{key, Args} -> 
	    Cmd = ex11_lib_keyboard_driver:analyse(Args),
	    io:format("Pressed:~p=>~p~n",[Args,Cmd]),
	    State1 = key_pressed(Cmd, State),
	    loop(State1);
	{resized, _Width, _Ht} ->
	    loop(State);
	Any ->
	    io:format("example20 received:~p~n",[Any]),
	    loop(State)
    end.

key_pressed({_,_, Mod, Cmd}, State) ->
    handle_key(Mod, Cmd, State).

handle_key(none, {cmd, right}, _State) ->
    a.
