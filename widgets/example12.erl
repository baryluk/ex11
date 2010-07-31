-module(example12).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([start/0]).

-import(sw, [xStart/1, rpc/2]).

-import(lists, [map/2, mapfoldl/3, max/1]).

-include("sw.hrl").

-define(bg, 16#ffffcc).

start() ->
    spawn(fun win/0).

win() ->
    Val = form(25, ["Title", "Keywords", "Subject",
		    "Author", "Date", "Link"]),
	Val.

form(Chars, Data) ->
    S = self(),
    Pid = spawn_link(fun() -> form_widget(S, Chars, Data) end),
    receive
      {Pid, Reply} ->
	    Reply
    end.

form_widget(S, Len, Labels) ->
    %% spmewhat experimental
    %% closing things down is a problem
    %% find the max length of the Labels
    Max = max(map(fun(I) -> length(I) end,  Labels)),
    WidthLabel = Max*9+20,
    WidthEntry = 9*Len+20,
    Width = WidthEntry + WidthLabel+40,
    Ht = length(Labels)*30 + 65,
    Display = xStart("3.1"),
    Win  = swTopLevel:make(Display,Width, Ht, ?bg),
    {Pids,Y1} = mapfoldl(fun(Txt,Y) ->
				 mkLabel1(Win, 10, Y, WidthLabel, Txt),
				 Entry = swEntry:make(Win, WidthLabel+20, Y, 
						      WidthEntry, ""),
				 {Entry, Y + 30}
			 end, 10, Labels),
    But1 = swButton:make(Win,10, Y1+10, 120, 30, ?yellow, "Accept"),
    Me = self(),
    But1 ! {onClick,  
	    fun(_) ->
		    io:format("Pid=~p~n",[Pids]),
		    Vals = map(fun(I) -> rpc(I, read) end, Pids),
		    io:format("Vals=~p~n",[Vals]),
		    S ! {self(), {vals, Vals}},
		    Me ! done
	    end},
    loop().

loop() ->
    receive
	done ->
	    true;
	Any ->
	    io:format("form received:~p~n",[Any]),
	    loop()
    end.


mkLabel1(Win, X, Y, Width, Txt) ->
    swLabel:make(Win, X, Y, Width, 30, 0, ?grey88, Txt).

