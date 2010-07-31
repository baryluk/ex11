-module(example0).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([start/0]).

-import(sw, [mkTopLevel/4,xStart/1]).

-include("sw.hrl").
-define(bg, 16#ffffcc).

start() ->
    spawn(fun() -> win(examples1()) end).

win(Examples) ->
    Connection = xStart("3.1"),
    Ht = 35 * length(Examples) + 20,
    Win  = swTopLevel:make(Connection, 290, Ht, ?bg),
    add_examples(Win, 10, 10, 240, Examples),
    loop().

loop() ->
    receive
	Any ->
	    io:format("top level received:~p~n",[Any]),
	    loop()
    end.

examples1() ->
    [{"Quit", fun() -> erlang:halt() end},
     {"(1) Button", fun() -> example1:start() end, "example1.erl"},
     {"(2) Button + Label", fun() -> example2:start() end, "example2.erl"},
     {"(3) Entry + Label", fun() -> example3:start() end, "example3.erl"},
     {"(4) Cr event in entry", fun() -> example4:start() end, "example4.erl"},
     {"(5) Scrollbar", fun() -> example5:start() end,"example5.erl"},
     {"(6) Progress Bar", fun() -> example6:start() end, "example6.erl"},
     {"(7) Resizable button", fun() -> example7:start() end, "example7.erl"},
     {"(8) Srollable text", fun() -> example8:start() end, "example8.erl"},
     {"(9) Show file", fun() -> example9:start() end,"example9.erl"},
     {"(10) Shell", fun() -> example10:start() end, "example10.erl"},
     {"(11) Visual Erlang", fun() -> example11:start() end, "example11.erl"},
     {"(12) Forms", fun() -> example12:start() end, "example12.erl"},
     {"(13) Draggable frames", fun() -> example13:start() end,"example13.erl"},
     {"(14) Draggable Objects", fun() -> example14:start() end,
      "example14.erl"},
     {"(t1) test exits", fun() -> test1:start() end, "test1.erl"},
     {"(16) Popup's ", fun() -> example16:start() end, "example16.erl"},
     {"(17) Animation ", fun() -> example17:start() end, "example17.erl"},
     {"More examples", 
      fun() -> spawn(fun() -> win(examples2()) end) end}
    ].

examples2() ->
    [{"Quit", fun() -> erlang:halt() end},
     {"(18) Color picker ", fun() -> example18:start() end, "example18.erl"},
     {"(19) Drag frame ", fun() -> example19:start() end, "example19.erl"},  
     {"(20) Editor framework", fun() -> example20:start() end, "example20.erl"},  
     {"(21) Editor", fun() -> example21:start() end, "example21.erl"},  
     {"(22) Clock", fun() -> example22:start() end, "example22.erl"},  
     {"(23) Canvas", fun() -> example23:start() end, "example23.erl"},
     {"(24) Top level graphics", fun() -> example24:start() end, 
      "example24.erl"},
     {"(25) Fancy frame", fun() -> example25:start() end, "example25.erl"},
     {"(26) Colored text", fun() -> example26:start() end, "example26.erl"},
     {"(27) Map", fun() -> example27:start() end, "emacs1.erl"},
     {"(28) ErlPoint", fun() -> example28:start() end, "example28.erl"},
     {"     FontSelector", fun() -> fontSelector:start() end, "fontSelector.erl"},
     {"(29) Lifts widget", fun() -> example29:start() end, "example29.erl"},
     {"     Lift simulation", fun() -> lifts:start() end, "lifts.erl"},
     {"(30) Color Buttons", fun() -> example30:start() end, "example30.erl"},
     {"     Old Emacs", fun() -> emacs:start() end, "emacs.erl"},
     {"(31) Emacs (current) ", fun() -> example31:start() end, "emacs31.erl"},
     {"(32) Emacs Lots + To do", fun() -> example32:start() end, "emacs32.erl"}
    ].

add_examples(Win, X, Y, Width, [{Txt, Fun, File}|T]) ->
    But  = swButton:make(Win, X, Y, Width, 30, ?yellow, Txt),
    But1 = swButton:make(Win,X+Width+10,Y,20,30,?red,""),
    But  ! {onClick, fun(_) -> Fun() end},
    But1 ! {onClick, fun(_) -> emacs:start(File) end},
    add_examples(Win, X, Y+35, Width, T);
add_examples(Win, X, Y, Width, [{Txt, Fun}|T]) ->
    But = swButton:make(Win, X, Y, Width, 30, ?yellow, Txt),
    But ! {onClick, fun(_) -> Fun() end},
    add_examples(Win, X, Y+35, Width, T);
add_examples(_Win, _X, Y, _Width, []) ->
    Y.

