-module(swSelector).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% started 2004-04-23 by joe@sics.se (Joe Armstrong)
%% This is a selector widgets it works very much like emacs

%%3456789012345678901234567890123456789012345678901234567890123456789012345678
%%3456789012345678901234567890123456789012345678901234567890123456789012345

-export([make/7]).

-import(sw, [xStart/1]).
-import(ex11_lib, [eListFonts/2, xDo/2, rpc/2]).
-import(lists, [duplicate/2,foldl/3,map/2,member/2,reverse/1, 
		reverse/2, sort/1]).

-import(lib_emacs, [click/3,
		    control/2,
		    delete_behind_cursor/1,
		    display/1,
		    insert_char/2,
		    move_down/1,
		    move_left/1,
		    move_right/1,
		    move_up/1,
		    next/1,
		    prior/1,
		    resize/3,
		    scroll/1]).


-include("sw.hrl").

-define(bg, 16#ffffcc).

-include("emacs.hrl").

make(Parent, X, Y, Width, Ht, Border, Color) -> 
    spawn_link(fun() -> init(Parent,  X, Y, Width, Ht, Border, Color) end).


init(Win,  X, Y, WidthChars, HtChars, Border, Color) ->
    {Width, Ht} = sw:sizeInCols2pixels(WidthChars, HtChars),
    io:format("Win=~p XYRowcCols:~p~n",[Win,{X,Y,WidthChars,HtChars}]),
    Text        = swColorText:make(Win,X,Y,WidthChars,HtChars,Border,Color),
    S = self(),
    Text ! {onClick, fun(X1) -> S ! {click, X1} end},
    Text ! {onKey, fun(X1)   -> S ! {key, X1} end},
%%  Win ! {onReconfigure, fun({Width,Ht}) -> S ! {resized, Width, Ht} end},
    Lines = mk_menu(["",""]),
    State = #e{text=Text, width=WidthChars,exit=[],mode=passive,ht=HtChars,
	       start=1,current=1,col=1, data=Lines},
    Text ! {newPen, normal, ?black, ?white},
    Text ! {newPen, blue, ?white, ?blue},
    Text ! {newPen, gray, ?black, ?cyan},
    Freturn = fun(_, _) -> void end,
    display(State),
    loop(State, Freturn).

loop(State, Freturn) ->
    #e{current=C1x,data=L1x} = State,
    Max = lines:count(L1x),
    if 
	C1x > Max ->
	    io:format("*** This is not good ~n"
		      "Current = ~p Max = ~p ~nData=~p~n",
		      [C1x,Max,lines:convert_to_list(L1x)]),
	    exit(oops);
	true ->
	    void
    end,


    receive
	{onClick, E, F} ->
	    State1 = State#e{exit=E},
	    loop(State1, F);
	{display, Strings} ->
	    Lines  = mk_menu(Strings),
	    State1 = State#e{data=Lines, current=1, mode=passive},
	    Text = State#e.text,
	    Text ! {clearPage, normal},
	    display(State1),
	    loop(State1, Freturn);
	{click, {X, Y}} ->
	    io:format("Clicked:X=~p Y=~p~n", [X,Y]),
	    State1 = click(X, Y, State),
	    State2 = State1#e{mode=active},
	    display(State2),
	    loop(State2, Freturn);
	{key, Args} -> 
	    Cmd = ex11_lib_keyboard_driver:analyse(Args),
	    io:format("Pressed:~p=>~p~n",[Args,Cmd]),
	    case is_exit(Cmd, State) of
		{yes, Name} ->
		    #e{current=C, data=Lines} = State,
		    Str = lib_emacs:lines_nth(here123, C, Lines),
		    Freturn(Name, Str),
		    State1 = State#e{mode=passive},
		    display(State1),
		    loop(State1, Freturn);
		no ->
		    State1 = key_pressed(Cmd, State),
		    State2 = scroll(State1),
		    display(State2),
		    loop(State2, Freturn)
	    end;
	{resized, Width, Ht} ->
	    State1 = resize(Width, Ht, State),
	    display(State1),
	    loop(State1, Freturn);
	{mode, active} ->
	    Text = State#e.text,
	    Text ! setInputFocus,
	    State1 = State#e{mode=active},
	    display(State1),
	    loop(State1, Freturn);
	{mode, passive} ->
	    Pid = State#e.text,
	    Win = rpc(Pid, mountPoint),
	    State1 = State#e{mode=passive},
	    display(State1),
	    loop(State1, Freturn);
	Any ->
	    io:format("example20 received:~p~n",[Any]),
	    loop(State, Freturn)
    end.

is_exit(Key, State) -> 
    Vals = State#e.exit,
    io:format("Vals=~p~n",[Vals]),
    is_exit1(Key, Vals).

is_exit1({_,_,_,{cmd,left}}, Vals)  -> mmember(left, Vals);
is_exit1({_,_,_,{cmd,right}}, Vals) -> mmember(right, Vals);
is_exit1({_,_,_,{char,13}}, Vals)   -> mmember(ret, Vals);
is_exit1(_,_) -> no.

mmember(X, L) ->
    case member(X, L) of
	true -> {yes, X};
	false -> no
    end.
	    
mk_menu(Fonts) ->
    foldl(fun(F, L) -> lines:append(F, L) end, lines:new(), Fonts).
		  
key_pressed({_,_, Mod, Cmd}, State) -> handle_key(Mod, Cmd, State).

handle_key(none, {cmd, right}, State) -> State;
handle_key(none, {cmd, left}, State)  -> State;
handle_key(none, {cmd, up}, State)    -> move_up(State);
handle_key(none, {cmd, down}, State)  -> move_down(State);
handle_key(none, {cmd, next}, State)  -> next(State);
handle_key(none, {cmd, prior}, State) -> prior(State);
handle_key(Mode, Other, State) ->
    io:format("Cannot handle:~p ~p ~n",[Mode, Other]),
    State.

