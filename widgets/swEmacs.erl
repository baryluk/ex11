-module(swEmacs).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% started 2004-01-22 by joe@sics.se (Joe Armstrong)
%% This is a basic pico text editor with emacs-like bindings

%%3456789012345678901234567890123456789012345678901234567890123456789012345678
%%3456789012345678901234567890123456789012345678901234567890123456789012345

%% Note the resize code is buggy - onReconfigure is commented out

-export([make/7]).

-import(sw, [xStart/1]).
-import(ex11_lib, [rpc/2]).
-import(lists, [duplicate/2,foldl/3,reverse/1, reverse/2]).

-import(lib_emacs, [click/3,
		    control/2,
		    delete_behind_cursor/1,
		    display/1,
		    insert_char/2,
		    move_down/1,
		    move_left/1,
		    move_right/1,move_up/1,next/1,prior/1,resize/3,
		    scroll/1, split/2]).

-include("sw.hrl").
-include("emacs.hrl").

-define(bg, 16#ffffcc).

make(Parent, X, Y, Width, Ht, Border, Color) -> 
    spawn_link(fun() -> init(Parent,  X, Y, Width, Ht, Border, Color) end).


init(Win,  X, Y, WidthChars, HtChars, Border, Color) ->
    {Width, Ht} = sw:sizeInCols2pixels(WidthChars, HtChars),
    Text    = swColorText:make(Win, X, Y, WidthChars,HtChars,1,?white),
    File = "emacs.txt",
    Lines = read_file(File),
    io:format("Read ~w lines from ~s~n",[lines:count(Lines), File]),
    S = self(),
    Text ! {onClick, fun(I) -> S ! {click, I} end},
    Text ! {onKey,   fun(I) -> S ! {key, I}  end},
    %% Win ! {onReconfigure, fun({Width,Ht}) -> S ! {resized, Width, Ht} end},
    State = #e{text=Text, width=WidthChars,ht=HtChars,start=1,current=1,
	       col=1,data=Lines, mode=emacs},
    Text ! {newPen, normal, ?black, ?white},
    display(State),
    io:format("Ht=~p~n",[HtChars]),
    loop(State).

read_file(F) ->
    {ok, Bin} = file:read_file(F),
    add_lines(binary_to_list(Bin), lines:new()).

add_lines([], L) -> L;
add_lines(Str, L) ->
    {Line, Next} = get_line(Str, []),
    add_lines(Next, lines:append(detab(Line), L)).

detab(L) -> detab(L, 1, []).

detab([$\t|T], N, L) when N rem 8 == 0 -> detab(T, N+1, [$ |L]);
detab([$\t|T], N, L)                   -> detab([$\t|T], N+1, [$ |L]);
detab([H|T], N, L)                     -> detab(T, N+1, [H|L]);
detab([], _, L)                        -> reverse(L).

get_line([$\r,$\n|T], L) -> {reverse(L), T};
get_line([$\r|T], L)     -> {reverse(L), T};
get_line([$\n|T], L)     -> {reverse(L), T};
get_line([H|T], L)       -> get_line(T, [H|L]);
get_line([], L)          -> {reverse(L), []}.

loop(State) ->
    receive
	{edit, File} ->
	    Lines = read_file(File),
	    State1 = State#e{start=1,current=1,col=1,data=Lines},
	    Text = State1#e.text,
	    Text ! {clearPage, normal},
	    display(State1),
	    loop(State1);
	{click, {X, Y}} ->
	    io:format("Clicked:X=~p Y=~p~n", [X,Y]),
	    State1 = click(X, Y, State),
	    display(State1),
	    loop(State1);
	{key, Args} -> 
	    Cmd = ex11_lib_keyboard_driver:analyse(Args),
	    io:format("Pressed:~p=>~p~n",[Args,Cmd]),
	    State1 = key_pressed(Cmd, State),
	    State2 = scroll(State1),
	    display(State2),
	    loop(State2);
	{resized, Width, Ht} ->
	    State1 = resize(Width, Ht, State),
	    display(State1),
	    loop(State1);
	Any ->
	    io:format("example20 received:~p~n",[Any]),
	    loop(State)
    end.

key_pressed({_,_, Mod, Cmd}, State) ->
    handle_key(Mod, Cmd, State).

handle_key(none, {cmd, right}, State) -> move_right(State);
handle_key(none, {cmd, left}, State)  -> move_left(State);
handle_key(none, {cmd, up}, State)    -> move_up(State);
handle_key(none, {cmd, down}, State)  -> move_down(State);
handle_key(none, {cmd, next}, State)  -> next(State);
handle_key(none, {cmd, prior}, State) -> prior(State);
handle_key(ctrl, {char, X}, State)    -> control(X, State);

handle_key(none, {char, 8}, State) ->
    delete_behind_cursor(State);
handle_key(none, {char, 13}, #e{current=Line,col=Col,data=Lines} = State) ->
    %% New line 
    Str = lines:nth(Line, Lines),
    {Before, After} = split(Col-1, Str),
    Lines1 = lines:replace(Line, Lines, Before),
    Lines2 = lines:replace(Line+1, Lines1, After),
    Col1 = 1,
    Current1 = Line + 1,
    State#e{col=Col1,current=Current1, data=Lines2};
handle_key(none, {char, X}, State) ->
    insert_char(X, State);
handle_key(shift, {char, X}, State) ->
    insert_char(X, State);
handle_key(Mode, Other, State) ->
    io:format("Cannot handle:~p ~p ~n",[Mode, Other]),
    State.


