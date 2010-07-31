-module(emacs).

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

-export([start/0, start/1]).

-import(sw, [xStart/1]).
-import(ex11_lib, [rpc/2]).
-import(lists, [foldl/3,reverse/1, reverse/2]).

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
    Win     = swTopLevel:make(Display, 716, 390, ?bg),
    Text    = swEdText:make(Win, 10,10,696, 370,1,?white),
    Lines = read_file(File),
    io:format("Read ~w lines from ~s~n",[lines:count(Lines), File]),
    S = self(),
    Text ! {onClick, fun(X) -> S ! {click, X} end},
    Text ! {onKey, 
	    fun(X) -> 
		    S ! {key, X}
	    end},
    Win ! {onReconfigure,
	   fun({Width,Ht}) ->
		   S ! {resized, Width, Ht}
	   end},
    {size, Width, Ht} = rpc(Text, size),
    io:format("Width=~p Ht=~p~n",[Width,Ht]),
    State = #e{text=Text,
	       width=Width,ht=Ht,start=1,current=1,col=1,data=Lines},
    display(State),
    loop(State).

read_file(F) ->
    {ok, Bin} = file:read_file(F),
    add_lines(binary_to_list(Bin), lines:new()).

add_lines([], L) -> L;
add_lines(Str, L) ->
    {Line, Next} = get_line(Str, []),
    add_lines(Next, lines:append(detab(Line), L)).

detab(L) -> detab(L, 1, []).

detab([$\t|T], N, L) when N rem 8 == 0 ->
    detab(T, N+1, [$ |L]);
detab([$\t|T], N, L) ->
    detab([$\t|T], N+1, [$ |L]);
detab([H|T], N, L) ->
    detab(T, N+1, [H|L]);
detab([], _, L) ->
    reverse(L).

get_line([$\r,$\n|T], L) -> {reverse(L), T};
get_line([$\r|T], L)     -> {reverse(L), T};
get_line([$\n|T], L)     -> {reverse(L), T};
get_line([H|T], L)       -> get_line(T, [H|L]);
get_line([], L)          -> {reverse(L), []}.

loop(State) ->
    receive
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

handle_key(none, {cmd, right}, State) ->
    move_right(State);
handle_key(none, {cmd, left}, State) ->
    move_left(State);
handle_key(none, {cmd, up}, State) ->
    move_up(State);
handle_key(none, {cmd, down}, State) ->
    move_down(State);
handle_key(none, {cmd, next}, State) ->
    next(State);
handle_key(none, {cmd, prior}, State) ->
    prior(State);
		    
handle_key(ctrl, {char, X}, State) ->
    control(X, State);

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

split(N, X) -> split(N, X, []).

split(0, X, L)     -> {reverse(L), X};
split(N, [H|T], L) -> split(N-1, T, [H|L]);
split(N, [], L)    -> {reverse(L), []}.

scroll(#e{current=C,start=Start} = State) when C < Start ->
    State#e{start=C};
scroll(#e{current=C,start=Start,ht=H} = State) -> 
    {X,Y} = where_is_cursor(State),
    if Y > H ->
	    State#e{start=Start+1};
       true ->
	    State
    end.

next(State) -> next(8, State).

next(0, State) -> State;
next(N, State) -> next(N-1, scroll(move_down(State))).

prior(State) -> prior(8, State).

prior(0, State) -> State;
prior(N, State) -> prior(N-1, scroll(move_up(State))).

control($a, State) ->
    State#e{col=1};
control($b, State) ->
    move_left(State);
control($f, State) ->
    move_right(State);
control($k, State) ->
    erase_to_eol(State);
control($n, State) ->
    move_down(State);
control($y, State) ->
    yank(State);
control($p, State) ->
    move_up(State);
control($d, State) ->
    State1 = scroll(move_right(State)),
    delete_behind_cursor(State1);
control($e, #e{current=Line, data=Lines}=State) ->
    Len = length(lines:nth(Line, Lines)),
    State#e{col=Len+1};
control(X, State) ->
    State.

erase_to_eol(#e{current=Line, kill=K, col=Col, data=Lines}=State) ->
    Str = lines:nth(Line, Lines),
    Len = length(Str),
    if 
	Col < Len ->
	    {Before, After} = split(Col-1, Str),
	    Lines1 = lines:replace(Line, Lines, Before),
	    State#e{data=Lines1, kill=[After|K]};
	true ->
	    Max = lines:count(Lines),
	    if
		Line < Max ->
		    Line1 = Line + 1,
		    Str1 = lines:nth(Line1, Lines),
		    Str2 = Str ++ Str1,
		    Lines1 = lines:replace(Line, Lines, Str2),
		    Lines2 = lines:delete(Line1, Lines1),
		    State#e{data=Lines2};
		true ->
		    State
	    end
    end.

insert_char(X, #e{current=Line,col=Col,data=Lines} = State) ->
    Str = lines:nth(Line, Lines),
    {Before,After} = split(Col-1, Str),
    Str1 = Before ++ [X|After],
    io:format("Str1=|~s|~n",[Str1]),
    Lines1 = lines:replace(Line, Lines, Str1),
    Col1 = Col + 1,
    State#e{col=Col1,data=Lines1}.

move_up(#e{current=Line} = State) ->
    if 
	Line > 1 ->
	    State#e{current=Line-1};
	true ->
	    State
    end.

move_down(#e{current=Line, data=Lines} = State) ->
    Max = lines:count(Lines),
    if 
	Line < Max ->
	    State#e{current=Line+1};
	true ->
	    State
    end.


move_left(#e{start=S,current=Line,col=Col,data=Lines} = State) ->
    {XX,_} = where_is_cursor(State),
    if 
	XX > 1 ->
	    State#e{col=XX-1};
	XX == 1 ->
	    if
		Line > 1 ->
		    Str = lines:nth(Line-1, Lines),
		    Col1 = length(Str) + 1,
		    State#e{current=Line-1,col=Col1};
		true ->
		    State
	    end
    end.

move_right(#e{start=S,current=Line,col=Col,data=Lines} = State) ->
    Str = lines:nth(Line, Lines),
    Len = length(Str),
    State1 = if 
		 Col > Len ->
		     %% move to the next line if there is one
		     Max = lines:count(Lines),
		     if 
			 Line < Max ->
			     Line1 = Line + 1,
			     State#e{current=Line1, col=1};
			 true ->
			     State
		     end;
		 true ->
		     Col1 = Col + 1,
		     State#e{col=Col1}
	     end,
    State1.

delete_behind_cursor(#e{current=Line,col=Col,data=Lines} = State) ->
    %% delete the character behind the cursor
    if
	Col == 1 ->
	    if
		Line > 1 ->
		    StrPrev = lines:nth(Line-1, Lines),
		    Str = lines:nth(Line, Lines),
		    Str1 = StrPrev++Str,
		    Lines1 = lines:replace(Line, Lines, Str1),
		    Lines2 = lines:delete(Line-1, Lines1),
		    Col1 = length(StrPrev) + 1,
		    State#e{current=Line-1,col=Col1,data=Lines2};
		true ->
		    State
	    end;
	Col > 0 ->
	    Str = lines:nth(Line, Lines),
	    {Before,After} = split(Col-2, Str),
	    Str1 = Before ++ tl(After),
	    io:format("Str1=|~s|~n",[Str1]),
	    Lines1 = lines:replace(Line, Lines, Str1),
	    Col1 = Col - 1,
	    State#e{col=Col1, data=Lines1}
    end.

yank(#e{current=Line,kill=K,data=Lines}=State) ->
    Lines1 = foldl(fun(Str, Lns) ->
			   lines:insert_after(Line, Lns, Str)
		   end, Lines, K),
    State#e{data=Lines1,kill=[]}.
    

%%----------------------------------------------------------------------
%% display(State)
%%    sets up the display correctly

display(#e{text=Text,width=W,ht=H,start=Start,current=Current,
	   col=Col,data=Lines}) ->
    %% We have to send the widget a list of lines to display
    Strs = extract_lines(Start, H, W, Lines, []), 
    %% io:format("Strs=~p~n",[Strs]),
    %% io:format("Len = ~p~n",[length(Strs)]),
    Text ! {show, Strs},
    {X,Y} = where_is_cursor(Current, Col, Start, Lines, W), 
    io:format("Where is cursor returns X=~p Y=~p~n",[X,Y]),
    Text ! {blink, X, Y}.

extract_lines(Ln, 0, Width, Lines, Strs) ->
    reverse(Strs);
extract_lines(Ln, N, Width, Lines, Strs) ->
    Max = lines:count(Lines),
    if 
        Ln > Max ->
            reverse(Strs);
        true ->
            Str = lines:nth(Ln, Lines),
            %% io:format("Line ~w = |~s|~n",[Ln,Str]),
            Ss = split_line(Str, Width),
            {Strs1, N1} = add_lines(Ss, N, Strs),
            extract_lines(Ln+1, N1, Width, Lines, Strs1)
    end.

split_line([], _)      -> [[]];
split_line(Str, Width) -> split1(Str, Width).

split1([], _) -> [];
split1(Str, Width) ->
    {Line, Left} = take_chars(Str, Width, []),
    [Line|split1(Left, Width)].

take_chars(Str, 0, L)   -> {reverse(L), Str};
take_chars([H|T], N, L) -> take_chars(T, N-1, [H|L]);
take_chars([], N, L)    -> {reverse(L), []}.

add_lines(_, 0, Strs)     -> {Strs, 0};
add_lines([H|T], N, Strs) -> add_lines(T, N-1, [H|Strs]);
add_lines([], N, Strs)    -> {Strs, N}.

%%---- end display

click(X, Y, #e{text=Text,start=Ln,data=Lines,width=W}=S) ->
    {Current, Col, XX, YY} = 
        locate_line(Ln, length(lines:nth(Ln, Lines)), 
                    0, W, Lines, 1, X, Y), 
    io:format("Current=~p Col=~p XX=~p YY=~p~n",[Current,Col,XX,YY]),
    Text ! {blink, XX, YY},
    S1 = S#e{current=Current,col=Col},
    case where_is_cursor(S1) of
        {XX, YY} ->
            io:format("ok remove later~n"),
            ok;
        _ ->
            io:format("**** Curor computation buggered up"
                      "X=~p Y=~p Col=~p XX=~p YY=~p~n",
                      [X, Y, Col, XX, YY])
    end,
    S1.

%% locate_line(State, X, Y) ->
%%    {Line, Vcol, XX, YY}
%%       Line = The line number in lines at the start of
%%              the line containing the cursor
%%       Vcol = The virtual col number in this line
%%       XX   = the X position where the cursor should really go
%%       YY   = the Y position where the cursor should really go

%% Invarient Line Ln starts at line N on the screen

locate_line(Ln, Len, Extra, W, Lines, N, X, N) ->
    if 
        X > Len+1 ->
            {Ln, Len+Extra*(W+1)+1, Len+1, N};
        true ->
            {Ln, X+Extra*(W+1), X, N}
    end;
locate_line(Ln, Len, Extra, W, Lines, N, X, Y) when Y > N ->
    if
        Len >= W ->
            locate_line(Ln, Len - W, Extra+1, W, Lines, N+1, X, Y);
        Len =< W ->
            Ln1 = Ln + 1,
            Size = lines:count(Lines),
            if
                Ln1 > Size ->
                    %% run out of lines to read
                    {Ln, Len+Extra*W, Len, N};
                true ->
                    Len1 = length(lines:nth(Ln1, Lines)),
                    locate_line(Ln1, Len1, 0, W, Lines, N+1, X, Y)
            end
    end.

%% Numbering of the bars
%%  This illustrates the numbering of a string "abcdef" 
%%  written into a line of width 4 - the last two characters spill 
%%  onto the second line
%%
%%  +---+---+---+---+
%%  | a | b | c | d |   <-- Line 1
%%  +---+---+---+---+
%%  1   2   3   4   5       Indices of the bars (numbered 1 to 5)
%%  +---+---+---+---+
%%  | e | f |           <-- Line 2
%%  +---+---+
%%  6   7   8

xy2vcol(X, Y, Width) ->
    (Y-1)*(Width+1) + X.

vcol2xy(Vcol, Width) ->
    Y = (Vcol-1) div (Width + 1),
    X = Vcol - Y * (Width + 1),
    {X, Y+1}.

display_width(Len, Width) ->
    {_,Y} = vcol2xy(Len, Width),
    Y.

where_is_cursor(#e{current=Current, col=Col, start=Start,
                   data=Lines, width=W}) ->
    where_is_cursor(Current, Col, Start, Lines, W).

%% {X,Y} = where_is_cursor(Current, Col, Start, Lines, W)


where_is_cursor(Current, Col, Start, Lines, W) ->
    io:format("where is Current=~p Col=~p~n",[Current,Col]),
    Len = length(lines:nth(Start, Lines)),
    where_is_cursor(Current, Col, Start, Lines, W, Len, 0).

where_is_cursor(Current, Col, Current, Lines, W, Len, Y) ->
    {Dx, Dy} = vcol2xy(Col, W),
    Str = lines:nth(Current, Lines),
    Len = length(Str),
    XX = if Dx > Len ->
                 Len + 1;
            true ->
                 Dx
         end,
    {XX, Dy+Y};
where_is_cursor(Current, Col, Start, Lines, W, Len, Y) ->
    Dy = display_width(Len, W),
    Y1 = Y + Dy,
    Start1 = Start + 1,
    Max = lines:count(Lines),
    if
        Start1 =< Max ->
            Len1 =  length(lines:nth(Start1, Lines)),
            where_is_cursor(Current, Col, Start1, Lines, W, Len1, Y1);
        true ->
            %% run out of lines
	    {Dx, _} = vcol2xy(Col, W),
	    {Dx, Y1}
    end.

%%----------------------------------------------------------------------

resize(Width, Ht, #e{text=Pid}=State) ->
    W1 = Width-20,
    H1 = Ht - 20,
    io:format("Resizing to: Width=~p Ht=~p",[Width,Ht]),
    Pid ! {setWidthHt, W1, H1},
    W2 = W1 div 9,
    H2 = H1 div 18,
    State#e{width=W2, ht=H2}.

