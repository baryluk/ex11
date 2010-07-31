-module(lib_emacs).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([click/3,
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
	 scroll/1,
	 split/2, 
	 lines_nth/3]).

-import(lists, [duplicate/2, foldl/3, reverse/1]).

-include("emacs.hrl").

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
    Len = length(lines_nth(?LINE, Line, Lines)),
    State#e{col=Len+1};
control(X, State) ->
    State.

erase_to_eol(#e{current=Line, kill=K, col=Col, data=Lines}=State) ->
    Str = lines_nth(?LINE, Line, Lines),
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
		    Str1 = lines_nth(?LINE, Line1, Lines),
		    Str2 = Str ++ Str1,
		    Lines1 = lines:replace(Line, Lines, Str2),
		    Lines2 = lines:delete(Line1, Lines1),
		    State#e{data=Lines2};
		true ->
		    State
	    end
    end.

insert_char(X, #e{current=Line,col=Col,data=Lines} = State) ->
    Str = lines_nth(?LINE, Line, Lines),
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
		    Str = lines_nth(?LINE, Line-1, Lines),
		    Col1 = length(Str) + 1,
		    State#e{current=Line-1,col=Col1};
		true ->
		    State
	    end
    end.

move_right(#e{start=S,current=Line,col=Col,data=Lines} = State) ->
    Str = lines_nth(?LINE, Line, Lines),
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
		    StrPrev = lines_nth(?LINE, Line-1, Lines),
		    Str = lines_nth(?LINE, Line, Lines),
		    Str1 = StrPrev++Str,
		    Lines1 = lines:replace(Line, Lines, Str1),
		    Lines2 = lines:delete(Line-1, Lines1),
		    Col1 = length(StrPrev) + 1,
		    State#e{current=Line-1,col=Col1,data=Lines2};
		true ->
		    State
	    end;
	Col > 0 ->
	    Str = lines_nth(?LINE, Line, Lines),
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

display(#e{text=Text,width=W,ht=H,mode=Mode, start=Start,current=Current,
	   col=Col,data=Lines}) ->
    %% We have to send the widget a list of lines to display
    %% io:format("split width=~p~n",[W]),
    Strs = extract_lines(Start, H, W, Lines, []), 
    %% io:format("Strs=~p~n",[Strs]),
    %% io:format("Len = ~p~n",[length(Strs)]),
    {X,Y} = where_is_cursor(Current, Col, Start, Lines, W), 
    %% io:format("Where is cursor returns X=~p Y=~p~n",[X,Y]),
    foldl(fun(Str,I) ->
		  Color = case I of
			      Y -> 
				  case Mode of
				      active -> blue;
				      passive -> gray;
				      emacs -> normal
				  end;
			      _ ->
				  normal
			  end,
		  %% io:format("swSelector ! text :~p~n",[{1,I,Color,Str}]),
		  Text ! {display, 1, I, Color, Str},
		  Len = length(Str),
		  Delete = W - Len,
		  if Delete > 0 ->
			  Del = duplicate(Delete, $\s),
			  %% io:format("swSelector ! text :~p~n",
			  %% [{Len+1,I,Color,Del}]),
			  Text ! {display, Len+1, I, Color, Del};
		     true -> void
		  end,
		  I+1
	  end, 1, Strs),
    if Mode == emacs ->
	    Text ! {blink, X, Y};
       true ->
	    void
    end.


%% extract_lines(Start, N, Width, Lines)
%%   Extract N lines starting at Start

extract_lines(Start, N, Width, Lines) -> 
    extract_lines(Start, N, Width, Lines, []).

extract_lines(Ln, 0, Width, Lines, Strs) ->
    reverse(Strs);
extract_lines(Ln, N, Width, Lines, Strs) ->
    Max = lines:count(Lines),
    if 
        Ln > Max ->
            reverse(Strs);
        true ->
            Str = lines_nth(?LINE, Ln, Lines),
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
        locate_line(Ln, length(lines_nth(?LINE, Ln, Lines)), 
                    0, W, Lines, 1, X, Y), 
    io:format("Current=~p Col=~p XX=~p YY=~p~n",[Current,Col,XX,YY]),
    Text ! {blink, XX, YY},
    S1 = S#e{current=Current,col=Col},
    case where_is_cursor(S1) of
        {XX, YY} ->
            io:format("ok remove later~n"),
            ok;
        _ ->
            io:format("**** Cusror computation buggered up"
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
                    Len1 = length(lines_nth(?LINE, Ln1, Lines)),
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
    io:format("where is Current=~p Col=~p W=~p~n",[Current,Col,W]),
    Len = length(lines_nth(?LINE, Start, Lines)),
    where_is_cursor(Current, Col, Start, Lines, W, Len, 0).

where_is_cursor(Current, Col, Current, Lines, W, Len, Y) ->
    {Dx, Dy} = vcol2xy(Col, W),
    Str = lines_nth(?LINE, Current, Lines),
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
            Len1 =  length(lines_nth(?LINE, Start1, Lines)),
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
    io:format("RRRRRRRRRRRRRR~nResizing to: Width=~p Ht=~p",[Width,Ht]),
    Pid ! {setWidthHt, W1, H1},
    W2 = W1 div 9,
    H2 = H1 div 18,
    State#e{width=W2, ht=H2}.

split(N, X) -> split(N, X, []).

split(0, X, L)     -> {reverse(L), X};
split(N, [H|T], L) -> split(N-1, T, [H|L]);
split(N, [], L)    -> {reverse(L), []}.

%%------------------------------------------------------------------------

lines_nth(Line, N, Lines) ->
    Max = lines:count(Lines),
    if 
	N > Max ->
	    io:format("*** This is not good lib_emacs:extract_lines~n"
		      "Line=~p Max = ~p N=~p~nData=~p~n",
		      [Line, Max, N,lines:convert_to_list(Lines)]);
	true ->
	    void
    end,
    %% io:format("~p~n",[catch lines:nth(N, Lines)]),
    lines:nth(N, Lines).


    
