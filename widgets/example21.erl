-module(example21).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% started 2004-02-28 by joe@sics.se (Joe Armstrong)
%% A basic canvas

%% started 2004-01-22 by joe@sics.se (Joe Armstrong)
%% This is a basic (empty) text editor

-export([start/0]).

-import(sw, [xStart/1, rpc/2]).

-include("sw.hrl").
-import(lists, [last/1, map/2, reverse/1]).

-define(bg, 16#ffffcc).

start() -> spawn(fun win/0).

win() ->
    Display = xStart("3.2"),
    swBlinker:ensure_blinker(Display),
    edit(Display, 80, 24, 
"This is a very simple editor.

Note that many
commands don't work at all.
Cursor movement up and down does not work
but left and right movements do work.
Adding and deleting characters does work
and the cursor appears to move the correct
position when you click on the mouse.

Rather than program the editor like this you should 
not mix the editor code in with the widget but use the
editor framework as in example20.erl.
").

-record(b,{width,ht,b1,b2,l1,flag,l2,a1,a2}).

%% b1 = line before screen  (reversed) = [{Flag,Str}]
%% a1 = lines after screen               [{Flag,Str}] 
%% b2 = lines before current line (on screen) normal order [{Flag,Str}]
%% a2 = lines after  current line (on screen) normal order [{Flag,Str}]
%% l1 = current line before cursor = Str
%% l2 = current line after  cursor = Str
%% flag = bool (current line ends with \n")

edit(Display, Wc, Hc, Str) ->
    Width = Wc * 9 + 20,
    Ht    = Hc * 18+20,
    Lines = split_into_lines(Str, Wc, []),
    %% io:format("Lines=~p~n",[Lines]),
    Win     = swTopLevel:make(Display, Width, Ht, ?bg),
    Text    = swEdText:make(Win, 10,10,Width-20,Ht-20,1,?white),
    Winfo = rpc(Text, winfo),
    %% io:format("Here:~p~n",[Winfo]),
    TextWin = Winfo#win.win,
    S = self(),
    Text ! {onClick, fun(X) -> S ! {click, X} end},
    Text ! {onKey, fun(X) -> 
			   Cmd = ex11_lib_keyboard_driver:analyse(X),
			   S ! {key, Cmd} 
		   end},
    B1 = [],
    {Screen, A1} = first(Ht, Lines),
    %% Put the cursor Before the first character
    B2 = [],
    {Line, A2} = first(1, Screen),
    %% io:format("Line=~p~n",[Line]),
    {Flag, Line1, Line2} = case Line of
			       [] -> {false, [], []};
			       [{F,Str1}] ->
				   {F, [], Str1}
			   end,
    Buff = #b{width=Wc,ht=Hc,b1=B1,b2=B2,flag=Flag,l1=Line1,
	      l2=Line2,a1=A1,a2=A2},
    refresh(Text, Buff),
    loop(Display, Text, TextWin, Buff).

refresh(Text, #b{b2=B2,l1=L1,l2=L2,a2=A2}) ->
    %% io:format("Refresh Text=~p B2=~p L1=~p L2=~p A2=~p~n",[Text,B2,L1,L2,A2]),
    StrsB = strings(B2),
    StrsA = strings(A2),
    CurrentLine = L1 ++ L2,
    Strs = StrsB ++ [CurrentLine|StrsA],
    Text ! {show, Strs}.

strings(S) -> map(fun({_,X}) -> X end, S).

%% first(N, X) -> {L1,L2} take first N elements from X (if possible)

first(N, X) -> first(N, X, []).

first(0, X, L)     -> {reverse(L), X};
first(N, [H|T], L) -> first(N-1, T, [H|L]);
first(_, [], L)    -> {reverse(L), []}.

loop(Display, Text, TextWin, Buff) ->
    debug(loop, Buff),
    receive
	{click, Pos} ->
	    Buff1 = set_cursor(Pos, Buff),  
	    show_cursor(Display, TextWin, Buff1),
	    loop(Display, Text,  TextWin, Buff1);
	{key,{_,_,_,{char,13}}} ->
	    Buff1 = insert_line(Buff),
	    update(Display, Text, TextWin, Buff1),
	    loop(Display, Text,  TextWin, Buff1);
	{key,{_,_,_,{char,8}}} ->
	    Buff1 = delete(Buff),
	    update(Display, Text, TextWin, Buff1),
	    loop(Display, Text,  TextWin, Buff1);
	{key,{_,_,_,{char,C}}} ->
	    %% io:format("Char=~p~n",[C]),
	    #b{l1=L1} = Buff,
	    %% io:format("L1=~s~n",[L1]),
	    Buff1 = Buff#b{l1=L1++[C]},
	    update(Display, Text, TextWin, Buff1),
	    loop(Display, Text,  TextWin, Buff1);
	{key,{_,_,_,{cmd,right}}} ->
	    Buff1 = right(Buff),
	    update(Display, Text, TextWin, Buff1),
	    loop(Display, Text,  TextWin, Buff1);
	{key,{_,_,_,{cmd,left}}} ->
	    Buff1 = left(Buff),
	    update(Display, Text, TextWin, Buff1),
	    loop(Display, Text,  TextWin, Buff1);
	Any ->
	    io:format("example21 received:~p~n",[Any]),
	    loop(Display, Text,  TextWin, Buff)
    end.

delete(B = #b{l1=L1=[_|_]}) ->
    {First,_} = split_tail(L1),
    B#b{l1=First};
delete(B) ->
    debug(delete, B),
    B.
    
insert_line(B = #b{l1=L1,l2=_L2,flag=_F,b2=B2}) ->
    Line1 = {true,L1},
    B#b{l1=[],b2=B2++[Line1]};
insert_line(B) ->
    debug(insert_line, B).

-ifdef(debug).
debug(Tag,B= #b{b1=B1,b2=B2,flag=F,l1=L1,l2=L2,a1=A1,a2=A2}) ->
    io:format("no match:~p~n ",[Tag]),
    io:format("b1=~p~nb2=~p~n(F,L1,L2)=(~p,~s,~s)~na2=~p~na1=~p~n",
	      [B1,B2,F,L1,L2,A2,A1]),
    B.
-else.
debug(_, B) ->
    B.
-endif.

right(B = #b{l1=L1,l2=[H|L2x]}) ->
    L1x = L1 ++ [H],
    B#b{l1=L1x,l2=L2x};
right(_B = #b{flag=F,l1=L1,l2=[],b2=B2,a2=[{Flag,T}|A2]}) ->
    Line = {F,L1},
    #b{l1=[],l2=T,flag=Flag,b2=B2++[Line],a2=A2};
right(B) -> B.

left(B = #b{l1=L1=[_|_],l2=L2}) ->
    L1x = first(L1),
    H = last(L1),
    B#b{l1=L1x,l2=[H|L2]};
left(B = #b{l1=[],l2=L2,flag=F,b2=B2,a2=A}) when B2 =/= []->
    %% move onto the previous line . no scroll
    {First,{Flag,Str}} = split_tail(B2),
    Line={F,L2},
    A2x=[Line|A],
    B#b{l1=Str,l2=[],flag=Flag,b2=First,a2=[Line|A]};
left(B) -> B.

split_tail(T) ->
    {first(T), last(T)}.


update(Display, Text, Win, Buff) ->
    refresh(Text, Buff),
    show_cursor(Display, Win, Buff),
    Buff.

show_cursor(Display, Win, #b{b2=B2,l1=L1}) ->
    Line1 = length(B2),
    Col1 = length(L1),
    XX = 9*Col1+11, YY = Line1*18+4,
    swBlinker:blink(Display, Win, XX, YY).

set_cursor({X, Y}, B0) ->
    #b{width=W,ht=H,b2=B2,a2=A2,flag=Flag,l1=L1,l2=L2} = B0,
    %% adjust the buffer according to the 
    %% mouse click
    %% io:format("---------------~nX=~p Y=~p W=~p~n",[X,Y,W]),
    Col  = nearest_character(Y,W),
    Line = nearest_line(X,H),
    %% io:format("Col=~p Line=~p~n",[Col,Line]),
    %% B1 and A1 do not change
    %% step 1) remake the buffer
    Lines = make_buffer(B2,Flag,L1,L2,A2),
    %% Then 2) split it correctly
    %% Line = 1 means we have clicked in the first line etc.
    {B2x, Tmp} = first(Line-1, Lines),
    %% The cursor should be somewhere in Tmp but Tmp might be empty 
    case Tmp of
	     [] ->
		 B0#b{b2=B2x,l1=[],l2=[],a2=[]};
	     [{F,Str}|T] ->
		 {L1x, L2x} = first(Col, Str),
		 B0#b{b2=B2x,l1=L1x,l2=L2x,flag=F,a2=T}
    end.

make_buffer(B2,false,[],[],[]) -> B2;
make_buffer(B2,Flag,L1,L2,A2) -> B2 ++ [{Flag,L1++L2}|A2].
    

nearest_character(N, Max) ->
    %% N = (X-10) div 9,
    if 
	N > Max -> Max;
	N < 0   -> 0;
	true -> N
    end.

nearest_line(N, Max) ->
    %%N = (Y+14) div 18,
    if 
	N > Max -> Max;
	N < 0 -> 1;
	true -> N
    end.

%% The origonal lines are split into 
%%  [{Flag,Str}] Flag = true if the line terminates with CR
%%  Str is <= Max

split_into_lines([], Max, L) ->
    reverse(L);
split_into_lines(Str, Max, L) ->
    {Next, Str1} = get_next(Str, Max, []),
    split_into_lines(Str1, Max, [Next|L]).
    
get_next(Str, 0, L) ->
    {{false,reverse(L)}, Str};
get_next([], _, L) ->
    {{false,reverse(L)}, []};
get_next([$\n|T], _, L) ->
    {{true,reverse(L)}, T};
get_next([H|T], N, L) ->
    get_next(T, N-1, [H|L]).

first([_])   -> [];
first([H|T]) -> [H|first(T)].

    


















