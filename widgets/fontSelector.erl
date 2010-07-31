-module(fontSelector).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% started 2004-04-23 by joe@sics.se (Joe Armstrong)

%%3456789012345678901234567890123456789012345678901234567890123456789012345678
%%3456789012345678901234567890123456789012345678901234567890123456789012345

-export([start/0, start/1]).

-import(sw,       [xStart/1,rpc/2]).
-import(ex11_lib, [eListFonts/2, eSetInputFocus/3, xDo/2, xFlush/1,  
		   xColor/2, xCreateGC/2, xEnsureFont/2]).

-import(lists,    [duplicate/2,foldl/3,map/2,member/2,reverse/1, 
		   reverse/2, sort/1]).

-include("sw.hrl").

-define(bg, 16#ffffcc).

start() -> start("emacs.erl").

start(File) ->
    spawn(fun() -> win(File) end).
		  
win(File) ->
    Display = xStart("3.2"), 
    Width = 700, Ht = 600,
    Win       = swTopLevel:make(Display, Width, Ht, ?bg),  
    Win ! {onClick, fun(X) -> io:format("Pos=~p~n",[X]) end},
    Selector1 = swSelector:make(Win, 10, 10, 35, 20, 1,?white),
    Fonts = font_names(Display),
    Selector2 = swSelector:make(Win, 360, 10, 15, 10, 1, ?white),
    Selector3 = swSelector:make(Win, 550, 10, 15, 10, 1, ?white), 
    Text    = swErlPoint:make(Win, 10,365, Width-20, 200,1,?azure),
    show_text(Text, "*-urw-chancery l-*-*-*-*-80-*-*-*-*-*-*-*"),
    show_text(Text, "fixed"),
    show_text(Text, "-abiword-century schoolbook-bold-r-normal--42-300-75-100-p-0-iso8859-1"),
    Selector3 ! {display,["10","20","30","40","50","60","70","80","90"]},
    Selector1 ! {display, Fonts},
    Selector1 ! {onClick, [right], 
		 fun(Why, Str) -> 
			 menu1(Display, Selector1, Selector2, Selector3, Text, 
			       Str) end},
    loop().

menu1(Display, Selector1, Selector2, Selector3, Text, Str1) ->
    io:format("clicked=~p~n",[Str1]),
    Re = "-*-" ++ Str1 ++ "*-*-*-*-*-*-*-*-*-*-*-iso8859-1", 
    {ok, Fonts} = xDo(Display, eListFonts(100000, Re)),
    L1 = map(fun(I) ->
		     [_,Name,Type,Style|_] = string:tokens(I, "-"),
		     Type ++ "-" ++ Style
	     end, Fonts),
    Types = sort(remove_duplicates(L1, [])),
    Selector2 ! {display, Types},
    Selector1 ! {mode, passive},
    Selector2 ! {mode, active},
    Selector2 ! {onClick, [left,right],
		 fun(Why, Str2) ->
			 menu2(Why, Str1, Str2, Selector1, Selector2, 
			       Selector3, Text, Display) 
		 end}.

menu2(right, Str1, Str2, S1, S2, S3, Text, Display) ->
    io:format("Chosen:~p ~p~nj",[Str1, Str2]),
    S2 ! {mode, passive},
    S3 ! {mode, active},
    S3 ! {onClick, [left,right],
	  fun(Why, Str3) ->
		  menu3(Why, Str1, Str2, Str3, S1, S2, S3, Text, Display)
	  end};
menu2(left, Str1, Str2, S1, S2, S3, Text, Display) ->
    S2 ! {mode, passive},
    S1 ! {mode, active};
menu2(_, _, _, _, _, _, _, _) ->
    true.

menu3(left, _, _, _, S1, S2, S3, Text, _) ->
    S2 ! {mode, active},
    S3 ! {mode, passive};
menu3(right, Str1, Str2, Str3,_,_,_,Text, Display) ->
    io:format("Finally:~s ~s ~s~n",[Str1, Str2, Str3]),
    Re1 = "-*-" ++ Str1 ++ "-" ++ Str2 ++"-*-*-*-" ++ Str3 ++
	"0-*-*-*-*-iso8859-1",
    Re = "-*-a.d. mono-medium-r-*-*-*-300-*-*-*-*-*-*",
    io:format("Re =~p~n",[Re]),
    io:format("Re1=~p~n",[Re1]),
    {ok, Fonts} = xDo(Display, eListFonts(100000, Re1)),
    io:format("Fonts=~p~n",[Fonts]),
    case Fonts of
	[] ->
	    io:format("Nothing~n");
	_ ->
	    %% Just go and create a label here
	    show_text(Text, hd(Fonts))
    end.
    
show_text(Text, Face) ->
    io:format("calling show_text:~p ~p~n",[Text, Face]),
    D =  [{face,fixed, ?black, "fixed"},
	  {face,test,?black, Face},
	  {text,fixed, 10, 50,Face},
	  {text, test, 10, 100, "Hello Joe"}],
    Text ! {display, D}.

font_names(Display) ->
    Re = "-*-*-*-*-*-*-*-*-*-*-*-*-iso8859-1",
    {ok, Fonts} = xDo(Display, eListFonts(100000, Re)),
    L1 = map(fun(I) ->
		     [_,Name|_] = string:tokens(I, "-"),
		     Name
	     end, Fonts),
    sort(remove_duplicates(L1, [])).

remove_duplicates([], L) -> L;
remove_duplicates([H|T], L) ->
    case member(H, L) of
	true ->
	     remove_duplicates(T, L);
	false ->
	    remove_duplicates(T, [H|L])
    end.

loop() ->
    receive
	Any ->
	    io:format("top level received:~p~n",[Any]),
	    loop()
    end.



