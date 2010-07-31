-module(ex11_lib_utils).
-author('tnt@home.se').
%%%---------------------------------------------------------------------
%%% Created : 15 Feb 1999 by tnt@home.se
%%% Function: Misc. utility routines.
%%% ====================================================================
%%% The contents of this file are subject to the Erlang Public License
%%% License, Version 1.0, (the "License"); you may not use this file
%%% except in compliance with the License. You may obtain a copy of the
%%% License at http://www.eddieware.org/EPL
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is ex11-0-1
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1999, Ericsson
%%% Telecom AB. All Rights Reserved.
%%% Origonal Author:
%%    Unknown - probably Torbjörn Törnvkist or Tony Rogvall
%%% Contributor(s):
%%%   Joe Armstrong   <joe@sics.se>
%%%   Frej Drejhammar <frej@stacken.kth.se> (bugfix in mk_colorfun).
%%%
%%%---------------------------------------------------------------------
-export([all/2,first/2,i16/1,i16/2,i32/1,i32/4,int2hex/1,split_list/2,
	 b16/2,b32/2,xalloc_id/1, mk_colorfun/1, 
	 mk_colorfun/2, mk_rgb/0]).

-export([xalloc_id1/1]).
-import(dict, [fetch/2, store/3]).
-import(lists,[reverse/1]).


-include("ex11_lib.hrl").

%% ---------------
%% Misc. routines

%% Que...? See XlibInt.c l.1492

%% xalloc_id(Display) -> {Id, Display'}

xalloc_id1(D0) ->
    ResId = fetch(resource_id, D0),
    ResShift = fetch(resource_shift, D0),
    ResMask = fetch(resource_mask, D0),
    %% io:format("ID:~p Shift=~p Mask=~p Base=~p~n",
    %% [ResId, ResShift, ResMask, fetch(Ets, resource_base)]),
    Id       = ResId bsl ResShift,
    D1 = if 
	     Id >= ResMask -> 
		 store(resource_mask,ResMask+1,D0);
	     true -> 
		 D0
	 end,
    if Id =< ResMask -> 
	    D2 = store(resource_id, ResId+1, D1),
	    Id1 = fetch(resource_base, D2) + Id,
	    {Id1, D2};
       true ->
	    if (Id =/= 16#10000000) ->
		    io:format("Xlib: ID allocation space exhausted!\n"),
		    D2 = store(resource_id, 16#10000000 bsr ResShift, D1),
		    {16#10000000, D2};
	       true -> 
		    {Id, D1}
	    end
    end.

xalloc_id(Dpy) when ?IS_DISPLAY(Dpy) ->    
    %% take these three variables from display
    ResId    = Dpy#display.resource_id,
    ResShift = Dpy#display.resource_shift,
    ResMask  = Dpy#display.resource_mask,
    io:format("ID:~p Shift=~p Mask=~p Base=~p~n",
	      [ResId, ResShift, ResMask, Dpy#display.resource_base]),
    Id       = ResId bsl ResShift,
    Dpy1 = if (Id >= ResMask) -> 
		   Dpy#display{resource_mask=ResMask+1};
	      true -> 
		   Dpy
	   end,
    if (Id =< ResMask) -> 
	    {Dpy1#display.resource_base+Id,
	     Dpy1#display{resource_id=ResId+1}};
       true ->
	    if (Id =/= 16#10000000) ->
		    io:format("Xlib: ID allocation space exhausted!\n"),
		    {16#10000000,
		     Dpy#display{resource_id = 16#10000000 bsr ResShift}};
	       true -> {Id,Dpy1}
	    end
    end.
	

%% ---------------------------------------------------------
%% Fetch the first element in list which satisfy F(Element),
%% where F(Element) => {true,Value] | false

first(F,[H|T]) ->
    case F(H) of
	{true,Value} -> {true,Value};
	false -> first(F,T)
    end;
first(_,[]) -> false.
	

%% ---------------------------------------------------------
%% Fetch the all elements in list which satisfy F(Element),
%% where F(Element) => {true,Value] | false

all(F,[H|T]) ->
    case F(H) of
	{true,Value} -> [Value|all(F, T)];
	false -> all(F,T)
    end;
all(_,[]) -> [].
	
%% ----------------------------------
%% Encode/Decode 16/32 bits integers

i16(Int) when is_binary(Int) ->
    i16(binary_to_list(Int));
i16(Int)  when is_integer(Int) -> 
    [(Int bsr  8) band 255,Int band 255];
i16([X1,X0]) ->
    i16(X1,X0).

i16(X1,X0) ->
    (X1 bsl 8) bor X0.

i32(Int) when is_binary(Int) ->
    i32(binary_to_list(Int));
i32(Int)  when is_integer(Int) -> 
    [(Int bsr 24) band 255,
     (Int bsr 16) band 255,
     (Int bsr  8) band 255,
     Int band 255];
i32([X1,X2,X3,X4]) ->
    i32(X1,X2,X3,X4).

i32(X1,X2,X3,X4) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

%% Deal with the byte order
    
b16(?LSB_BYTEORDER,I) when is_integer(I) -> reverse(i16(I));
b16(?LSB_BYTEORDER,L) when is_list(L)    -> i16(reverse(L));
b16(?MSB_BYTEORDER,I)                 -> i16(I).

b32(?LSB_BYTEORDER,I) when is_integer(I) -> reverse(i32(I));
b32(?LSB_BYTEORDER,L) when is_list(L)    -> i32(reverse(L));
b32(?MSB_BYTEORDER,I)                 -> i32(I).
    

%% --------------------------
%% Split a list in two parts

split_list(0,L) -> {[],L};
split_list(I,L) -> split_list(I,L,[]).

split_list(I,[H|T],Acc) when I>0 -> split_list(I-1,T,[H|Acc]);
split_list(0,L,Acc)              -> {reverse(Acc),L};
split_list(_,[],Acc)             -> {reverse(Acc),[]}.


mk_colorfun(Display) ->
    Screen = Display#display.default_screen,
    mk_colorfun(Display, Screen).

mk_colorfun(Display, Screen) ->
    S = lists:nth(Screen + 1, Display#display.screens),
    D = hd(S#screen.depths),
    V = hd(D#depth.visuals),
    #visual{red_mask=R, green_mask=G, blue_mask=B} = V,
    %% io:format("R=~p G=~p B=~p~n",[R,G,B]),
    mkColorMapFun(R, G, B).

%% Make a HOF which computes the colors

%%   mkColorMapFun(RMask, GMask, BMask) -> f(I) -> Pixel
%% I is a normalised int 16#ffaabb

mkColorMapFun(Rmask, Gmask, Bmask) ->
    Rx = Rmask / 255,
    Gx = Gmask / 255,
    Bx = Bmask / 255,
    fun(I) ->
	    <<_:8,R:8,G:8,B:8>> = <<I:32>>,
	    R1 = (trunc(R * Rx) band Rmask),
	    G1 = (trunc(G * Gx) band Gmask),
	    B1 = (trunc(B * Bx) band Bmask),
	    %% io:format("I=~s~n",[int2hex(I)]),
	    %% io:format("R=~p G=~p B=~p~n", [int2hex(R),int2hex(G),int2hex(B)]),
	    %% io:format("Rmask=~p Gmask=~p Bmask=~p~n", [Rmask,Gmask,Bmask]),
	    %% io:format("R1=~p G1=~p B1=~p~n", [R1,G1,B1]),
	    %% io:format("Val=~s~n", [int2hex(R1 bor G1 bor B1)]),
	    R1 bor G1 bor B1
    end.

%% from httpd_util.erl

int2hex(I) ->
    i2h(I).

i2h(Num)->
    i2h(Num,getSize(Num),[]).

i2h(Num,Pot,Res) when Pot<0 ->
    convert_to_ascii([Num|Res]);

i2h(Num,Pot,Res) ->
    Position=(16 bsl (Pot*4)),
    PosVal=Num div Position,
    i2h(Num-(PosVal*Position),Pot-1,[PosVal|Res]).
convert_to_ascii(RevesedNum)->
    convert_to_ascii(RevesedNum,[]).

convert_to_ascii([],Num)->
    Num;
convert_to_ascii([Num|Reversed],Number)when Num>-1, Num<10 ->
    convert_to_ascii(Reversed,[Num+48|Number]);
convert_to_ascii([Num|Reversed],Number)when Num>9, Num<16 ->
    convert_to_ascii(Reversed,[Num+55|Number]);
convert_to_ascii(NumReversed,Number) ->
    error.

getSize(Num)->
    getSize(Num,0).

getSize(Num,Pot)when Num<(16 bsl(Pot *4))  ->
    Pot-1;

getSize(Num,Pot) ->
    getSize(Num,Pot+1).

    

%% home
%%  redmask      = 16711680  ffff00
%%  green_mask   = 65280       ff00
%%  blue_mask    = 255           ff
%%  bits_per_rgb = 8
%%  map_entries  = 256
%%  look in http://www.visibone.com/colorlab/

%% This is hand written from the color masks
%% redmask       = 63488  = f800 1111 1000 0000 0000
%% green = 2016  =   7e0               111 1110 0000
%% blue = 31     =    1f                      1 1111

%% (5 bit red) ++ (6 bit green) ++ (5 bit blue)
%% 0..31          0..63            0..31

mk_rgb() ->
    {ok, F} = file:open("/usr/X11R6/lib/X11/rgb.txt", [read]),
    {ok, O} = file:open("ex11_lib_rgb.hrl", [write]),
    {ok, O1} = file:open("ex11_lib_rgb.erl", [write]),
    io:format(O, "%% derived from /usr/X11R6/lib/X11/rgb.txt~n",[]),
    L = mk_rgb(F, O, []),
    file:close(F),
    file:close(O),
    io:format(O1, "-module(ex11_lib_rgb).~n", []),
    io:format(O1, "%% derived from /usr/X11R6/lib/X11/rgb.txt~n",[]),
    io:format(O1, "-export([colors/0]).~n", []),
    io:format(O1, "colors() -> ~n~p.~n~n",[L]),
    file:close(O1).

mk_rgb(F, O, Acc) ->
    case io:get_line(F, '') of
	eof -> Acc;
	L -> 
	    Toks = string:tokens(L," \t\n"),
	    %% io:format("~p~n", [Toks]),
	    case Toks of
		[R,G,B,Name] ->
		    R1 = list_to_integer(R),
		    G1 = list_to_integer(G),
		    B1 = list_to_integer(B),
		    Bin = <<0:8,R1:8,G1:8,B1:8>>,
		    <<N:32>> = Bin,
		    S = i2h(N),
		    io:format(O, "-define(~s,16#~s).~n",[Name, S]),
		    Term = {Name,N},
		    mk_rgb(F, O, [Term|Acc]);
		_ ->
		    mk_rgb(F, O, Acc)
	    end
    end.
