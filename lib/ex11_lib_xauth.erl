-module(ex11_lib_xauth).

-author('tnt@home.se').
%%%---------------------------------------------------------------------
%%% Created : 15 Feb 1999 by tnt@home.se
%%% Function: Read all entries of the .Xauthority file.
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
%%%
%%% Contributor(s): ______________________________________.
%%%
%%% Origonal Author: Unknown - probably Torbjörn Törnkvist
%%% Modified: 23 Feb 1998 by tony@cslab.ericsson.se
%%%           To make it work under Windows. Added filename/0.
%%%
%%% Modified 2004-01-01 by joe@sics.se 
%%% Modified 2004-01-17 by Shawn Pearce spearce@spearce.or
%%  2004-01-31 Added code from Luke Gorrie for handlign of unix domain
%%             sockets
%%  2004-02-07 Substantially re-organised joe@sics.se
%%%---------------------------------------------------------------------
%%
%% Interface
%% read()                            -> xauth() | EXIT
%% print(xauth())                    -> void
%% search_unix(xauth(), Display)     -> [Cookies] 
%% search_ip(xauth(), IP, Display)   -> [{IP,Port,Cookie}]
%% all_cookies(xauth())              -> [Cookies] 

-export([read/0, print/0, print/1, search_unix/2, search_ip/3, 
	 all_cookies/1, get_display/2,
	 bstr2hstr/1]).

%% -import(ex11_lib_utils,[all/2,first/2,i16/2,split_list/2]).

-import(lists, [filter/2, map/2, member/2, reverse/1]).

-include_lib("kernel/include/inet.hrl").

-define(FAMILY_LOCAL,          256).  % not part of X standard (i.e. X.h) 
-define(FAMILY_WILD,         65535). 
-define(FAMILY_NETNAME,        254).  % not part of X standard 
-define(FAMILY_KRB5_PRINCIPAL, 253).  % Kerberos 5 principal name 
-define(FAMILY_LOCALHOST,      252).  % for local non-net authentication 
-define(FAMILY_IP_ADDRESS,       0).  % ...as it seems...

-record(xauth,	{family, address, number, name, data}).

-define(PRINT_XAUTH(A),
	io:format("XAUTHORITY:~n"
		  "  family  = ~p~n  address = ~p~n"
		  "  number  = ~p~n  name    = ~p~n"
		  "  data    = ~w~n",
		  [A#xauth.family, A#xauth.address,
		   A#xauth.number, A#xauth.name,
		   A#xauth.data])).

%% -------------------------------------------
%% Return the name of the authority file

filename() ->
    case os:getenv("XAUTHORITY") of
	false ->
	    case os:getenv("HOME") of
		false ->
		    case os:type() of
			{win32,_} ->
			    case os:getenv("USERNAME") of
				false -> "";
				Name ->
				    filename:join(["/users/",Name,
						   ".Xauthority"])
			    end;
			_ -> ""
		    end;
		Home -> filename:join(Home, ".Xauthority")
	    end;
	File -> File
    end.

%% -------------------------------------------
%% Return all entries in the .Xauthority file

read() ->
    F = filename(),
    read(F).

read(Fname) ->
    case file:read_file(Fname) of
	{ok,Bin} ->
	    List = binary_to_list(Bin),
	    case catch parse_xauth(List) of
		{'EXIT',Reason} -> {error,Reason};
		Else            -> {ok,Else}
	    end;
	Error ->
	    Error
    end.

%% ---------------------------------------------
%% Print xauth records.

print() ->
    case filename() of
	""    -> io:format("No Xauthenticate file~n");
	FName -> 
	    {ok, Cookies} = read(FName), 
	    print(Cookies)
    end.

print([]) ->
    ok;
print([Adata|Rest]) ->
    print(Adata),
    print(Rest);
print(A = #xauth{}) ->
    io:format("~-10.10s ~-20s ~-10s ~s~n", 
	      [
	       family2str(A#xauth.family),
	       io_lib:format("~s:~w", [A#xauth.address, A#xauth.number]),
	       A#xauth.name, bstr2hstr(A#xauth.data)
	      ]).

%% get_display(Xauth, Display) ->
%%   [{Type,Address,Code}]

get_display(Xauth, Display) -> 
    Cs = [C||C=#xauth{number=N} <- Xauth, N==Display],
    Cs1 = filter(fun(C) ->
			 member(C#xauth.family, [?FAMILY_LOCAL,   
						 ?FAMILY_IP_ADDRESS])
		 end, Cs),
    map(fun(C) ->
		%% The name is always
		#xauth{family=F,address=A,data=D} = C,
		{symName(F),A,D}
	end, Cs1).

symName(?FAMILY_IP_ADDRESS) -> ip;
symName(?FAMILY_LOCAL) -> unix.


search_unix(Xauth, Display) -> 
    Cs = [C||#xauth{family=F,number=N,data=C} <- Xauth,
	     F == ?FAMILY_LOCAL,N==Display],
    remove_duplicates(Cs).

search_ip(Xauth, IP, Display) -> 
    io:format("ex11_lib_xauth:search_ip IP=~p Display=~p~nXauth=~p~n",
	      [IP, Display,Xauth]),
    io:format("Family =~p~n",[?FAMILY_IP_ADDRESS]),
    %% 
    XX = Display - 6000,
    Tmp = [C||C=#xauth{number=N} <- Xauth,
	     N==XX],
    io:format("Here Tmp=~p~n",[Tmp]),
    Cs = [C||#xauth{family=F,address=A,number=N,data=C} <- Xauth,
	     F == ?FAMILY_IP_ADDRESS,A==IP,N==Display],
    remove_duplicates(Cs).

all_cookies(Xauth) -> 
    Cs = [C||#xauth{data=C} <- Xauth],
    remove_duplicates(Cs).

remove_duplicates(Xs) -> remove_duplicates(Xs, []).

remove_duplicates([H|T], L) ->
    case member(H, L) of
	true ->
	    remove_duplicates(T, L);
	false ->
	    remove_duplicates(T, [H|L])
    end;
remove_duplicates([], L) ->
    L.

parse_xauth([]) -> [];
parse_xauth([Fam1,Fam0,Alen1,Alen0|D0]) ->
    Family = i16(Fam1,Fam0),
    Alen = i16(Alen1,Alen0),
    {Address,D1} = split_list(Alen,D0),
    [Nlen1,Nlen0|D2] = D1,
    Nlen = i16(Nlen1,Nlen0),
    {Number,D3} = split_list(Nlen,D2), 
    [Len1,Len0|D4] = D3,
    Len = i16(Len1,Len0),
    {Name,D5} = split_list(Len,D4),
    [Dlen1,Dlen0|D6] = D5,
    Dlen = i16(Dlen1,Dlen0),
    {Data,Rest} = split_list(Dlen,D6),
    [#xauth{family=Family,
	    address=is_ip(Family,Address),
	    number=list_to_integer(Number),
	    name=Name,
	    data=Data}|
     parse_xauth(Rest)].

is_ip(?FAMILY_IP_ADDRESS,[X3,X2,X1,X0]) -> 
    ip2str(X3,X2,X1,X0);
is_ip(_,WhatEver) -> WhatEver.

ip2str(X3,X2,X1,X0) ->
    lists:flatten(io_lib:format("~w.~w.~w.~w",[X3,X2,X1,X0])).

bstr2hstr([]) 		-> [];
bstr2hstr([X|List]) -> byte2hex(X) ++ bstr2hstr(List).

byte2hex(X) -> [nibble2hex(X bsr 4), nibble2hex(X band 15)].

nibble2hex(X) when X >= 0, X =< 9 -> $0 + X;
nibble2hex(X) -> $a + (X - 10).

family2str(?FAMILY_LOCAL)          -> "local";
family2str(?FAMILY_WILD)           -> "wild";
family2str(?FAMILY_NETNAME)        -> "netname";
family2str(?FAMILY_KRB5_PRINCIPAL) -> "krb5";
family2str(?FAMILY_LOCALHOST)      -> "localhost";
family2str(?FAMILY_IP_ADDRESS)     -> "ip";
family2str(X)                      -> X.

	
i16(X1,X0) ->
    (X1 bsl 8) bor X0.

%% --------------------------
%% Split a list in two parts

split_list(0,L) -> {[],L};
split_list(I,L) -> split_list(I,L,[]).

split_list(I,[H|T],Acc) when I>0 -> split_list(I-1,T,[H|Acc]);
split_list(0,L,Acc)              -> {reverse(Acc),L};
split_list(_,[],Acc)             -> {reverse(Acc),[]}.








