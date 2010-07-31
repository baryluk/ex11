-module(ex11_lib_connect).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.


%% 2004-01-01 Origional version by by joe@sics.se

%% 2004-01-17 Improved handling of code to resolve display addresses by
%%            Shawn Pearce <spearce@spearce.org>
%% 2004-01-28 Fixed get_connect_reply which only worked if the
%%            complete reply was read in one go with no extra data.
%%            Frej Drejhammar <frej@stacken.kth.se>
%% 2004-01-31 Added code from Luke Gorrie for handling of unix domain
%%            sockets
%% 2004-02-15 Added initial support for multiple screens
%%            Frej Drejhammar <frej@stacken.kth.se>
%% ex11_connect only handles the connection
%%   connection is complex - mainly because some large data structures need
%%   parsing.

%% ex11_driver:start(Host) -> 
%%    {ok, {Pid,Display}}    if the connection works
%%    {error, Why} otherwise

%% author: joe@sics.se

%% ex11_connect only handles the connection
%%   connection is complex - mainly because some large data structures need
%%   parsing.

%% ex11_lib_connect:start(Host) -> 
%%    {ok, {Display, Screen, Fd}}    if the connection works
%%    {error, Why} otherwise


-export([start/0, start/1, find_my_real_ip_address/0]).

-import(lists, [all/2, filter/2, foldl/3, map/2, member/2, reverse/1]).

-include("ex11_lib.hrl").
-include_lib("kernel/include/inet.hrl").

start() ->
    case os:getenv("DISPLAY") of
	false   -> 
	    io:format("no DISPLAY variable set - assuming :0.0~n"),
	    start(":0.0");
	Display -> 
	    start(Display)
    end.

start(Display) ->
    %% io:format("Trying to open connection with:~s~n",[Display]),
    case ex11_lib_xauth:read() of
	{ok, Xauth} ->
	    %% io:format("Xauth~n"),
	    %% ex11_lib_xauth:print(Xauth),    
	    case parse_display(Display) of
		{ok, {Host, DisplayNumber, ScreenNumber}}->
		    %% Get all the relevant entries from Xauth
		    Es = ex11_lib_xauth:get_display( Xauth, DisplayNumber), 
		    Es1 = map(fun({X,Y,_}) -> {X,Y} end, Es),
		    %% io:format("Start Host=~p Es=~n~p~n",[Host,Es]),
		    Try = tryList(Host, DisplayNumber, Es),
		    %% io:format("Try these:~p~n",[Try]),
		    case try_to_start(Try, ScreenNumber) of
			error -> 
			    {ok, HostName} = inet:gethostname(),
			    make_error_report(Display, Host, DisplayNumber, ScreenNumber,
					      HostName, Es, Try),
			    io:format("initialisation failed~n"),
			    io:format("Please mail the file startup_error_report to joe@sics.se~n"),
			    error;
			Other ->
			    {ok, HostName} = inet:gethostname(),
			    make_error_report(Display, Host, DisplayNumber, ScreenNumber,
					      HostName, Es, Try),
			    Other
		    end;
		Error ->
		    Error
	    end;
	E ->
	    io:format("cannot read XAuthenticate~n"),
	    E
    end.


make_error_report(Display, Host, DisplayNumber, ScreenNumber, HostName, Es, Try) ->
    {ok, S } = file:open("startup_error_report", [write]),
    io:format(S, "$DISPLAY = ~s~n", [Display]),
    io:format(S, "parsed display Host=~p DisplayNumber=~p ScreenNumber=~p~n",
	      [Host, DisplayNumber, ScreenNumber]),
    io:format(S, "inet:gethostname() = ~p~n", [HostName]),
    Labels = make_labels(Es, 1, dict:new()),
    Es1 = rename_es(Es,  Labels), 
    Try1 = rename_try(Try, Labels),
    io:format(S, "Es~n", []),
    lists:foreach(fun(I) -> io:format(S, "~p~n", [I]) end, Es1),
    io:format(S, "Try list~n", []),
    lists:foreach(fun(I) -> io:format(S, "~p~n", [I]) end, Try1),
    file:close(S).
    

make_labels([{_,_,Code}|T], Max, D) ->
    case dict:find(Code, D) of
	error ->
	    make_labels(T, Max+1, dict:store(Code, Max, D));
	_ ->
	    make_labels(T, Max, D)
    end;
make_labels([], _, D) -> D.

rename_es(Es, D) ->
    map(fun({X,Y,C}) -> {X,Y,"Code" ++ integer_to_list(dict:fetch(C, D))} end,
	Es).

rename_try(Try, D) ->
    map(fun({X,Y,C}) -> {X,Y,"Code" ++ integer_to_list(dict:fetch(C, D))} end,
	Try).


tryList(Host, Display, Es) ->
    %% io:format("tryList Host=~p Dispay=~p Es=~p~n",[Host, Display, Es]),
    All = 
	tryList1(Host, Display, Es) ++	
	tryList1({host,"localhost"}, Display, Es) ++
	tryList1({host,"127.0.0.1"}, Display, Es) ++
	tryList1(everything, Display, Es),
    remove_duplicates(All, []).


remove_duplicates([H|T], L) ->
    case member(H, L) of
	true -> remove_duplicates(T, L);
	false -> remove_duplicates(T, [H|L])
    end;
remove_duplicates([], L) ->
    reverse(L).


tryList1(everything, Display, Es) ->
    [{unix,Display,Code} || {unix,Name,Code} <- Es] ++
	[{{ip,"localhost"},Display,Code} || {ip,Name,Code} <- Es];
tryList1(none, Display, Es) ->
    %% If no hostname given look up the hostname
    {ok, HostName} = inet:gethostname(),
    [{unix,Display,Code} || {unix,Name,Code}<-Es, matches(HostName,Name)] ++
	[{{ip,"localhost"},Display,Code} || {ip,Name,Code}<-Es, Name == HostName];
tryList1({host, "localhost"}, Display, Es) ->
    %% io:format("local host specified - checking my hostname~n"),
    {ok, HostName} = inet:gethostname(),
    %% io:format("Hostname:~s~n",[HostName]),
    %% Hack city this is listed as a unix socket - but *really*
    %% it's on a port    
    [{{ip,"localhost"},Display,Code} || {unix,Name,Code} <- Es, matches(HostName, Name)];
tryList1({host, "127.0.0.1"}, Display, Es) ->
    [{{ip,"localhost"},Display,Code} || {ip,"127.0.0.1", Code} <- Es];
tryList1({host, H}, Display, Es) ->
    [{{ip,H},Display,Code} ||{ip,Name,Code}<-Es, matches(H, Name)].


matches(X, Y) -> 
	matches1(X, Y).


matches1([], _) -> true;
matches1([H|T],[H|T1]) -> matches1(T, T1);
matches1(_,_) -> false.

%%----------------------------------------------------------------------
%% iterate down the list of possible starting points

%% try_to_start(List, Screen)
%%   List = [{Host,Display,Cookie}] where Host = {ip, HostName} | unix
%%     If Host = {ip,HostName} we open socket 6000+Display on HostName
%%     If Host = unix          we open unix domain socket <Display>       

try_to_start([], Screen) -> error;
try_to_start([H|T], Screen) ->
    case try_to_connect(H, Screen) of
	O = {ok, D} ->
	    O;
	{error, _} ->
	    try_to_start(T, Screen)
    end.

%%----------------------------------------------------------------------
%% try_to_connect({Host,Display,Cookie}, Screen) -> 
%%     {ok, {Display, Screen, Fd}} | error
%%     Host = unix | {ip, IP} | local

try_to_connect({Host, Display, Cookie}, Screen) ->
    io:format("Trying Host=~p Display=~p Screen=~p~n",
	      [Host, Display, Screen]),
    case connect(Host, Display) of
	{ok, Fd} -> 
	    io:format("Port opened sending cookie:~n"),
	    Res = send(Fd, ex11_lib:eConnect(Cookie)),
	    Bin = get_connect_reply(Fd, <<>>),
	    case ex11_lib:pConnect(Bin, Screen) of
		{ok, Dpy} ->
		    %% io:format("Display=~p~n",[Dpy]),
		    %% ?PRINT_DISPLAY(Dpy),
		    {ok, {Dpy, Screen, Fd}};
		Error ->
		    Error
	    end;
	Error = {error, Why} ->
	    io:format("cannot connect reason:~p~n",[Why]),
	    Error
    end.

connect(unix, Display) ->
    case (catch unixdom2:module_info()) of
        {'EXIT',_} ->
            {error,noUnixDomainSockets};
        _ ->
	    io:format("Connecting to unix domain socket:~p~n",[Display]),
	    {ok, Sock} = unixdom2:start_link(),
	    Path = lists:flatten(io_lib:format("/tmp/.X11-unix/X~p", 
					       [Display])),
	    unixdom2:connect(Sock, Path, [{active,true}, binary]),
	    {ok, {unix, Sock}}
    end;
connect({ip,IP}, Display) ->
    io:format("Connecting to tcp port:~p~n",[6000+Display]),
    case gen_tcp:connect(IP, 6000+Display, [{packet,raw}, binary]) of
	{ok, Sock} -> {ok, {tcp, Sock}};
	Err        -> Err
    end;
connect(local, Display) ->
    case gen_tcp:connect("localhost", 6000+Display, [{packet,raw}, binary]) of
	{ok, Sock} -> {ok, {tcp, Sock}};
	Err        -> Err
    end.


%%----------------------------------------------------------------------
%% If DISPLAY=Blaaa:N.M 
%%   {V1,V2,V3} each V1 = Str | none V1 = V2 = int
%%

%% parse_display(DISPLAY) ->
%%   {ok, {unix, {Display,Screen}}} |
%%   {ok, {ip, Host, {Display,Screen}}} |
%%   {error, Why}

parse_display(DISPLAY) ->
    case (catch parse_display1(DISPLAY)) of
	{'EXIT', Why} ->
	    {error, {display,DISPLAY, Why}};
	Other ->
	    {ok, Other}
    end.

parse_display1(":" ++ T) ->
    {none, parse_display_number(T), parse_screen_number(T)};
parse_display1(Name) ->
    case string:tokens(Name, ":") of
	[Host, Num]    -> {{host, Host}, 
			   parse_display_number(Num),
			   parse_screen_number(Num)};
	[Host]         -> {{host, Host}, 0, 0};
	[]             -> exit(badDisplay)
    end.

%% parse_display_number("N.M") -> N
%% parse_display_number("N")   -> N

parse_display_number(Str) ->
    case string:tokens(Str, ".") of
	    [Num|_]   -> list_to_integer(Num);
	    Num       -> list_to_integer(Num)
    end.

parse_screen_number(Str) ->
    case string:tokens(Str, ".") of
	[_, Num] -> list_to_integer(Num);
	_        -> 0
    end.


%%----------------------------------------------------------------------
%% Get connect_reply
%%   Has two passes first we have to do the frameing to make sure the 
%%   entire data structue has been retreived then we parse the reply.
%%   The connection reply is 8 + (LengthOfExtraData) * 4 bytes long
%%   the length is a two byte quantity in bytes 7..8 of the reply

get_connect_reply(Fd, Bin0) ->
    Bin = my_concat_binary(Bin0, recv(Fd)),
    case Size = size(Bin) of
	N when N < 8 -> get_connect_reply(Fd, Bin);
	_ ->
	    <<_:48, LenExtra:16,_/binary>> = Bin,
	    Need = 8 + LenExtra*4,
	    if 
		Need > Size ->
		    get_connect_reply(Fd, Bin);
		
		Need =< Size ->
		    Bin
	    end
    end.

my_concat_binary(<<>>, B) -> B;
my_concat_binary(B1, B2)  -> list_to_binary([B1, B2]).

recv({unix, S}) ->
    receive
	{unixdom, S, Data} -> Data
    end;
recv({tcp, Fd}) ->
    receive
	{tcp,Fd,Data} -> 
	    %% io:format("Received ~w bytes from server~n~p~n",
	    %% [size(Data), Data]),
	    Data
    end.



send({unix, S}, Bin) ->
    unixdom2:send(S, Bin),
    true;
send({tcp, Fd}, Bin) ->
    %% io:format("[~w] Sending ~w bytes to server~n~p~n",
    %% [Seq, size(Bin), Bin]),
    gen_tcp:send(Fd, Bin),
    %sleep(1500).
    true.


find_my_real_ip_address() ->
    %% return my real IP address(s)
    case inet:gethostname() of
	{ok,Host} ->
	    case inet:gethostbyname(Host) of
		{ok, H} ->
		    %% io:format("Here1:~p~n",[H]),
		    IPs = H#hostent.h_addr_list,
		    IPs1 = map(fun(I) -> ip2str(I) end, IPs),
		    %% io:format("My real IP=~s~n",[IPs1]),
		    {ok, IPs1};
		{error, _} ->
		    {error, 'cannot Determine Local IP Address'}
	    end;
	{error, _}->
	    {error, 'connot determine Hostname'}
    end.

%% force_hostOrIp_to_ip(HostOrIP) ->
%% Host is either an IP or a hostname
%% If it is a hostname then convert it to an IP
%%     io:format("implement force_hostOrIp_to_ip:~p~n",[HostOrIP]),
%%     case isIP(HostOrIP) of
%% 	false ->
%% 	    case inet:gethostbyname(HostOrIP) of
%% 		{ok, H} ->
%% 		    io:format("Here2:~p~n",[H]),
%% 		    IPs = H#hostent.h_addr_list,
%% 		    IPs;
%% 		{error, _} ->
%% 		    {error, 'cannot Determine IP Address of', HostOrIP}
%% 	    end;
%% 	true ->
%% 	    HostOrIP
%%     end.

%% isIP("123.45....") -> true
%% isIP("aaa.ddd....") -> false

%% isIP(X) ->
%%     X1 = filter(fun($.) -> false; (_) -> true end, X),
%%     all(fun is_digit/1, X1).
 
%% is_digit(X) when X >= $0, X =< $9 -> true;
%% is_digit(_) -> false.

ip2str({X3,X2,X1,X0}) -> ip2str(X3,X2,X1,X0);
ip2str([X3,X2,X1,X0]) -> ip2str(X3,X2,X1,X0).

ip2str(X3,X2,X1,X0) ->
    lists:flatten(io_lib:format("~w.~w.~w.~w",[X3,X2,X1,X0])).







