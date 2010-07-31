-module(ex11_lib_driver).

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
%% ex11_connect only handles the connection
%%   connection is complex - mainly because some large data structures need
%%   parsing.

%% ex11_driver:start(Host) -> 
%%    {ok, {Pid,Display, Screen}}  if the connection works
%%                                 screen is the screen that was requested at
%%                                 start
%%                                 Pid = is the Pid of the driver process
%%    {error, Why} otherwise

-export([start/0, send_cmd/2, new_id/1, get_display/2]).


-import(ex11_lib, [pError/1, pEvent/1]).
-import(lists, [map/2,reverse/1]).

-include("ex11_lib.hrl").

new_id(Pid) -> rpc(Pid, create_id).
    
get_display(Pid, Key) -> rpc(Pid, {get_display, Key}).

send_cmd(Pid, C) -> Pid ! {cmd, C}.

rpc(Pid, Q) ->
    Pid ! {self(), Q},
    receive
	{Pid, Reply} ->
	    Reply
    end.

start() ->
    process_flag(trap_exit,true),
    S = self(),
    Pid = spawn_link(fun() -> init(S) end),
    %% io:format("ex11_lib_driver start/1 Pid=~p~n",[Pid]),
    receive 
	{Pid,Ack} -> Ack 
    end.

return(Pid, Val) ->
    Pid ! {self(), Val}.

init(From) ->
    case ex11_lib_connect:start() of
	{ok, {Display, Screen, Fd}} ->
	    %% io:format("Display=~p~n",[Display]),
	    %% ?PRINT_DISPLAY(Display),
	    %% Max command length 
	    Max = Display#display.max_request_size,
	    %% io:format("Max RequestSize=~p~n",[Max]),
	    return(From, {ok, {self(), Display, Screen}}),
	    driver_loop(From, Fd, Max);
	Error -> 
	    return(From, {error, connect})
    end.

driver_loop(Client, Fd, Max) ->
    loop(Client, Fd, <<>>, Max, [], 0).

loop(Client, Fd, Bin, Max, OB, LO) ->
    receive
	{cmd, C} ->
	    if
		size(C) + LO < Max ->
		    %% io:format("storing~p bytes~n",[size(C)]),
		    loop(Client, Fd, Bin, Max, [C|OB], LO + size(C));
		true ->
		    send(Fd, reverse(OB)),
		    loop(Client, Fd, Bin, Max, [C], size(C))
	    end;
	flush ->
	    %% io:format("Driver got flush~n"),
	    send(Fd, reverse(OB)),
	    loop(Client, Fd, Bin, Max, [], 0);
	{tcp, Port, BinX} ->
	    %% io:format("received:~p bytes~n",[size(BinX)]),
	    Bin1 = handle(Client, <<Bin/binary, BinX/binary>>),
	    loop(Client, Fd, Bin1, Max, OB, LO);
	{unixdom, Socket, BinX} ->
	    Bin1 = handle(Client, <<Bin/binary, BinX/binary>>),
	    loop(Client, Fd, Bin1, Max, OB, LO);
	{'EXIT', _, die} ->
	    gen_tcp:close(Fd),
	    exit(killed);
	Any ->
	    io:format("top_loop (driver) got:~p~n",[Any]),
	    loop(Client, Fd, Bin, Max, OB, LO)
	after 2000 ->
		if
		    LO > 0 ->
			io:format("Flushing (forgotten xFlush() ???)~n"),
			send(Fd, reverse(OB));
		    true ->
			void
		end,
		loop(Client, Fd, Bin, Max, [], 0)
	end.

handle(Client, <<0:8,_/binary>>= B1) when size(B1) >= 31 ->
    %% error
    {E, Bin1} = split_binary(B1, 32),
    %% io:format("Error:~p~n",[E]),
    Client ! pError(E),
    handle(Client, Bin1);
handle(Client, <<1:8,_/binary>>= B1) when size(B1) >= 31 ->
    %% io:format("Bingo got a reply~n"),
    decode_reply(Client, B1);
handle(Client, B1) when size(B1) >= 32 ->
    {E, Bin1} = split_binary(B1, 32),
    Client ! pEvent(E),
    handle(Client, Bin1);
handle(Client, Bin) when size(Bin) < 32 ->
    Bin.

decode_reply(Client, <<_:32,Len:32,_/binary>> = Bin) ->
    Need = 32 + 4*Len,
    Size = size(Bin),
    %% io:format("Need=~p Size=~p~n",[Need,Size]),
    if
	Need =< Size ->
	    {Bin0, Bin1} = split_binary(Bin, Need),
	    dispatch(Client, Bin0),
	    handle(Client, Bin1);
	Need > Size ->
	    Bin
    end.

dispatch(Client, <<1:8,_:8,Seq:16,_/binary>> = B) ->
    Client ! {reply, Seq, B}.

send({unix, S}, Bin) ->
    unixdom2:send(S, Bin),
    true;
send({tcp, Fd}, Bin) ->
    %% io:format("[~w] Sending ~w bytes to server~n~p~n",
    %% [Seq, size(Bin), Bin]),
    gen_tcp:send(Fd, Bin),
    %sleep(1500).
    true.

% sleep(T) ->
%   receive
%     after T ->
%	    true
%    end.






