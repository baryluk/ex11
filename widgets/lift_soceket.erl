-module(lift_soceket).

-compile(export_all).
-export([autostart/2]).

-import(lists, [foreach/2, member/2]).
-import(misc_utils, [rpc/2, reply/2, make_server/2]).

%% the edaemon is itself a registered process called edaemon :-)

start([Mod]) ->
    Port = Mod:port(),
    log:start(),
    log:trace(true),
    %% make_server is used for the system itself
    make_server(edaemon, fun() -> run(Port) end),
    start_services(Mod);
start(Other) ->
    usage().

stop([Mod]) ->
    Port = Mod:port(),
    case session:start("localhost", Port) of
	{ok, S} ->
	    session:send(S, halt),
	    session:stop(S),
	    io:format("Stopped~n");
	_ ->
	    io:format("Edaemon was not running~n")
    end,
    init:stop().

status([Mod]) ->
    Port = Mod:port(),
    case session:start("localhost", Port) of
	{ok, S} ->
	    Status = session:rpc(S, status),
	    io:format("Status: ~p~n",[Status]),
	    session:stop(S);
	_ ->
	    io:format("Edaemon is not running~n")
    end,
    init:stop().

usage() ->
    io:format("Usage edaemon Port~n"),
    erlang:halt().

autostart(Name, Fun) ->
    rpc(edaemon, {autostart, Name, Fun}).

start_services(Mod) ->
    Mod:cold_start().

run(Port) ->
    case tcp_server:start_raw_server(Port,
				     fun(Socket) ->
					     server_start(Socket)
				     end,
				     1000,
				     0) of
	{ok, Pid} ->
	    true;
	{error, Why} ->
	    io:format("Error:~p~n", [Why]),
	    erlang:halt(1)
    end,
    loop(dict:new()).


%% top loop of edaemon

loop(Dict) ->
    receive
	{From, {autostart, Name, Fun}} ->
	    reply(From, ack),
	    loop(dict:store({autoStart, Name}, Fun, Dict));
	Any ->
	    log:error({edaemon,bad,message,Any}),
	    loop(Dict)
    end.


%% When a new connection is opened we end up here    
%% Note it doesn't matter if we crash - the socket will be closed :-)
%%  server_start(Socket) is spawned once per connection

server_start(Socket) ->
    case get_line(Socket) of
	{ok, Bin} ->
	    case classify(Bin) of
		{ok, MiddleManMod, Args, Service} -> 
		    %% Start a handler -- Mod is middle_man_*
		    %% and we ourselfs become the correct type of middle
		    %% man
		    Handler = handler_start(self(), Service),
		    %% log:info({calling,MiddleManMod,start,
		    %%           binary_to_list(Bin)}),
		    MiddleManMod:start(Args, Handler, Socket, Bin);
		unknown -> 
		    gen_tcp:close(Socket)
	    end;
	tcp_closed ->
	    void;
	{error, Other} ->
	    log:error({edaemon,server,dropping,Other}),
	    gen_tcp:close(Socket)
    end.    
	    
handler_start(Client, Service) ->
    spawn_link(fun() ->
		       process_flag(trap_exit, true),
		       %% log:info({handler,started, self()}),
		       case (catch handler:start(Client, Service)) of
			   {'EXIT', handler_closed} ->
			       %% log:info({handler,stopped, self()}),
			       true;
			   {'EXIT', Abnormal}  ->
			       log:error({handler,bad,exit,Abnormal});
			   Abnormal ->
			       log:error({handler,bad,exit,Abnormal})
		       end
	       end).


%% get_line(Socket) -> {ok, Bin} | error | tcp_closed
%%   returns everything read so far as a Binary
%%   the binary is guarantted to contain a \n
%%   character

get_line(Socket) ->
    get_line(Socket, <<>>).

get_line(Socket, Bin) ->
    receive
	{tcp, Socket, Bin1} ->
	    Bin2 =  <<Bin/binary, Bin1/binary>>,
	    L = binary_to_list(Bin1),
	    case member($\n, L) of
		true  -> {ok, Bin2};
		false -> get_line(Socket, Bin2)
	    end;
	{tcp_closed, Socket} ->
	    tcp_closed;
	Other ->
	    {error, Other}
    end.


%%----------------------------------------------------------------------
%%  Classify looks at the first line 
%%   classify(Bin) -> {ok, MiddleMan, Args, Service} | unknown

classify(Bin) ->
    List = binary_to_list(Bin),
    %% io:format("edaemon classify start=|~s|~p|~w|~n", [List,List,List]),
    case List of
	"estream04\n" ++ _ ->
	    %% io:format("It's an estream~n"),
	    {ok, middle_man_estream,04, void};
	"<?xml" ++ _ ->
	    {ok, middle_man_xml, void, void};
	"ubf1\n" ++ _ ->
	    {ok, middle_man_ubf1, void, void};
	_ ->
	    case is_http(List) of
		{yes, Vsn} ->
		    %% io:format("It's HTTP~n"),
		    {ok, middle_man_http, Vsn, http};
		no ->
		    %% io:format("It's unknown~n"),
		    unknown
	    end
    end.

%%----------------------------------------------------------------------
%% is_http(Str) -> {yes, Vsn}| no

is_http("HTTP/1.0\r\n" ++ _) -> {yes, "1.0"};
is_http("HTTP/1.1\r\n" ++ _) -> {yes, "1.1"};
is_http([_|T])               -> is_http(T);
is_http([])                  -> no.










