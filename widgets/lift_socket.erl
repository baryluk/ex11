-module(lift_socket).

-compile(export_all).
-export([autostart/2]).

-import(lists, [foreach/2, member/2]).
-import(misc_utils, [rpc/2, reply/2, make_server/2]).

%% the edaemon is itself a registered process called edaemon :-)

start(Port) ->
    run(2000).
    
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
    end.

server_start(Socket) ->
    Pid = swLifts:make(),
    S = self(),
    Pid ! {onClick, fun(X) -> S ! {event, X} end},
    link(Pid),
    get_line(Socket, Pid, "").

get_line(Socket, Pid, L0) ->
    receive
	{tcp, Socket, Bin} ->
	    L1 = handle(binary_to_list(Bin), L0, Pid),
	    get_line(Socket, Pid, L1);
	{tcp_closed, Socket} ->
	    tcp_closed;
	{event, X} ->
	    X = format_event(X),
	    gen_tcp:send(Socket, [X]),
	    get_line(Socket, Pid, L0);
	Other ->

	    {error, Other}
    end.

format_event({click,floor,N,up}) -> [$F, N+$0, $U,$\n];
format_event({click,floor,N,down}) -> [$F, N+$0, $D,$\n];
format_event({click,floor,N,M}) -> [$B, N+$0, M+$0,$\n].

handle([$\n|T], L, Pid) ->
    do_cmd(reverse(L), Pid),
    handle(T, []);
handle([H|T], L, Pid) ->
    handle(T, [H|L], Pid);
handle([], L, Pid) ->
    L.

do_cmd(Cmd, Pid) ->
    io:format("Do cmd:~p~n",[Cmd]),
    true.


    

