-module(lift_socket_server).

-compile(export_all).

-include("sw.hrl").

-import(lists, [reverse/1, foreach/2, member/2]).


%% the edaemon is itself a registered process called edaemon :-)

start() ->
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
	    io:format("here:~p~n",[binary_to_list(Bin)]),
	    L1 = handle(binary_to_list(Bin), L0, {Pid,Socket}),
	    get_line(Socket, Pid, L1);
	{tcp_closed, Socket} ->
	    io:format("TCP CLOSED~n"),
	    exit(Pid),
	    tcp_closed;
	{event, X} ->
	    Y = format_event(X),
	    gen_tcp:send(Socket, [Y]),
	    get_line(Socket, Pid, L0);
	Other ->
	    {error, Other}
    end.

format_event({click,floor,N,up}) -> [$F, N+$0, $U,$\n];
format_event({click,floor,N,down}) -> [$F, N+$0, $D,$\n];
format_event({click,lift,N,M}) -> [$L, N+$0, M+$0,$\n].

handle([$\n|T], L, Control) ->
    do_cmd(reverse(L), Control),
    handle(T, [], Control);
handle("\r\n" ++ T, L, Control) ->
    do_cmd(reverse(L), Control),
    handle(T, [], Control);
handle([H|T], L, Control) ->
    handle(T, [H|L], Control);
handle([], L, Control) ->
    L.

do_cmd([$L,N,N1,N2,N3], {Pid, _}) ->
    Lift = list_to_integer([N]),
    Pos = list_to_integer([N1,N2,N3]),
    Pid ! {setPos, Lift, Pos};
do_cmd([$D,N,N1,N2,N3], {Pid,_}) ->
    Lift = list_to_integer([N]),
    Pos = list_to_integer([N1,N2,N3]),
    Pid ! {setDoor, Lift, Pos};
do_cmd([$I,N,M,C], {Pid,_}) ->
    Lift = list_to_integer([N]),
    Floor = list_to_integer([M]),
    Pid ! {setLiftLamp, Lift, Floor, cmap(C)};
do_cmd([$F,N,M,C], {Pid,_}) ->
    Lift = list_to_integer([N]),
    Floor = list_to_integer([M]),
    Pid ! {setFloorLamp, Lift, Floor, cmap(C)};
do_cmd("H", {Pid, Socket}) ->
    gen_tcp:send(Socket, 
		 "Q         -- quit\n"
		 "L<N><MMM> -- move lift  N to pos MMM (N=1..3 MMM=000..100)\n"
		 "D<N><MMM> -- move doors N to pos MMM (N=1..3 MMM=000..100)\n"
		 "ILFC      -- set lift  lamp L=1..3 F=1..6 C=R|G|W\n"
		 "FLFC      -- set floor lamp L=1..3 F=1..6 C=R|G|W\n");
do_cmd("Q", _) ->
    exit(true);
do_cmd(Str, {Pid,Socket}) ->
    io:format("Do cmd:~p~n",[Str]),
    gen_tcp:send(Socket, "Widget does not understand:" ++ "|" ++ Str ++ "|\n"),
    gen_tcp:send(Socket, "type H for help\n"),
    true.

cmap($G) -> ?grey84;
cmap($R) -> ?red;
cmap($W) -> ?white.

    
    

