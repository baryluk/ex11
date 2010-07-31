-module(lift_socket_client).
-compile(export_all).


start() ->
    Pid = spawn_link(fun() -> start1() end),
    loop(Pid).

loop(Pid) ->
    Str = io:get_line('>'),
    case Str of
	"Q\n" ->
	    exit(Pid);
	_ ->
	    Pid ! {line, Str},
	    loop(Pid)
    end.


start1() ->
    case gen_tcp:connect("localhost", 2000,
                         [binary, {active, true},
                          {packet, 0}]) of
        {ok, Socket} ->
	    loop1(Socket);
	Other ->
	    io:format("Cannot open socket:~p~n",[Other])
    end.

loop1(Socket) ->
    receive
	{tcp, Socket, Bin} ->
	    io:format(" +++ ~s",[binary_to_list(Bin)]),
	    loop1(Socket);
	{tcp_closed, Socket} ->
	    io:format("Socket closed~n"),
	    exit(dies);
	{line, Str} ->
	    gen_tcp:send(Socket, [Str]),
	    loop1(Socket)
    end.

  
	    
