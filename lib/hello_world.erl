-module(hello_world).

%%% Hello_world template
%%% Created: 2003-12-29 by joe@sics.se

%%% Cut and paste from this to make your first windows
%%% program


-import(ex11_lib, [eMapWindow/1,
		   ePolyText8/5,
		   xAddAction/3,
		   xCreateGC/2,
		   xCreateSimpleWindow/7,
		   xDo/2,
		   xEnsureFont/2,
		   xFlush/1,
		   xColor/2,
		   xSpawn/1,
		   xStart/1]).

-export([start/0]).

-include("ex11_lib.hrl").

start() -> 
    spawn_link(fun() -> init() end).

init() ->
    {ok, Pid} = xStart("3.2"),
    Win  = xCreateSimpleWindow(Pid, 10, 10, 300, 100, ?XC_arrow, 
			       xColor(Pid, ?wheat2)),
    Font = xEnsureFont(Pid, "9x15"),  
    Pen  = xCreateGC(Pid, [{function, copy},
			   {font, Font},
			   {fill_style, solid},
			   {foreground, xColor(Pid, ?DarkBlue)}]),
    Cmd =  ePolyText8(Win, Pen, 10, 35, "Hello World"),
    xDo(Pid, Cmd),
    xDo(Pid, eMapWindow(Win)),
    xFlush(Pid),
    loop(Pid, Win, Cmd).

loop(Pid, Win, Cmd) ->
    receive
	{event,Win,expose,_} ->
	    io:format("expose event sending data~n"),
	    xDo(Pid, Cmd),
	    xFlush(Pid),
	    loop(Pid, Win, Cmd);
	Any ->
	    io:format("Here:~p~n",[Any]),
	    loop(Pid, Win, Cmd)
    end.



   









