-module(hello_buttons).

%%% hello_buttons: demonstarates how to make a button widget
%%  using ex11_lib
%%  To run this program fgive the command
%%     erl -s hello_buttons start

%%  To *understand* this program read 

%%% Created: 2003-12-29 by joe@sics.se


-import(ex11_lib, [eMapWindow/1,
		   ePolyFillRectangle/3,
		   ePolyLine/4,
		   ePolyText8/5,
		   mkPoint/2,
		   mkRectangle/4,
		   xAddAction/3,
		   eClearArea/6,
		   xColor/2,
		   xCreateGC/1,
		   xCreateGC/2,
		   xCreateNamedGC/3,
		   xCreateSimpleWindow/7,
		   xCreateSimpleWindow/10,
		   xDo/2,
		   xEnsureFont/2,
		   xFlush/1,
		   xGC/2,
		   xSpawn/1,
		   xStart/1]).

-export([start/0]).

-include("ex11_lib.hrl").



start() -> 
    spawn_link(fun() -> init() end).

init() ->
    {ok,Pid}   = xStart("3.2"),
    spawn(fun()  -> win1(Pid) end),
    spawn(fun()  -> win2(Pid) end),
    spawn(fun()  -> win3(Pid) end),
    spawn(fun()  -> win4(Pid) end),
    spawn(fun()  -> win5(Pid) end),
    true.
    
win1(Pid) ->
    Win  = xCreateSimpleWindow(Pid, 10, 10, 300, 100, ?XC_arrow, 
			       xColor(Pid, ?wheat2)),
    Font = xEnsureFont(Pid, "9x15"),  
    Pen  = xCreateGC(Pid, [{function, copy},
			   {font, Font},
			   {fill_style, solid},
			   {foreground, xColor(Pid, ?DarkBlue)}]),
    Red = xCreateGC(Pid, [{function, copy},
			  {font, Font},
			  {fill_style, solid},
			  {foreground, xColor(Pid, ?red)}]),
    xCreateNamedGC(Pid, "black", [{function,copy},
				  {line_width,2},
				  {line_style,solid},
				  {foreground, xColor(Pid, ?black)}]),
    xCreateNamedGC(Pid, "white", [{function,copy},
				  {line_width,2},
				  {line_style,solid},
				  {foreground, xColor(Pid, ?white)}]),
    Cmds  = [ePolyFillRectangle(Win, Red,
				[mkRectangle(10,30,120,10)]),
	     ePolyLine(Win, xGC(Pid, "black"), origin, 
		       [mkPoint(10,50),mkPoint(140,50), mkPoint(140,15)]),
	     ePolyLine(Win, xGC(Pid, "white"), origin, 
		       [mkPoint(150,15), mkPoint(150,50), mkPoint(180, 50)]),
	     ePolyText8(Win, Pen, 10, 35, "Hello World")],
    xDo(Pid, eMapWindow(Win)),
    xFlush(Pid),
    loop(Pid, Cmds).

loop(Pid, Cmds) ->
    receive
	{event,_,expose,_} ->
	    xDo(Pid, Cmds),
	    xFlush(Pid),
	    loop(Pid, Cmds);
	Any ->
	    io:format("here:~p~n",[Any]),
	    loop(Pid, Cmds)
    end.


win2(Pid) ->
    Win  = xCreateSimpleWindow(Pid, 10, 10, 300, 100, ?XC_arrow, xColor(Pid, ?wheat2)),
    Font = xEnsureFont(Pid, "9x15"),  
    Pen  = xCreateGC(Pid, [{function, copy},
			   {font, Font},
			   {fill_style, solid},
			   {foreground, xColor(Pid, ?DarkBlue)}]),
    Red = xCreateGC(Pid, [{function, copy},
			  {font, Font},
			  {fill_style, solid},
			  {foreground, xColor(Pid, ?red)}]),
    xCreateNamedGC(Pid, "black", [{function,copy},
				  {line_width,2},
				  {line_style,solid},
				  {foreground, xColor(Pid, ?black)}]),
    xCreateNamedGC(Pid, "white", [{function,copy},
				  {line_width,2},
				  {line_style,solid},
				  {foreground, xColor(Pid, ?white)}]),
    Cmds  = [ePolyFillRectangle(Win, Red,
				[mkRectangle(10,20,110,22)]),
	     ePolyLine(Win, xGC(Pid, "black"), origin, 
		       [mkPoint(10,43),mkPoint(120,43), mkPoint(120,20)]),
	     ePolyLine(Win, xGC(Pid, "white"), origin, 
		       [mkPoint(10,43),mkPoint(10,20), mkPoint(120,20)]),
	     ePolyText8(Win, Pen, 12, 35, "Hello World")],
    xDo(Pid, eMapWindow(Win)),
    xFlush(Pid),
    loop(Pid, Cmds).


win3(Pid) ->
    Win    = xCreateSimpleWindow(Pid, 10, 10, 300, 100, ?XC_arrow, 
				 xColor(Pid, ?wheat2)),
    Region = xCreateSimpleWindow(Pid, Win, 20, 20,100,50,1, ?XC_cross, 
				 xColor(Pid, ?white),
				 ?EVENT_EXPOSURE bor ?EVENT_BUTTON_PRESS),
    xDo(Pid, eMapWindow(Win)),
    xDo(Pid, eMapWindow(Region)),
    xFlush(Pid),
    loop(Pid).

loop(Pid) ->
    receive
	Any ->
	    io:format("Any=~p~n",[Any]),
	    loop(Pid)
    end.

win4(Pid) ->
    Win    = xCreateSimpleWindow(Pid, 10, 10, 300, 100, ?XC_arrow, 
				 xColor(Pid, ?wheat2)),
    Button = xCreateSimpleWindow(Pid, Win, 20,20,130,25,0, ?XC_cross, 
				 xColor(Pid, ?white),
				 ?EVENT_EXPOSURE bor ?EVENT_BUTTON_PRESS),
    Font = xEnsureFont(Pid, "9x15"),  
    Pen  = xCreateGC(Pid, [{function, copy},
			   {font, Font},
			   {fill_style, solid},
			   {foreground, xColor(Pid, ?DarkBlue)}]),
    Red = xCreateGC(Pid, [{function, copy},
			  {font, Font},
			  {fill_style, solid},
			  {foreground, xColor(Pid, ?red)}]),
    xCreateNamedGC(Pid, "black", [{function,copy},
				  {line_width,2},
				  {line_style,solid},
				  {foreground, xColor(Pid, ?black)}]),
    Cmds  = [ePolyFillRectangle(Button, Red,
				[mkRectangle(2,2,126,22)]),
	     ePolyLine(Button, xGC(Pid, "black"), origin, 
		       [mkPoint(1, 24),mkPoint(129,24), mkPoint(129,1)]),
	     ePolyText8(Button, Pen, 12, 16, "Press Me")],
    F = fun(_) -> xDo(Pid, Cmds), xFlush(Pid) end,
    xDo(Pid, eMapWindow(Win)),
    xDo(Pid, eMapWindow(Button)),
    loop1(Pid, Button, F).

loop1(Pid, B, F) ->
    receive
	{event,_,buttonPress,_} ->
	    flash(Pid, B, F),
	    loop1(Pid, B, F);
	{event,_,expose,_} ->
	    F(void),
	    loop1(Pid, B, F);
	Any ->
	    io:format("~p~n",[Any]),
	    loop1(Pid, B, F)
    end.


win5(Pid) ->
    Win = xCreateSimpleWindow(Pid, 10, 10, 300, 100, ?XC_arrow, 
			      xColor(Pid, ?wheat2)),
    init_buttons(Pid),
    spawn(fun() -> add_button(Pid, Win, 10, 10, "Press me", 
			      fun() -> io:format("Button 1 pressed~n") end)
	  end),
    spawn(fun() -> add_button(Pid, Win, 10, 45, "I am button 2", 
			      fun() -> io:format("Button 2 pressed~n") end)
	  end),
    xFlush(Pid),
    loop(Pid).
    
add_button(Pid, Win, X, Y, Str, Fun) ->
    Width = 9*length(Str) + 24,
    Button = xCreateSimpleWindow(Pid, Win, X, Y, Width, 25,0, ?XC_cross, 
				 xColor(Pid, ?white),
				 ?EVENT_EXPOSURE bor ?EVENT_BUTTON_PRESS),
    Cmds  = [ePolyFillRectangle(Button, xGC(Pid, "red"),
				[mkRectangle(2,2,Width,22)]),
	     ePolyLine(Button, xGC(Pid, "black"), origin, 
		       [mkPoint(1, 24),mkPoint(Width-1,24), 
			mkPoint(Width-1,1)]),
	     ePolyText8(Button, xGC(Pid, "pen"), 12, 16, Str)],
    Redraw = fun() -> xDo(Pid, Cmds), xFlush(Pid) end,
    Press = fun() -> 
		    Fun(),
		    xDo(Pid, eClearArea(false, Button, 0, 0, 0, 0)),
		    xFlush(Pid),
		    sleep(100),
		    Redraw()
	    end,
    xDo(Pid, eMapWindow(Win)),
    xDo(Pid, eMapWindow(Button)),
    xFlush(Pid),
    loopB(Button, Redraw, Press).


loopB(Button, Redraw, Press) ->
    receive
	{event, _, buttonPress, _} ->
	    Press(),
	    loopB(Button, Redraw, Press);
	{event, _, expose, _} ->
	    Press(),
	    loopB(Button, Redraw, Press);
	Any ->
	    io:format("Any=~p~n",[Any]),
	    loopB(Button, Redraw, Press)
    end.

init_buttons(Pid) ->
    Font   = xEnsureFont(Pid, "9x15"),  
    xCreateNamedGC(Pid, "pen", [{function, copy},
				{font, Font},
				{fill_style, solid},
				{foreground, xColor(Pid, ?DarkBlue)}]),
    xCreateNamedGC(Pid, "red", [{function, copy},
				{font, Font},
				{fill_style, solid},
				{foreground, xColor(Pid, ?red)}]),
    xCreateNamedGC(Pid, "black", [{function,copy},
				  {line_width,2},
				  {line_style,solid},
				  {foreground, xColor(Pid, ?black)}]),
    xFlush(Pid).


flash(Pid, Button, Refresh) ->
    xDo(Pid, eClearArea(false, Button, 0, 0, 0, 0)),
    xFlush(Pid),
    sleep(500),
    Refresh(void).

sleep(T) ->
    receive
    after T ->
	    true
    end.
