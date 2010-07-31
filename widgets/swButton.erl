-module(swButton).


%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([make/7]).

-include("sw.hrl").

-import(ex11_lib, [ePolyText8/5, rpc/2, sleep/1, 
		   xClearArea/1,
		   xDo/2, xFlush/1,
		   xVar/2]).

make(Parent, X, Y, Width, Ht, Color, Str) ->
    spawn_link(fun() -> init(Parent, X, Y, Width, Ht, Color, Str) end).

%% Parent = the controlling process
%%          Usually a top level

init(Parent, X, Y, Width, Ht, Color, Str) ->
    Display = rpc(Parent, display),
    Attach = rpc(Parent, mountPoint),
    Wargs = #win{x=X, y=Y, border=0,width=Width,ht=Ht,color=Color, 
		 type=button, parent=Attach, 
		 mask = ?EVENT_EXPOSURE bor ?EVENT_BUTTON_PRESS},
    Wargs1 = sw:mkWindow(Display, Parent, Wargs),
    Win = Wargs1#win.win,
    Bin = draw_cmd(Display, Wargs1, Str),
    %% setup some handlers
    loop(Bin, Display, Wargs1, fun(_) -> void end).

loop(B, Display, Wargs, Fun) ->
    receive
	{event,_,buttonPress,X} ->
	    flash(Display, Wargs),
	    Fun(X),
	    loop(B, Display, Wargs, Fun);
	{event,_,expose,_} ->
	    xDo(Display, B),
	    xFlush(Display),
	    loop(B, Display, Wargs, Fun);
	{onClick, Fun1} ->
	    loop(B, Display, Wargs, Fun1);
	{set, Str} ->
	    Win = Wargs#win.win, 
	    xDo(Display, xClearArea(Win)),
	    Bin =  draw_cmd(Display, Wargs, Str),
	    loop(Bin, Display, Wargs, Fun);
	{'EXIT', Pid, Why} ->
	    io:format("Here:~p~n",[{self(),Pid,Why}]),
	    true;
	Any ->
	    Wargs1 = sw:generic(Any, Display, Wargs),
	    loop(B, Display, Wargs1, Fun)
    end.

draw_cmd(Display, Wargs, Str) ->
    #win{win=Win, width=Width,ht=Ht} = Wargs,
    Bin = [ePolyText8(Win, xVar(Display, sysFontId), 10, 18, Str)|
	   sw:raised_frame(Display, Win, Width, Ht)],
    xDo(Display, Bin),
    xFlush(Display),
    Bin.

flash(Display, Wargs) ->
    S = self(),
    Win=Wargs#win.win,
    spawn(fun() ->
		  xDo(Display, xClearArea(Win)),
		  xFlush(Display),
		  sleep(200),
		  S ! {event,Win, expose, void}
	  end).

