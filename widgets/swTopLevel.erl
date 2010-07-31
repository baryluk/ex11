-module(swTopLevel).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% 2004-01-?? Original version by joe@sics.se
%% 2004-02-15 Added support for multiple screens
%%            Frej Drejhammar <frej@stacken.kth.se>


-include("sw.hrl").

-import(ex11_lib,
	[reply/2, reply/2, rpc/2, get_display/2, get_root_of_screen/2,
	 xClearArea/1, xColor/2, xCreateGC/3, xDo/2, xFlush/1]).

-import(lists, [foreach/2]).

-export([make/4, make/5]).

make(Parent, Width, Ht, Color) ->
    make(Parent, get_display(Parent, default_screen), Width, Ht, Color).

make(Parent, Screen, Width, Ht, Color) ->
    S = self(),
    spawn_link(fun() -> init(S, Parent, Screen, Width, Ht, Color) end).

init(Pid, Display, Screen, Width, Ht, Color) ->
    Wargs = #win{parent=get_root_of_screen(Display, Screen),
		 width=Width,ht=Ht,color=Color,type=top,
		 mask=?EVENT_BUTTON_PRESS bor ?EVENT_EXPOSURE bor 
		 ?EVENT_STRUCTURE_NOTIFY,
		 screen=Screen},
    Wargs1 = sw:mkWindow(Display, Pid, Wargs),
    Win = Wargs1#win.win,
    process_flag(trap_exit, true),
    %Display1 = rpc(Display, display),
    io:format("Display=~p~n",[Display]),
    loop(Display, Wargs1, dict:new(), {[],1}, 
	 fun(_) -> void end, fun(_) -> void end).

loop(Display, Wargs, Pens, L, Fun, CFun) ->
    receive
	{onClick, Fun1} ->
	    loop(Display, Wargs, Pens, L, Fun1, CFun);
	{onReconfigure, Fun1} ->
	    loop(Display, Wargs, Pens, L, Fun, Fun1);
	{event,_,expose,_} ->
	    refresh(Display, Wargs#win.win,L),
	    loop(Display, Wargs, Pens, L, Fun, CFun);
	{event,_,buttonPress, X} ->
	    Fun(X),
	    loop(Display, Wargs, Pens, L, Fun, CFun);
	{event,_,configureNotify, X} ->
	    CFun(X),
	    loop(Display, Wargs, Pens, L, Fun, CFun);
	{event,_,buttonPress, X} ->
	    Fun(X),
	    loop(Display, Wargs, Pens, L, Fun, CFun);
	{newPen, Name, Color, Width} ->
	    Win = Wargs#win.win,
	    GC = xCreateGC(Display, Win,
			   [{function,copy},
			    {line_width,Width},
			    {line_style,solid},
			    {foreground, xColor(Display, Color)}]),
	    Pens1 = dict:store(Name, GC, Pens),
	    loop(Display,Wargs,Pens1, L, Fun, CFun);
	{From, {draw, Pen, Obj}} ->
	    case dict:find(Pen, Pens) of
		{ok, GC} ->
		    case swCanvas:cdraw(Wargs#win.win, GC, Obj) of
			{error, What} ->
			    exit({badArg, What});
			Bin ->
			    {L1, Free} = L,
			    L2 = L1 ++  [{Free, Bin}],
			    L3 = {L2, Free+1},
			    refresh(Display,Wargs#win.win,L3),
			    reply(From, Free),
			    loop(Display, Wargs, Pens, L3, Fun, CFun)
		    end;
		error ->
		    io:format("Invalid pen:~p~n",[Pen]),
		    exit({eBadPen, Pen})
	    end;
	{From, {delete, Id}} ->
	    {L1, Free} = L,
	    L2 = swCanvas:delete_obj(Id, L1),
	    L3 = {L2, Free},
	    refresh(Display,Wargs#win.win,L3),
	    reply(From, Free),
	    loop(Display, Wargs, Pens, L3, Fun, CFun);
	{From, id} ->
	    reply(From, {Display, Wargs#win.win}),
	    loop(Display, Wargs, Pens, L, Fun, CFun);
	{'EXIT', Who,Why} ->
	    io:format("swToplevel got Exit from:~p reason:~p~n",
		      [Who,Why]),
	    exit(killed);
	Any ->
	    Wargs1 = sw:generic(Any, Display, Wargs),
	    loop(Display, Wargs1, Pens, L, Fun, CFun)
    end.

refresh(Display, Win, {L,_}) ->
    xDo(Display, xClearArea(Win)),
    foreach(fun({_,Bin}) -> xDo(Display, Bin) end, L),
    xFlush(Display).

    

