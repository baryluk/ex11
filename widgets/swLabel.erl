-module(swLabel).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([make/8]).

-include("sw.hrl").

-import(ex11_lib, [ePolyText8/5, rpc/2, sleep/1, 
		   xClearArea/1,
		   xDo/2, xFlush/1,
		   xVar/2]).

make(Parent, X, Y, Width, Ht, Border, Color, Str) -> 
    spawn_link(fun() -> init(Parent,  X, Y, Width, Ht, Border, Color,Str) end).

init(Parent, X, Y, Width, Ht, Border, Color,Str) ->
    Display = rpc(Parent, display),
    PWin = rpc(Parent, mountPoint),
    Wargs = #win{x=X, y=Y, parent=PWin,
		 border=Border,width=Width,ht=Ht,color=Color, 
		 type=label, mask = ?EVENT_EXPOSURE},
    Wargs1 = sw:mkWindow(Display, Parent, Wargs),
    Win = Wargs1#win.win,
    Bin =  ePolyText8(Win, xVar(Display, sysFontId), 10, 18, Str),
    loop(Bin, Display, Wargs1).

loop(B, Display, Wargs) ->
    receive
	{event,_,expose, _} ->
	    xDo(Display, B),
	    xFlush(Display),
	    loop(B, Display, Wargs);
	{set, Str} ->
	    Win = Wargs#win.win, 
	    xDo(Display, xClearArea(Win)),
	    Bin =  ePolyText8(Win, xVar(Display, sysFontId), 10, 18, Str),
	    self() ! {event,void,expose,void},
	    loop(Bin, Display, Wargs);
	{'EXIT', Pid, Why} ->
	    true;
	Any ->
	    %% Now we call the generic operators 
	    Wargs1 = sw:generic(Any, Display, Wargs),
	    loop(B, Display, Wargs1)
    end.



