-module(swRectangle).

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

make(Parent, X, Y, Width, Ht, Border, Color) ->
    spawn_link(fun() -> init(Parent,  X, Y, Width, Ht, Border, Color) end).

init(Parent, X, Y, Width, Ht, Border, Color) ->
    Display = rpc(Parent, display),
    PWin = rpc(Parent, mountPoint),
    Wargs = #win{x=X, y=Y, parent=PWin,
		 border=Border,width=Width,ht=Ht,color=Color, 
		 type=rectangle},
    Wargs1 = sw:mkWindow(Display, Parent, Wargs),
    Win = Wargs1#win.win,
    loop(Display, Wargs1).

loop(Display, Wargs) ->
    receive
	Any ->
	    %% Now we call the generic operators
	    Wargs1 = sw:generic(Any, Display, Wargs),
	    loop(Display, Wargs1)
    end.

