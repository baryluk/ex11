-module(swProgressBar).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([make/8]).

-include("sw.hrl").

-import(ex11_lib, [eConfigureWindow/2,
		   eMapWindow/1,
		   ePolyText8/5, rpc/2, sleep/1, 
		   xColor/2,
		   xClearArea/1,
		   xCreateCursor/2,
		   xCreateWindow/10,
		   xDo/2, xFlush/1,
		   xVar/2]).

make(Parent, X, Y, Width, Ht, Border, Color1, Color2) -> 
    spawn_link(
      fun() -> init(Parent, X, Y, Width, Ht, Border, Color1, Color2) end).

init(Parent, X, Y, Width, Ht, Border, Color1, Color2) ->
    %% make a top frame
    Display = rpc(Parent, display),
    Attach = rpc(Parent, mountPoint),
    Wargs = #win{x=X, y=Y, border=Border,width=Width,ht=Ht,color=Color1, 
		 type=progressBar, parent=Attach},
    Wargs1 = sw:mkWindow(Display, Parent, Wargs),
    Dir = if 
	      Width > Ht -> horizontal;
	      true -> vertical
	  end,
    Size = lists:min([Width,Ht]) - 4,
    %% Make a little rectangle
    ParentWin = Wargs1#win.win,
    Rect = xCreateWindow(Display, ParentWin, 2, 2, Size, Size, 0, 0, 0,
			 [{backgroundPixel, xColor(Display, Color2)},
			  {backgroundPixmap, 0},
			  {cursor, xCreateCursor(Display, ?XC_hand1)},
			  {borderPixmap,0}]),
    xDo(Display, eMapWindow(Rect)),  
    xFlush(Display),
    loop(Display, Rect, Wargs1).


loop(Display, Rect, Wargs) ->
    receive
	{setW, W} ->
	    xDo(Display, eConfigureWindow(Rect, [{width,W}])),
      	    xFlush(Display),
	    loop(Display, Rect, Wargs);
	{setX, X} ->
	    xDo(Display, eConfigureWindow(Rect, [{x,X}])),
      	    xFlush(Display),
	    loop(Display, Rect, Wargs);
	X ->
	    Wargs1 = sw:generic(X, Display, Wargs),
	    loop(Display, Rect, Wargs)
    end.

