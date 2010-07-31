-module(swScrollbar).

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
      fun() -> 
	      init(Parent, X, Y, Width, Ht, Border, Color1, Color2)
      end).

init(Parent, X0, Y0, Width, Ht, Border, Color1, Color2) ->
    Display = rpc(Parent, display),
    Attach = rpc(Parent, mountPoint),
    Wargs = #win{x=X0, y=Y0, border=Border,width=Width,ht=Ht,color=Color1, 
		 type=scrollbar, parent=Attach, 
		 mask= ?EVENT_BUTTON_PRESS %% <- do not remove
		 bor  ?EVENT_BUTTON1_MOTION},
    Wargs1 = sw:mkWindow(Display, Parent, Wargs),
    ParentWid = Wargs1#win.win,
    Dir = if 
	      Width > Ht -> horizontal;
	      true -> vertical
	  end,
    Size = case Dir of
		horizontal -> Ht - 4;
		vertical   -> Width - 4
	    end,
    OutVal = fun(X, Y) ->
		     outputVal(Dir, X, Y, X0, Y0, Width, Ht)
	     end,
    CursorPos = fun(Val) ->
			cursorPos(Dir, Val, X0, Y0, Width, Ht)
		end,
    %% Make a little rectangle
    Rect = xCreateWindow(Display, ParentWid, 2, 2, Size, Size, 0, 0, 0,
			 [{backgroundPixel, xColor(Display, Color2)},
			  {backgroundPixmap, 0},
			  {cursor, xCreateCursor(Display, ?XC_hand1)},
			  {borderPixmap,0}]),
    xDo(Display, eMapWindow(Rect)),  
    xFlush(Display),
    loop(Display, Rect, fun(_) -> void end, OutVal, CursorPos, Wargs1).


loop(Display, Rect, Fun, OutVal, CursorPos, Wargs) ->
    receive
	{event,_,Tag,{_,X,Y,_,_}} when Tag == buttonPress;
				       Tag == motionNotify ->
	    %% The output value of a horizontal slider
	    %% is defined to be an integer in the
	    %% range 0..Width
	    %% and for a vertical slider in 0..Ht
	    Val = OutVal(X, Y),
	    Fun(Val),
	    Pos = CursorPos(Val),
	    xDo(Display, eConfigureWindow(Rect, [Pos])),
      	    xFlush(Display),
	    loop(Display, Rect, Fun, OutVal, CursorPos, Wargs);
	{onMove, Fun1} ->
	    loop(Display, Rect, Fun1, OutVal, CursorPos, Wargs);
	{'EXIT', Pid, Why} ->
	    exit(died);
	Any ->
	    %% Now we call the generic operators
	    Wargs1 = sw:generic(Any, Display, Wargs),
	    loop(Display, Rect, Fun, OutVal, CursorPos, Wargs1)
    end.

outputVal(horizontal, X, Y, X0, Y0, Width, Ht) ->
    if
	X >  0, X < Width -> X;
	X >= Width -> Width;
	X =< 0 -> 0
    end;
outputVal(vertical, X, Y, X0, Y0, Width, Ht) ->
    if
	Y >  0, Y < Ht -> Y;
	Y >= Ht -> Ht;
	Y =< 0  -> 0
    end.

%% cursorPos we want a polynomial that maps 
%%    0     -> 2
%%    Width -> Width - Ht - 2.
%% 
%%    Pos = A*X + B
%%    2 = A*0 + B
%%    Width-Ht = A*Width
%%    --------------------
%%    Thus B = 2
%%    and  A = Width-Ht/Width

cursorPos(horizontal, X, X0, Y0, Width, Ht) ->
    A = (Width-Ht)/Width,
    B = 2,
    Pos = trunc(A*X + B),
    {x,Pos};
cursorPos(vertical, Y, X0, Y0, Width, Ht) ->
    A = (Ht-Width)/Ht,
    B = 2,
    Pos = trunc(A*Y + B),
    {y,Pos}.
