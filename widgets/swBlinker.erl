-module(swBlinker).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([ensure_blinker/1, blink/4]).

-include("sw.hrl").

-import(ex11_lib, [
		   eCopyArea/9,
		   eFreeGC/1,
		   ePolyFillRectangle/3,
		   mkRectangle/4,
		   xColor/2,
		   xCreateGC/2,
		   xCreatePixmap/4,
		   xDo/2,
		   xFlush/1,
		   xGetVar/2,
		   xSetVar/3,
		   xVar/2]).

-define(CursorWidth, 2).
-define(CursorHt, 14).
-define(EntryColor, ?white).
-define(BlinkTime, 800).
-define(TextColor, ?DarkBlue).
    
%%------------------------------------------------------------------------
%% The blinking cursor ... :-)

ensure_blinker(Display) ->
    case xGetVar(Display, blinker) of
	{ok, Pid} ->
	    Pid;
	error ->
	    Pid = spawn(fun() -> blinker_start(Display) end),
	    xSetVar(Display, blinker, Pid)
    end.

blink(Display, Win, X, Y) ->
    case xGetVar(Display, blinker) of
	{ok, Pid} ->
	    Pid ! {blink, Win, X, Y};
	error ->
	    Pid = spawn(fun() -> blinker_start(Display) end),
	    xSetVar(Display, blinker, Pid),
	    Pid ! {blink, Win, X, Y}
    end.

blinker_start(Display) ->
    %% create a Pixmap to draw the cursor
    %% this is filled rectangle containing the text color
    %% Win = xVar(Display, gcParent),
    Win = xVar(Display, defaultWindow),
    DrawCursor = xCreatePixmap(Display, Win, ?CursorWidth, ?CursorHt),
    GC0 = xCreateGC(Display, [{function,copy},
			      {line_width,2},
			      {line_style,solid},
			      {graphics_exposures, false},
			      {foreground, xColor(Display, ?TextColor)}]),
    xDo(Display, 
	ePolyFillRectangle(DrawCursor, GC0,
			   [mkRectangle(0,0,?CursorWidth, ?CursorHt)])),
    %% create a PixMap to erase the cursor
    %% this is a filled rectangle containing the background color of
    %% the entry
    ClearCursor = xCreatePixmap(Display, Win, ?CursorWidth, ?CursorHt),
    GC1 = xCreateGC(Display, [{function,copy},
			      {line_width,2},
			      {line_style,solid},
			      {graphics_exposures, false},
			      {foreground, xColor(Display, ?EntryColor)}]),
    xDo(Display, ePolyFillRectangle(ClearCursor, GC1,
				    [mkRectangle(0,0,?CursorWidth, ?CursorHt)])),
     %% free the GC
    xDo(Display, eFreeGC(GC1)),
    xFlush(Display),
    blinker(Display, DrawCursor, ClearCursor, GC0).

blinker(Display, On, Off, GC0) ->
    receive
	{blink, Win, X, Y} ->
	    setCursor(Display, Win, On, X, Y, GC0),
	    blinking(Display, Win, On, Off, true, X, Y, GC0)
    end.

blinking(Display, Win, On, Off, State, X, Y, GC0) ->
    receive
	{blink, Win1, X1, Y1} ->
	    case State of
		true  -> setCursor(Display, Win, Off, X, Y, GC0);
		false -> void
	    end,
	    setCursor(Display, Win1, On, X1, Y1, GC0),
	    blinking(Display, Win1, On, Off, true, X1, Y1, GC0)
    after ?BlinkTime ->
	    case State of
		true -> setCursor(Display, Win, Off, X, Y, GC0);
		false -> setCursor(Display, Win, On, X, Y, GC0)
	    end,
	    blinking(Display, Win, On, Off, not State, X, Y, GC0)
    end.


%% Note the GC used in setCursor has graphics_exposurs set to false 
%% this is to turn of update events caused then the Cursor pixmap writes
%% onto the drawable - I *know* the underlying text is OK :-)

setCursor(Display, Win, PixMap, X, Y, GC) ->
    xDo(Display, eCopyArea(PixMap,Win,GC,0,0,X,Y,?CursorWidth, ?CursorHt)),
    xFlush(Display).

