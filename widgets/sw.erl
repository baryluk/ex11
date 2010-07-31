-module(sw).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-include("sw.hrl").

-export([sizeInCols2pixels/2, xyInCols2pixels/2,
	 xyInPixels2cols/4, mkWindow/3, mkWindow/4, 
	 xStart/1, generic/3,
	 rpc/2, raised_frame/4, reply/2, sunken_frame/4]).

-import(ex11_lib, [eConfigureWindow/2,
		   eMapWindow/1,
		   ePolyLine/4,
		   eSetInputFocus/3,
		   eUnmapWindow/1,
		   mkPoint/2,
		   xCreateCursor/2, 
		   xColor/2,
		   xCreateGC/2,
		   xCreateWindow/10, 
		   xDo/2, 
		   xEnsureFont/2,
		   xFlush/1,
		   xSetVar/3,
		   xVar/2
		  ]).

-import(lists, [map/2]).


sizeInCols2pixels(W, H) -> {9*W+8,17*H+4}.

xyInCols2pixels(X, Y) -> {9*X-5, 17*Y-4}.

xyInPixels2cols(X, Y, W, H) ->
    XX = (X+5)div 9,
    YY = (Y+16)div 17,
    XX1 = if
	      XX < 1 -> 1;
	      XX > W -> W;
	      true -> XX
	  end,
    YY1 = if
	      YY < 1 -> 1;
	      YY > H -> H;
	      true -> YY
	  end,
    {XX1,YY1}.

xStart(Vsn) ->
    case ex11_lib:xStart(Vsn) of
	{ok, Display} ->
	    init(Display),
	    Display;
	_ ->
	    exit(connection)
    end.

%% mkWindow(Display, Pid, Wargs, Type)

mkWindow(Display, Pid, Wargs, Type) when Type == top ; Type == child ->
    io:format("mkWindow/4 depreciated~n"),
    #win{x=X,y=Y,width=Width,ht=Ht,color=Color,border=Border,cursor=Cursor,
	 mask=Mask, parent=ParentWinId} = Wargs,
    Win = xCreateWindow(Display, ParentWinId, X, Y, Width, Ht, Border, 0, 0,
			   [{backgroundPixel, xColor(Display, Color)},
			    {eventMask, Mask},
			    {backgroundPixmap, 0},
			    {cursor, xCreateCursor(Display, Cursor)},
			    {borderPixmap,0}]),
    Wargs1 = Wargs#win{win=Win},
    case Wargs#win.map of
	true -> xDo(Display, eMapWindow(Win));
	false -> void
    end,
    xFlush(Display),
    Wargs1.

mkWindow(Display, Pid, Wargs) ->
    #win{x=X,y=Y,width=Width,ht=Ht,color=Color,border=Border,cursor=Cursor,
	 mask=Mask, parent=ParentWinId} = Wargs,
    Win = xCreateWindow(Display, ParentWinId, X, Y, Width, Ht, Border, 0, 0,
			   [{backgroundPixel, xColor(Display, Color)},
			    {eventMask, Mask},
			    {backgroundPixmap, 0},
			    {cursor, xCreateCursor(Display, Cursor)},
			    {borderPixmap,0}]),
    Wargs1 = Wargs#win{win=Win},
    case Wargs#win.map of
	true -> xDo(Display, eMapWindow(Win));
	false -> void
    end,
    xFlush(Display),
    Wargs1.
    
init(Display) ->
    default_preferences(Display),
    catch (preferences:user_preferences(Display)),
    make_GCs(Display).

default_preferences(Display) ->
    xSetVar(Display, sysFont, "9x15"),
    xSetVar(Display, sysFontColor, ?DarkBlue),
    xSetVar(Display, buttonHeight, 30),
    xSetVar(Display, darkBlack, ?black),
    xSetVar(Display, weakBlack, ?grey85),
    xSetVar(Display, brightWhite, ?white),
    xSetVar(Display, weakWhite,?snow2),
    xSetVar(Display, buttonColor, ?bisque2),
    xSetVar(Display, entrySliderColor, ?red),
    xSetVar(Display, windowColor, 16#ffffcc),
    xSetVar(Display, entryColor, ?white).

make_GCs(Display) ->
    xSetVar(Display, sysFontId, 
	    xCreateGC(Display, [{function, copy},
				{font, xEnsureFont(Display,
						   xVar(Display,sysFont))},
				{fill_style, solid},
				{foreground, xColor(Display,
					   xVar(Display,sysFontColor))}])),
    xSetVar(Display, '3dShadows',
	    {pen(Display,xVar(Display,brightWhite)),
	     pen(Display,xVar(Display,weakBlack)),
	     pen(Display,xVar(Display,weakWhite)),
	     pen(Display,xVar(Display,darkBlack))}),
    xSetVar(Display, entrySliderGC, 
	    pen(Display,xVar(Display,entrySliderColor))).

pen(Display, Color) ->
    Id = xCreateGC(Display,[{function,copy},
			    {line_width,1},
			    {line_style,solid},
			    {foreground, xColor(Display,Color)}]),
    %% io:format("Colorof ~p = ~p~n",[Id, Color]),
    Id.


%%    Button sun at top left
%%      w = weakWhite
%%      W = brightWhite
%%      b = weakBlack
%%      B = darkblack

%%    wwwwwwwwwwwwwwwwwwwwwwB
%%    wWWWWWWWWWWWWWWWWWWWWbB
%%    wW...................bB
%%    wW...................bB
%%    wW...................bB
%%    wW...................bB
%%    wbbbbbbbbbbbbbbbbbbbbbB
%%    BBBBBBBBBBBBBBBBBBBBBBB

%% sunken frames are used for entries

sunken_frame(Display, D, Width, Ht) ->
    {CW, Cb, Cw, CB} = xVar(Display, '3dShadows'),
    draw_box(D, Width, Ht, CW, Cb, Cw, CB).
    
%% raised frames are used for buttons

raised_frame(Display, D, Width, Ht) ->
    {CW, Cb, Cw, CB} = xVar(Display, '3dShadows'),
    draw_box(D, Width, Ht, CB, Cw, Cb, CW).

draw_box(D, Width, Ht, CB, Cw, Cb, CW) ->
    [line(D, CB, [{0,Ht-1},{Width-1,Ht-1},{Width-1,0}]), %B
     line(D, Cw, [{0,Ht-2},{0,0},{Width-2,0}]),      %w
     line(D, Cb, [{1,Ht-2},{Width-2,Ht-2},{Width-2,1}]), %b
     line(D, CW, [{1,Ht-3},{1,1},{Width-3,1}]) %W
    ].

line(D, Color, Line) ->
    ePolyLine(D, Color, origin,
	      map(fun({X,Y}) -> mkPoint(X, Y) end, Line)).

rpc(Pid, Q) ->
    Pid ! {self(), Q},
    receive
	{Pid, Reply} ->
	    %% io:format("Reply=~p~n",[Reply]),
	    Reply
    end.

reply(Pid, R) ->
    Pid ! {self(), R}.

%%----------------------------------------------------------------------

generic({From, display}, Display, Wargs) ->
    reply(From, Display),
    Wargs;
generic({From, mountPoint}, Display, Wargs) ->
    Win = Wargs#win.win,
    reply(From, Win),
    Wargs;
generic(raise, Display, Wargs) ->
    #win{win=Win} = Wargs,
    xDo(Display, eConfigureWindow(Win, [{stackMode, above}])),
    xFlush(Display),
    Wargs;
generic(setInputFocus, Display, Wargs) ->
    #win{win=Win} = Wargs,
    xDo(Display, eSetInputFocus(none, Win, 0)),
    xFlush(Display),
    Wargs;
generic({setXY,X,Y}, Display, Wargs) ->
    #win{win=Win} = Wargs,
    Wargs1 = Wargs#win{x=X,y=Y},
    xDo(Display, eConfigureWindow(Win, [{x,X},{y,Y}])),
    xFlush(Display),
    Wargs1;
generic({setX,X}, Display, Wargs) ->
    #win{win=Win} = Wargs,
    Wargs1 = Wargs#win{x=X},
    xDo(Display, eConfigureWindow(Win, [{x,X}])),
    xFlush(Display),
    Wargs1;
generic({setY,Y}, Display, Wargs) ->
    #win{win=Win} = Wargs,
    Wargs1 = Wargs#win{y=Y},
    xDo(Display, eConfigureWindow(Win, [{y,Y}])),
    xFlush(Display),
    Wargs1;

generic({setWidthHt,Width,Ht}, Display, Wargs) ->
    #win{win=Win} = Wargs,
    Wargs1 = Wargs#win{width=Width,ht=Ht},
    xDo(Display, eConfigureWindow(Win, [{width,Width},{ht,Ht}])),
    xFlush(Display),
    Wargs1;
generic({up,N}, Display, Wargs) ->
    #win{y=Y,win=Win} = Wargs,
    Y1 = Y - N,
    Wargs1 = Wargs#win{y=Y1},
    xDo(Display, eConfigureWindow(Win, [{y,Y1}])),
    xFlush(Display),
    Wargs1;
generic({down,N}, Display, Wargs) ->
    #win{y=Y,win=Win} = Wargs,
    Y1 = Y + N,
    Wargs1 = Wargs#win{y=Y1},
    xDo(Display, eConfigureWindow(Win, [{y,Y1}])),
    xFlush(Display),
    Wargs1;
generic({left,N}, Display, Wargs) ->
    #win{x=X,win=Win} = Wargs,
    X1 = X - N,
    Wargs1 = Wargs#win{x=X1},
    xDo(Display, eConfigureWindow(Win, [{x,X1}])),
    xFlush(Display),
    Wargs1;
generic({right,N}, Display, Wargs) ->
    #win{x=X,win=Win} = Wargs,
    X1 = X + N,
    Wargs1 = Wargs#win{x=X1},
    xDo(Display, eConfigureWindow(Win, [{x,X1}])),
    xFlush(Display),
    Wargs1;
generic({From, winfo}, Display, Wargs) ->
    reply(From, Wargs),
    Wargs;
generic({wider,N}, Display, Wargs) ->
    #win{width=X,win=Win} = Wargs,
    X1 = X + N,
    Wargs1 = Wargs#win{width=X1},
    xDo(Display, eConfigureWindow(Win, [{width,X1}])),
    xFlush(Display),
    Wargs1;
generic({narrower,N}, Display, Wargs) ->
    #win{width=X,win=Win} = Wargs,
    X1 = X - N,
    Wargs1 = Wargs#win{width=X1},
    xDo(Display, eConfigureWindow(Win, [{width,X1}])),
    xFlush(Display),
    Wargs1;
generic({taller,N}, Display, Wargs) ->
    #win{ht=X,win=Win} = Wargs,
    X1 = X + N,
    Wargs1 = Wargs#win{ht=X1},
    xDo(Display, eConfigureWindow(Win, [{ht,X1}])),
    xFlush(Display),
    Wargs1;
generic({shorter,N}, Display, Wargs) ->
    #win{ht=X,win=Win} = Wargs,
    X1 = X - N,
    Wargs1 = Wargs#win{ht=X1},
    xDo(Display, eConfigureWindow(Win, [{ht,X1}])),
    xFlush(Display),
    Wargs1;
generic(map, Display, Wargs) ->
    #win{ht=X,win=Win} = Wargs,
    xDo(Display, eMapWindow(Win)),
    xFlush(Display),
    Wargs#win{map=true};
generic(unmap, Display, Wargs) ->
    #win{ht=X,win=Win} = Wargs,
    xDo(Display, eUnmapWindow(Win)),
    xFlush(Display),
    Wargs#win{map=false};
generic(Event, Display, Wargs) ->
    io:format("sw:generic skipping:~p~n", [Event]),
    Wargs.



