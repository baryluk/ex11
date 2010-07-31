-module(swErlPoint).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([make/7]).

%% Protocol
%%   C !  {blink, X, Y}
%%   C !  {setSize, Width, Ht}
%%   C !  {mkPen, Name, Fg, Bg}
%%   C !  {display, X, Y, PenName, String}
%%   C !! read -> [{X,Y,Pen,Str}]

-include("sw.hrl").

-compile(export_all).

-import(ex11_lib, [eImageText8/5,
		   ePolyFillRectangle/3,
		   ePolyText8/5, 
		   mkRectangle/4,
		   rpc/2, sleep/1, 
		   xClearArea/1,
		   xCreateGC/2,
		   xCreateGC/3,
		   xColor/2,
		   xDo/2, 
		   xEnsureFont/2,
		   xFlush/1,
		   xVar/2]).

-import(lists, [foldl/3, foreach/2, reverse/1]).

make(Parent, X, Y, W, H, Border, Color) -> 
    spawn_link(fun() -> init(Parent,  X, Y, W, H, Border, Color) end).

init(Parent, X, Y, Width, Ht, Border, Color) ->
    Display = rpc(Parent, display),
    PWin = rpc(Parent, mountPoint),
    Wargs = #win{x=X, y=Y, parent=PWin,
		 border=Border,width=Width,ht=Ht,color=Color, 
		 type=label, mask = ?EVENT_EXPOSURE bor ?EVENT_BUTTON_PRESS
		 bor ?EVENT_KEY_PRESS},
    Wargs1 = sw:mkWindow(Display, Parent, Wargs),
    Win = Wargs1#win.win,
    Pen0 = mkPen(Display, Win, 1, Color),
    loop(Display, Wargs1, [], Pen0, dict:new()).

mkFace(Display, FontName, Fg, Bg) ->
    Font   = xEnsureFont(Display, FontName),
    xCreateGC(Display,  [{font, Font},
			 {background, xColor(Display, Bg)},
			 {foreground, xColor(Display, Fg)}]).


mkFace(Display, FontName, Fg) ->
    io:format("swErlPoint:mkFace called ~s ~p~n",[FontName, Fg]),
    Font = xEnsureFont(Display, FontName),
    io:format("swErlPoint Font=~p~n",[Font]),
    xCreateGC(Display,  [{font, Font},
			 {foreground, xColor(Display, Fg)}]).

mkPen(Display, Win, Width, Color) ->
    xCreateGC(Display, Win,
	      [{function,copy},
	       {line_width,Width},
	       {line_style,solid},
	       {foreground, xColor(Display, Color)}]).

loop(Display, Wargs, State, Pen0, Env0) ->
    receive
	{event,_,expose, _} ->
	    Env1 = display(Display, Wargs, State, Pen0, Env0),
	    loop(Display, Wargs, State, Pen0, Env1);
	{'EXIT', Pid, Why} ->
	    true;
	{display, State1} ->
	    self() ! {event,void,expose,void},
	    loop(Display, Wargs, State1, Pen0, Env0);
	Any ->
	    %% Now we call the generic operators 
	    Wargs1 = sw:generic(Any, Display, Wargs),
	    loop(Display, Wargs, State, Pen0, Env0)
    end.

display(Display, Wargs, State, Pen0, Env0) ->
    io:format("State=~p~n",[State]),
    #win{win=Win, width=Width,ht=Ht} = Wargs,
    %% clear the window
    xDo(Display, ePolyFillRectangle(Win, Pen0,
				    [mkRectangle(0,0, Width, Ht)])),
    E1 = foldl(fun(I, E) -> 
		       io:format("I=~p~n",[I]),
		       do(I, Display, Wargs, Win, E) end, Env0, State),
    xFlush(Display),
    E1.
    

do({face,Name,Color,FontName}, Display, Wargs, Win, E) ->
    case dict:find({face,Name}, E) of
	{ok, {_, Color, FontName}} -> 
	    E;
	_ ->
	    GC = mkFace(Display, FontName, Color),
	    dict:store({face,Name}, {GC, Color, FontName} , E)
    end;
do({pen,Name,Width,Color}, Display, Wargs, Win, E) ->
    case dict:find({pen,Name}, E) of
	{ok, {_,Width, Color}} ->
	    E;
	_  ->
	    GC = mkPen(Display, Win, Width, Color),
	    dict:store({pen,Name}, {GC, Width, Color}, E)
    end;
do({text,Face,X,Y,Str}, Display, Wargs, Win, E) ->
    case dict:find({face,Face}, E) of
	{ok, {GC,_,_}} ->
	    xDo(Display, ePolyText8(Win, GC, X, Y, Str));
	error ->
	    io:format("Missing face:~p~n",[Face])
    end,
    E;
do({rectangle,PenName,X,Y,Width,Ht}, Display, Wargs, Win, E) ->
    case dict:find({pen,PenName}, E) of
	{ok, {GC,_,_}} ->
	    B = ePolyFillRectangle(Win, GC,
				   [mkRectangle(X,Y,Width,Ht)]),
	    xDo(Display, B);
	error ->
	    io:format("Missing pen:~p~n",[PenName])
    end,
    E;
do(X,_,_,_,E) ->
    io:format("swErlPoint:skipping:~p~n",[X]),
    E.







