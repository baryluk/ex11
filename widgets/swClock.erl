-module(swClock).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% Started 2004-02-28 by joe@sics.se joe armstrong

-export([make/6]).

-include("sw.hrl").

-import(ex11_lib, [ePolyText8/5, rpc/2, sleep/1, 
		   xClearArea/1,
		   eCopyArea/9,
		   ePolyFillRectangle/3,
		   ePolyLine/4,
		   mkPoint/2,
		   mkRectangle/4,
		   xClearArea/1,
		   xColor/2,
		   xCreateGC/2,
		   xCreatePixmap/4,
		   xDo/2, xFlush/1,
		   xVar/2]).

make(Parent, X, Y, Size, Border, Color) -> 
    spawn_link(fun() -> init(Parent,  X, Y, Size, Border, Color) end).

init(Parent, X, Y, Size, Border, Color) ->
    Display = rpc(Parent, display),
    PWin = rpc(Parent, mountPoint),
    Wargs = #win{x=X, y=Y, parent=PWin,
		 border=Border,width=Size,ht=Size,color=Color, 
		 type=label, mask = ?EVENT_EXPOSURE},
    Wargs1 = sw:mkWindow(Display, Parent, Wargs),
    Win = Wargs1#win.win,
    %% Make a pixmap with the clock face in it
    ClockFace =  xCreatePixmap(Display, Win, Size, Size),
    Pen0 = xCreateGC(Display, [{function,copy},
			      {line_width,1},
			      {line_style,solid},
			      {graphics_exposures, false},
			      {foreground, xColor(Display, ?white)}]),
    xDo(Display, ePolyFillRectangle(ClockFace, Pen0,
				    [mkRectangle(0,0,Size, Size)])),
    Pen1 = xCreateGC(Display, [{function,copy},
			      {line_width,1},
			      {line_style,solid},
			      {graphics_exposures, false},
			      {foreground, xColor(Display, ?black)}]),
    Text = xVar(Display, sysFontId),
    draw_clock_face({Display, ClockFace, Pen1, Text}, Size),

    %% Note the GC used in setCursor has graphics_exposurs set to false 
    %% this is to turn of update events caused then the Cursor pixmap writes
    %% onto the drawable - I *know* the underlying text is OK :-)
    GC = xCreateGC(Display, [{function,copy},
			     {line_width,2},
			     {line_style,solid},
			     {graphics_exposures, false},
			     {foreground, xColor(Display, Color)}]),
    Bin = eCopyArea(ClockFace,Win,GC,0,0,0,0,Size, Size),
    F = fun() ->
		update_hands(Display, Win, Pen1, Size, Bin)
	end,
    F(),
    loop(Display, Wargs1, Win, F).


%% Update hands does the following:
%%   1) Read the current time
%%   2) Copy the face bitmap to the top rectangle
%%      this will have the effect or erasing the hands
%%   3) Draw the hour minue and second ticks

update_hands(Display, Win, Pen, Size, Bin) ->
    {Hour, Min, Sec} = time(),
    %% Hour is 1..24
    Hour1 = if 
		Hour > 12 ->
		    Hour - 12;
		true ->
		    Hour
	    end,
    Hour2 = Hour1 + Min/60,
    Min1  = Min + Sec/60,
    Xc = Yc = Size div 2,
    R0 = trunc(Size*0.1),
    R1 = trunc(Size*0.3),
    R3 = trunc(Size*0.7),
    %% draw the background face
    xDo(Display, Bin),
    %% draw the hands
    draw_hand(Display, Win, Pen, Hour2*30, Size, 0, trunc(0.2 * Size)),
    draw_hand(Display, Win, Pen, Min1*6, Size, 0, trunc(0.3 * Size)),
    draw_hand(Display, Win, Pen, Sec*6, Size, 0,trunc(0.4*Size)),
    xFlush(Display).

draw_hand(Display, Win, Pen, Angle, Size, R1, R2) ->
    A = Angle * 3.14159/180,
    M = Size div 2,
    X1 = trunc(M + R1 * (S=math:sin(A))),
    Y1 = trunc(M - R1 * (C=math:cos(A))),
    X2 = trunc(M + R2 * S ),
    Y2 = trunc(M - R2 * C),
    xDo(Display,
	ePolyLine(Win, Pen, origin, [mkPoint(X1,Y1), mkPoint(X2,Y2)])).


draw_clock_face(D, Size) ->
    Xc = Yc = Size div 2,
    R1 = trunc(0.7*Xc),
    R2 = trunc(0.88*Xc),
    R3 = trunc(0.8*Xc),
    draw_face(D, Xc, Yc, R1, R2, R3, Size).

%% canvas X increase across left to right
%%        Y increases down

draw_face(C, Xc, Yc, R1, R2, R3, Size) ->
    for(1, 12, fun(I) -> draw_5_min_marks(C, Xc, Yc, I, R1, R3, Size) end),
    for(1, 60, fun(I) -> draw_1_min_marks(C, Xc, Yc, I, R2, R3) end).

draw_5_min_marks(D, Xc, Yc, I, R1, R2, Size) ->
    A = I * 30 * 3.14159/180,
    X1 = trunc(Xc + R1 * (S=math:sin(A))),
    Y1 = trunc(Yc - R1 * (C=math:cos(A))),
    X2 = trunc(Xc + R2 * S ),
    Y2 = trunc(Yc - R2 * C),
    draw(D, {line,X1,Y1,X2,Y2}),
    %% Only draw the numbers if the size is bigger than 200
    if 
	Size >= 200 -> 
	    X3 = trunc(Xc + R2*1.15*S) - 7,
	    Y3 = trunc(Yc - R2*1.15*C) + 4, 
	    draw(D, {text,X3,Y3,integer_to_list(I)});
	true ->
	    void
    end.

draw_1_min_marks(D, Xc, Yc, I, R1, R2) ->
    A = I * 6 * 3.14159/180,
    X1 = trunc(Xc + R1 * (S=math:sin(A))),
    Y1 = trunc(Yc - R1 * (C=math:cos(A))),
    X2 = trunc(Xc + R2 * S ),
    Y2 = trunc(Yc - R2 * C),
    draw(D, {line,X1,Y1,X2,Y2}).

draw({Display,Face,Pen, _}, {line, X1, Y1, X2, Y2}) ->
    xDo(Display,
	ePolyLine(Face, Pen, origin, [mkPoint(X1,Y1), mkPoint(X2,Y2)]));
draw({Display,Face,_,Text}, {text, X, Y, Str}) ->
    xDo(Display, ePolyText8(Face, Text, X,Y, Str)).

for(I, J, F) when I > J -> true;
for(I, J, F) ->
    F(I),
    for(I+1,J,F).

loop(Display, Wargs, Win, F) ->
    receive
	{event,_,expose, _} ->
	    F(),
	    loop(Display, Wargs, Win, F);
	{'EXIT', Pid, Why} ->
	    true;
	Any ->
	    %% Now we call the generic operators 
	    Wargs1 = sw:generic(Any, Display, Wargs),
	    loop(Display, Wargs1, Win, F)
	after 1000 ->
		F(),
		loop(Display, Wargs, Win, F)
    end.

    
