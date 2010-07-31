-module(swLifts).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([make/0, start/0]).

-include("sw.hrl").

-import(swCanvas, [newPen/4, draw/3]).
-import(sw, [mkTopLevel/4,xStart/1,rpc/2]).
-import(lists, [foldl/3,map/2,reverse/1,seq/2]).

-define(bg, 16#ffffcc).

%% start returns after eveything is ready

start() ->
    make().

make() ->
    S = self(),
    Pid =  spawn_link(fun() -> win(S) end),
    receive
	{Pid, ack} ->
	    Pid
    end.

win(Parent) ->
    Display = xStart("3.2"),
    Win0    = swTopLevel:make(Display, 645, 710, ?bg),
    Canvas  = swCanvas:make(Win0, 10,10,625,690,1,?LightCyan),
    Canvas ! {onClick,fun({_,X,Y,_,_})->io:format("Click ~p,~p~n",[X,Y]) end},
    newPen(Canvas, black, ?black,2),
    draw(Canvas, black, {text,10,10,"Lift Simulator"}),
    Nfloors = 6,
    Lifts = mkLifts(Canvas, 3, Nfloors),
    Width  = swScrollbar:make(Canvas, 10, 980, 60, 20, 1,  ?grey90, ?red),
    Pos    = swScrollbar:make(Canvas, 730, 10, 20, 513, 1, ?grey90, ?red),
    Parent ! {self(), ack},
    Event = fun(Any) -> io:format("swLifts received:~p~n",[Any]) end,
    loop(Event, Lifts).

loop(Event, Lifts) ->
    receive
	{setPos,I,N} ->
	    %% set lift I to position N
	    {Lhs, Rhs, _, _} = element(I, Lifts),
	    setLift(Lhs, Rhs, N),
	    loop(Event, Lifts);
	{setDoor,I,N} ->
	    %% set lift I to doors open N%
	    {Lhs, Rhs, _, _} = element(I, Lifts),
	    setDoor(Lhs, Rhs, N),
	    loop(Event, Lifts);
	{setLiftLamp, Lift, Floor, Color} ->
	    {_,_,T,_} = element(Lift, Lifts),
	    B = element(Floor, T),
	    B ! {set, Color},
	    loop(Event, Lifts);
	{setFloorLamp, Lift, Floor, Color} ->
	    {_,_,_,T} = element(Lift, Lifts),
	    B = element(Floor, T),
	    B ! {set, Color},
	    loop(Event, Lifts);
	{onClick, F} ->
	    loop(F, Lifts);
	Any ->
	    Event(Any),
	    loop(Event, Lifts)	    
    end.

setLift(Lhs, Rhs, N) ->
    %% Y = A*N + B
    Min = 36, Max = 546,
    A = (Max-Min)/100,
    B = Min,
    Y = trunc(A*N + B),
    Lhs ! {setY, Y},
    Rhs ! {setY, Y}.

setDoor(Lhs, Rhs, N) ->
    %% Min = 3, Max=60
    Min = 60, Max = 3,
    A = (Max-Min)/100,
    B = Min,
    W = trunc(A*N + B),
    %% io:format("SetDoor N=~p W=~p~n",[N,W]),
    Lhs ! {setW, W},
    Rhs ! {setW, W},
    Rhs ! {setX, 60-W}.

unselected() ->
     ?grey84.

mkLifts(Canvas, NLifts, Nfloors) ->
    Up = ?MistyRose,
    Down = ?LightGoldenrod1,
    S = self(),
    %% top lift button
    Bt = swButton:make(Canvas, 40,70,25,25,Down,"D"),
    mkEvent({click, floor, Nfloors, down}, Bt),
    %% middle lift buttons
    for(fun(I) ->
		XX = 40,
		YY = (I-1)*100 + 70,
		B1 = swButton:make(Canvas, 10,YY,25,25,Up,"U"),
		B2 = swButton:make(Canvas, 40,YY,25,25,Down,"D"),
		mkEvent({click, floor, Nfloors-I+1, up}, B1),
		mkEvent({click, floor, Nfloors-I+1, down}, B2)
	end, 2,Nfloors-1),
    %% lower lift buttons
    Bl = swButton:make(Canvas, 10,570,25,25,Up,"U"),
    mkEvent({click, floor, 1, up}, Bl),
    %% the lifts
    L = map(fun(I) -> mklift(I, Canvas, Nfloors) end, seq(1, NLifts)),
    list_to_tuple(L).

mklift(I, Canvas, Nfloors) ->
    W = 60,   % width of half of lift
    XX = 100 + (I-1) * (2*W + 60),
    YLift = y_lift_stop(1),
    %% outer rectangle
    draw(Canvas, black,   {rectangle, XX, 10, W*2+20, Nfloors*(W + 40)}),
    %% floor lines
    for(fun(J) ->
		Y = J * (W+40),
		draw(Canvas, black, {line, XX, Y, XX+2*W+20, Y})
	end, 1, Nfloors-1),
    %% status buttons
    FloorLampsL = map(fun(J) ->
			     Y = J * (W+40) - 22 ,
			     XX1 = XX - 20,
			     swColorButton:make(Canvas,XX1,Y,15,15,1,
						unselected())
		     end, seq(1, Nfloors)),
    FloorLamps = list_to_tuple(reverse(FloorLampsL)),
    Win = Canvas,
    %% the lifts
    Lhs    = swProgressBar:make(Win, XX+10, YLift, W, W, 0, ?white, ?grey46),
    Rhs    = swProgressBar:make(Win, XX + 10+ W,YLift, W, W, 0,?white,?grey46),
    %% lift call buttons
    YY1 = Nfloors*(W+40) + 15,
    S = self(),
    LiftLampsL = map(fun(J) ->
			      XX1 = XX + (J-1)*26 - 9,
			      B1 = swButton:make(Win, XX1, YY1, 25, 25, 
						 ?honeydew2, [J+$0]),
			      mkEvent({click,lift,I,J}, B1),
			      swColorButton:make(Win, XX1, YY1+30, 
						 25, 25,1,
						 unselected())
		      end, seq(1, Nfloors)),
    LiftLamps = list_to_tuple(LiftLampsL),
    {Lhs,Rhs, LiftLamps, FloorLamps}.

mkEvent(Event, Widget) ->
    S = self(),
    Widget ! {onClick, fun(_) -> S ! Event end}.
			       

y_lift_stop(I) ->
    W = 60,
    I*(W+40) - 65.

for(F, Max, Max) -> F(Max);
for(F, I, Max)  -> F(I), for(F, I+1,Max).
		   










