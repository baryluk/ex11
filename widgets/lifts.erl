-module(lifts).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-include("sw.hrl").

-compile(export_all).

-import(lists, [foreach/2, map/2,seq/2,sort/1]).

start() ->
    spawn(fun() -> go() end).

go() ->
    %% Hard_wired 3x6 lifts
    Pid = swLifts:make(),
    io:format("we're off~n"),
    %% and 6 floor processes
    Floors = map(fun(I) -> start_floor(I, Pid) end, seq(1,6)),
    Lifts  = map(fun(I) -> start_lift(I, Pid) end, seq(1,3)),
    broadcast(Floors, {lifts, Lifts}),
    F = list_to_tuple(Floors),
    L = list_to_tuple(Lifts),
    Pid ! {onClick, fun(X) -> handle_click(X, F, L) end},
    loop(Pid).

handle_click({click, floor, N, Dir}, Floors, _) ->
    element(N, Floors) ! {click, Dir};
handle_click({click, lift, N, But}, _, Lifts) ->
    element(N, Lifts) ! {click, But}.

loop(Pid) ->
    receive
	Any ->
	    void
    end,
    loop(Pid).

start_floor(I, Pid) ->
    spawn_link(fun() ->
		       %% starting floor I 
		       Pid ! {setFloorLamp, 1, I, ?white},
		       Pid ! {setFloorLamp, 2, I, ?white},
		       Pid ! {setFloorLamp, 3, I, ?white},
		       floor(I, Pid)
	       end).

floor(I, Pid) ->
    receive
	{lifts, Lifts} ->
	    floor(I, Lifts, Pid)
    end.


start_lift(I, Pid) ->
    spawn_link(fun() ->
		       %% starting lift I 
		       for(fun(F) ->
				   Pid ! {setLiftLamp, I, F, ?white},
				   {Door, _, Pos, Stop} = lift(I),
				   foreach(fun(J) ->
					      Pid ! {setLiftLamp,I,J,?red},
					      Pid ! {setFloorLamp,I,J,?red}
					   end, Stop),
				   Pid ! {setPos, I,  Pos},
				   Pid ! {setDoor, I, door_width(Door)}
			   
			   end, 1, 6),
		       lift(I, lift(I), Pid)
	       end).

%% The lift state
%% {Doors, Motion, Where, StopList}
%%   Doors = {open,T} the doors have been open for time T
%%         | closed
%%         | {opening, N} the doors  are opening and are N% open
%%         | {closing, N}  the doors are closing and are N% open
%%   Motion = {up, Goal} | {down, Goal} | stopped
%%         Goal is the next target that we are moving towards
%%         This is also in the stop list
%%   StopList = [N] = floors where the lift is to go to and stop
%%         This is ordered ...

lift(1) -> {{closing,10}, {up, 3}, y_floor(1), [3,4,5]}; 
lift(2) -> {closed, stopped, y_floor(2), [3,6,4]};
lift(3) -> {closed, stopped, y_floor(6), [4]}.
    
door_width(open)   -> 100;
door_width(closed) -> 0;
door_width({_,N})  -> N.

y_floor(1) -> 100;
y_floor(2) -> 78;
y_floor(3) -> 59;
y_floor(4) -> 39;
y_floor(5) -> 19;
y_floor(6) -> 0.

broadcast(Pids, M) -> foreach(fun(I) -> I ! M end, Pids).

lift(I, State, Pid) ->
    receive
	{stopAt, K} ->
	    State1 = stopAt(I, K, State, Pid),
	    lift(I, State1, Pid);
	{From, how_long, N, Dir} ->
	    Time = how_long(N, Dir, State),
	    From ! {self(), Time},
	    lift(I, State, Pid);
	{click, N} ->
	    self() ! {stopAt, N},
	    lift(I, State, Pid);
	Any ->
	    io:format("lift:~p  received:~p~n",[I, Any]),
	    lift(I, State, Pid)
    after 100 ->
	    {Doors, Move, Pos, Stop} = State,
	    Doors1 = move_doors(Doors, I, Pid),
	    %% if Doors1 = closed we can move off
	    case Doors1 of
		closed ->
		    State1 = move_lift(Move, I, Pos, Stop, Pid),
		    lift(I, State1, Pid);
		_ ->
		    lift(I, {Doors1, Move, Pos, Stop}, Pid)
	    end
    end.
		    
move_doors({closing, N}, I, Pid) when N < 0 ->
    Pid ! {setDoor, I, 0},
    closed;
move_doors({closing, W}, I, Pid) ->
    Pid ! {setDoor, I, W-4},
    {closing, W-4};
move_doors({opening, N}, I, Pid) when N > 100 ->
    Pid ! {setDoor, I, 100},
    {opened, 0};
move_doors({opening, W}, I, Pid) ->
    Pid ! {setDoor, I, W+4},
    {opening, W+4};
move_doors({opened, 8}, I, Pid) ->
    {closing, 100};
move_doors({opened, N}, I, Pid) ->
    {opened, N+1};
move_doors(closed, I, Pid) ->
    closed.

move_lift(Move={Dir, N}, I, Pos, Stop, Pid) ->
    DestY = y_floor(N),
    if
	Pos > DestY ->
	    Pid ! {setPos, I, Pos-1},
	    {closed, Move, Pos-1, Stop};
	Pos < DestY ->
	    Pid ! {setPos, I, Pos+1},
	    {closed, Move, Pos+1, Stop};
	true ->
	    %% we have arrived at floor N
	    Pid ! {setFloorLamp, I, N, ?white},
	    Pid ! {setLiftLamp, I, N, ?white},
	    Stop1 = delete(N, Stop),
	    {{opening,0}, stopped, Pos, Stop1}
    end;
move_lift(stopped, I, Pos, [], Pid) ->
    {closed, stopped, Pos, []};
move_lift(stopped, I, Pos, [K|T], Pid) ->
    DestY = y_floor(K),
    if 
	Pos > DestY ->
	    {closed, {up,K}, Pos, T};
	true  ->
	    {closed, {down,K}, Pos, T}
    end;
move_lift(Move, I, Pos, Stop, Pid) ->
    {closed, Move, Pos, Stop}.

floor(N, Lifts, Pid) ->
    receive
	{click, Dir} ->
	    reserve_lift(N, Lifts, Dir),
	    floor(N, Lifts, Pid);
	Any ->
	    io:format("floor:~p  received:~p~n",[N, Any]),
	    floor(N, Lifts, Pid)
    end.

reserve_lift(N, Lifts, Dir) ->
    broadcast(Lifts, {self(), how_long, N, Dir}),
    Replies = map(fun(Pid) -> receive {Pid,R} -> {R,Pid} end end, Lifts),
    phase2(N, Replies).

phase2(N, L) ->
    [{Time,Pid}|T] = sort(L),
    Chosen = choose_lift(T, Time, [Pid]),
    Chosen ! {stopAt, N}.

choose_lift([{T1,P1}|T], Time, Ok) when T1 =< Time ->
    choose_lift(T, Time, [P1|Ok]);
choose_lift(_, Time, [Pid]) ->
    Pid;
choose_lift(_, Time, L) ->
    N = random:uniform(length(L)),
    lists:nth(N, L).

for(F, Max, Max) -> F(Max);
for(F, I, Max)   -> F(I), for(F, I+1,Max).

delete(I, [I|T]) -> T;
delete(I, [H|T]) -> [H|delete(I,T)];
delete(I, [])    -> [].

how_long(N, Dir, {Door, stopped, Pos, []}) ->
    Td = time_to_close_door(Door),
    Pos1 = y_floor(N),
    Diff = abs(Pos - Pos1),
    T = Td + Diff * velocity();
how_long(N, Dir, State) ->
    io:format("How_long to go to floor:~p Dir:~p State=~p~n",
	      [N, Dir, State]),
    1000.

velocity()      -> 10.
velocity_door() -> 3.
    
stopAt(LiftNo, FloorNo, {Doors, State, Pos, Stop}, Pid) ->
    io:format("Lift:~p Stop at:~p State=~p~n",[LiftNo, FloorNo, State]),
    Pid ! {setLiftLamp, LiftNo, FloorNo,?red},
    Pid ! {setFloorLamp,LiftNo, FloorNo,?red},
    {Doors, State, Pos, Stop ++ [FloorNo]}.


time_to_close_door(closed)       -> 0;
time_to_close_door({closing, N}) -> N * velocity_door();
time_to_close_door({opening, N}) -> (208 - N) * velocity_door();
time_to_close_door({opened,N})   -> (108 - N) * velocity_door().




