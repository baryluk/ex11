-module(lifts).

-compiler(export_all).

-compiler(stub).

reserve_lift(Lifts, Dirn, Floor) ->
        Times = how_long(tuple_to_list(Lifts), Floor, Dirn),
        Min = min_time(Times),
        reserve(Times, Min).

how_long([], _, _) ->
        [];
how_long([Id|T], Floor, Dirn) ->
        Id ! {self(), how_long, Floor, Dirn},
        receive
           {Id, Time} ->
                true
        end,
        [{Time,Id} | how_long(T, Floor, Dirn)].

min_time([{Time,Id}|T]) ->
        min_time(T, Time, Id).
min_time([], Time, Id) ->
        Id;
min_time([{Time1,Id1}|T], Time, Id) when Time1 < Time ->
        min_time(T, Time1, Id1);
min_time([_|T], Time, Id) ->
        min_time(T, Time, Id).

reserve([], _) ->
    true;
reserve([{_,Id}|T], Id) ->
        Id ! {self(), commit},
        reserve(T, Id);
reserve([{_,Id1}|T], Id) ->
        Id1 ! {self(), unlock},
        reserve(T, Id).

top_floor(Floor, Lifts) ->
        receive 
            {Floor, UpDown} ->
                reserve_lift(Lifts, UpDown, Floor),
                light_lamp(Floor, UpDown),
                top_floor(Floor, Lifts);
            {A_lift, arrived, GoingUpOrDown} ->
                   extingush_lamp(Floor, GoingUpOrDown)
        end.

top_lift(Lift, State) ->
        receive 
            {system, stop, Time, Floor} ->
                trye;
            {Id , how_long, Floor, Dirn} ->
                {Doing,Now,Stop} = State,
                T = wait_time(Dirn, Floor, Now, Stop),
                Id ! T,
                receive 
                    {Id, commit} ->
                        Stop1 = insert(Dirn, Floor, Now, Stop);
                    {Id, unlock} ->
                        Stop1 = Stop
                end,
                top_lift(Lift, {Doing,Now,Stop1})
        end.

insert(Dirn, Floor, Now, Stop) ->
        insert(Dirn, Floor, Now, [], Stop).

insert(Dirn, Floor, Now, Before, []) ->
        lists:reverse([Floor|Before]);
insert(Dirn, Floor, Now, Before, [Floor|After]) ->
        lists:reverse(Before, [Floor|After]);
insert(Dirn, Floor, Floor, Before, [Next|After]) ->
        lists:reverse(Before, [Next|After]);
insert(up, Floor, Now, Before, [Next|After]) when 
    Now < Floor, Floor < Next ->
        lists:reverse(Before, [Floor,Next|After]);
insert(down, Floor, Now, Before, [Next|After]) 
    when Next < Floor, Floor < Now ->
        lists:reverse(Before, [Floor,Next|After]);
insert(Dirn, Floor, Now, Before, [Next|After]) ->
        insert(Dirn, Floor, Next, [Next|Before],After).

wait_time(Dirn, Floor, T, Now, []) ->
        T + abs(Floor - Now);
wait_time(Dirn, Floor, T, Now, [Floor|After]) ->
        T + abs(Floor - Now);
wait_time(up, Floor, T, Now, [Next|After]) when
        Now =< Floor, Floor =< Next ->
        T + Floor - Now;
wait_time(down, Floor, T, Now, [Next|After]) when
        Now >= Floor, Floor >= Next ->
        T + Now - Floor;
wait_time(Dirn, Floor, T, Now, [Next|After]) ->
        wait_time(Dirn, Floor, T + abs(Now - Next) + 5, Next, After).

stop_at(Floor, Now, Stop) ->
        stop_at_1(Floor, [], [Now|Stop]).

stop_at_1(Floor, Before, []) ->
        lists:reverse([Floor|Before]);
stop_at_1(Floor, Before, [Floor|After]) ->
        lists:reverse(Before, [Floor|After]);
stop_at_1(Floor, Before, [X,Y|After]) when X < Floor, Floor < Y ->
        lists:reverse(Before, [X,Floor,Y|After]);
stop_at_1(Floor, Before, [X,Y|After]) when Y < Floor, Floor < X ->
        lists:reverse(Before, [X,Floor,Y|After]);
stop_at_1(Floor, Before, [H|T]) ->
        stop_at_1(Floor, [H|Before],T).

top_lift1(Lift, State) ->
        receive 
            {Sys, stop, Time, Floor} ->
                format("~w lift:~w stop at:~w~n", [Time,Lift,Floor]),
                {Doing,Now,Stop} = State,
                Stop1 = stop_at(Floor, Now, Stop),
                State1 = {Doing,Now,Stop1},
                Sys ! ack,
                top_lift(Lift, State1);
            {Id, how_long, Floor, Dirn} ->
                top_lift(Lift, State)
        end.
