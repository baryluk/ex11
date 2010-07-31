-module(top).
-author('tobbe@serc.rmit.edu.au').
%%----------------------------------------------------------------------
%% File    : top.erl
%% Created : 3 Sep 1998 by tobbe@serc.rmit.edu.au
%% Function: Somewhat similar to Unix Top. Displays:
%%            + the pid
%%            + the number of reductions since last time
%%            + the registered name
%%            + change in stack size since last time
%%            + change in heap size since last time
%%            + current function call
%%
%%   It sorts the output depending on how it was started.
%%   The default is to sort on number of reductions since last call.
%%----------------------------------------------------------------------
%% -vc('$Id: top.erl,v 1.1 2000/03/17 18:08:51 scott Exp $ ').
%%
%% Subsequently modified by SLF to be less fragile and not require 132 columns.
%%
%% usage:	Pid = top:start().
%%		top:stop(Pid).
%%
-export([start/0,pid/0,reds/0,name/0,stack/0,heap/0,start/3,stop/1]).
%% Internal
-export([init/3]).

%% -------------------
%% Exported interface

%% Default is a time interval of 3 seconds,
%% sort the output on reductions and print
%% no more than 20 lines.

start() -> reds().

pid()   -> start(3,pid,20).
reds()  -> start(3,reds,20).
name()  -> start(3,name,20).
stack() -> start(3,stack,20).
heap()  -> start(3,heap,20).

start(Sec,Sort,Num) ->
    spawn(?MODULE, init, [Sec*1000,Sort,Num]).

stop(Pid) when pid(Pid) ->
    Pid ! stop.

%% -----------
%% The server

init(MilliSec,Sort,Num) ->
    Ps = processes(),
    {TotalReds, _} = statistics(reductions),
    loop(Ps,info(Ps),MilliSec,Sort,Num,TotalReds).

loop(Ps,Info,MilliSec,Sort,Num,LastReds) ->
    sleep(MilliSec),
    Ps2 = processes(),
    Info2 = info(Ps2),
    {TotalReds, _} = statistics(reductions),
    display(diff(Info2,Info),Sort,Num,TotalReds-LastReds),
    loop(Ps2,Info2,MilliSec,Sort,Num,TotalReds).

display(Info,Sort,Num,SysReds) ->
    io:format("~nPid            "
	      "Reds     "
	      "Registered Name "
	      "Stack  "
	      "Heap       "
	      "Current Function Call~n"),
    io:format("~s~n",[string:copies("-",79)]),
    print(lists:reverse(sort(Info,Sort)),Num),
    SumReds = reductions_sum(Info),
    io:format("~w of ~w reductions found, ~f%\n", [SumReds, SysReds,
						   SumReds / SysReds * 100]).
reductions_sum([{P,R,N,S,H,C}|T]) ->
    R + reductions_sum(T);
reductions_sum([]) ->
    0.

sort(Info,pid)   -> lists:keysort(1,Info);
sort(Info,reds)  -> lists:keysort(2,Info);
sort(Info,name)  -> lists:keysort(3,Info);
sort(Info,stack) -> lists:keysort(4,Info);
sort(Info,heap)  -> lists:keysort(5,Info).

print(_,1) -> ok;
print([{P,R,N,S,H,C}|T],Num) ->
    io:format("~-10.w ~8.w ~19.s ~5.w ~5.w ~27.s~n",[P,R,N,S,H,C]),
    print(T,Num-1);
print([],_) -> ok.

%% -----------------------------------------------
%% diff(Info1, Info2)
%%
%% Assume the Info to be sorted on the Pid field.
%% For each Pid in Info1 which have also exist in
%% Info2, compute the change of the field values.
%% NB: If Pid didn't exist in Info2 then use the
%% values in Info1 as is.

diff([{P,R1,N,S1,H1,C}|T1],[{P,R2,_,S2,H2,_}|T2]) ->
    [{P,R1-R2,N,S1-S2,H1-H2,C}|diff(T1,T2)];
diff([{P1,R1,N,S1,H1,C}|T1],[{P2,R2,_,S2,H2,_}|T2]) when P2<P1 ->
    diff([{P1,R1,N,S1,H1,C}|T1],T2);
diff([{P1,R1,N1,S1,H1,C1}|T1],[{P2,R2,N2,S2,H2,C2}|T2]) when P1<P2 ->
    [{P1,R1,N1,S1,H1,C1}|diff(T1,[{P2,R2,N2,S2,H2,C2}|T2])];
diff(_,_) ->
    [].

%% ------------------------------------------------
%% Return the process info sorted on the Pid field.

info(Ps) ->
    lists:keysort(1,info_2(Ps)).

info_2([H|T]) ->
    Reds  = proc_info(H,reductions),
    Name      = reg_name(H),
    Stack = proc_info(H,stack_size),
    Heap  = proc_info(H,heap_size),
    Cfn       = cur_func(H),
    [{H,Reds,Name,Stack,Heap,Cfn}|info_2(T)];
info_2([]) -> [].

proc_info(Pid, Type) ->
    case process_info(Pid, Type) of
	{Type, N} -> N;
	L when list(L) -> L;
	undefined -> 0
    end.

cur_func(P) ->
    case proc_info(P,current_function) of
	{M,F,A} when integer(A) ->
	    lists:concat([M,":",F,"/",A]);
	{M,F,A} when list(A) ->
	    lists:concat([M,":",F,"/",length(A)]);
	_ -> ""
    end.

reg_name(P) ->
    case proc_info(P,registered_name) of
	[] -> "";
	0 -> "";
	Name -> atom_to_list(Name)
    end.

sleep(MilliSec) ->
    receive
	stop ->
	    exit(stopped)
    after MilliSec ->
	    true
    end.

