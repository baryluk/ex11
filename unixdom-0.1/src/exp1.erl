%%%----------------------------------------------------------------------
%%% File    : exp1.erl
%%% Purpose : First naive experiment with R7B async-style drivers
%%%----------------------------------------------------------------------

-module(exp1).
-behaviour(gen_server).

-define(DRV_NAME, "exp1_drv").
%% We don't define a timeout macro because we can't do timeouts gracefully.
-define(MAXIMUM_ID, 4000000000).

%% Borrowed from inet_int.hrl
-define(int32(X), 
        [((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
         ((X) bsr 8) band 16#ff, (X) band 16#ff]).
-define(u32(X3,X2,X1,X0), 
        (((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

%%%
%%% Driver<->emulator communication codes (xref with top of exp1_drv.c)
%%%

-define(EXP1_REQ_GETHOSTBYNAME,		1).

-define(EXP1_RESP_OK,			0).
-define(EXP1_RESP_ERROR,		1).
-define(EXP1_RESP_BYTES,		2).

%% External exports
-export([start_link/0]).
-export([gethostbyname/1, status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	  port = void,
	  idle = [],				% Idle port list XXX unused
	  pending = [],				% Pending job list XXX unused
	  id = 0				% Next ID
	 }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

gethostbyname(H) ->
    gen_server:call(?MODULE, {gethostbyname, H}, infinity).

status() ->
    gen_server:call(?MODULE, {status}, infinity).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    Path = load_path(),
    erl_ddll:start(),
    ok = erl_ddll:load_driver(Path, ?DRV_NAME),
    Port = open_port({spawn, ?DRV_NAME}, []),
    {ok, #state{port = Port}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({gethostbyname, H}, From, State) ->
    {Port, NewState} = get_idle_port(State),
    ID = State#state.id,
    Reply = start_gethostbyname(H, ID, Port),
    PendingList = State#state.pending,
    Job = {ID, From},
    {noreply, State#state{pending = [Job|PendingList], id = next_id(ID)}};
handle_call({status}, From, State) ->
    Reply = {{port, State#state.port}, {idle, State#state.idle}, {pending, State#state.pending}},
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({Port, {data, [Response|Rest]}}, State) ->
    {ID, Res} = get_response(Response, Rest),
    {From, NewPending, NewIdle} = get_from(ID, State),
    gen_server:reply(From, Res),
    {noreply, State#state{idle = NewIdle, pending = NewPending}};
handle_info({Port, {'EXIT', Port, Reason}}, State) ->
    Msg = {error, port_died, Reason},
    {stop, Msg, State};
handle_info(Info, State) ->
    io:format("XXXYYYXXX ~w: ~s:handle_info got ~w\n", [self(), ?MODULE, Info]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    F = fun({_ID, From}) -> gen_server:reply(From, Reason) end,
    lists:map(F, State#state.pending),
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% We assume that the ddll driver is located
%% at the same directory as the jam/beam file.
%% Thus, this code can't be preloaded or interpreted.
load_path() ->
    case code:is_loaded(?MODULE) of
	{file,File} when is_list(File) -> 
	    filename:dirname(File) ++ "../src";
	_ -> 
	    Emsg = "~w: Can't find path to load driver from !~n",
	    error_logger:error_msg(Emsg,[?MODULE]),
	    exit(no_path)
    end.

start_gethostbyname(H, ID, Port) ->
    port_command(Port, [?EXP1_REQ_GETHOSTBYNAME, ?int32(ID), H, 0]).

get_response(?EXP1_RESP_OK, [I1,I2,I3,I4]) ->
    ID = ?u32(I1,I2,I3,I4),
    {ID, ok};
get_response(?EXP1_RESP_OK, [I1,I2,I3,I4|Data]) ->
    ID = ?u32(I1,I2,I3,I4),
    {ID, {ok, Data}};
get_response(?EXP1_RESP_ERROR, [I1,I2,I3,I4|List]) ->
    ID = ?u32(I1,I2,I3,I4),
    {ID, {error, list_to_atom(List)}};
get_response(?EXP1_RESP_BYTES, [I1,I2,I3,I4|List]) ->
    ID = ?u32(I1,I2,I3,I4),
    {ID, {ok, List}};
get_response(X, Y) ->
    R = {error, {bad_response_from_port, X, Y}},
    R.

%% Hrm.  I don't recall what this was all about.  I think the
%% original idea would be to have a number of ports open and
%% available in a worker pool.  {shrug}  With the async port
%% driver scheme, such work is overkill.

get_idle_port(State) ->
    {State#state.port, State}.

get_from(ID, State) ->
    F = fun({ID2, _From2}) when ID2 == ID -> true;
	   ({_, _}) -> false
	end,
    Pending = State#state.pending,
    Idle = State#state.idle,
    case lists:filter(F, Pending) of
	[{ID, From}] ->
	    NotF = fun(X) -> case F(X) of
				 false -> true;
				 true -> false
			     end
		   end,
	    NewPending = lists:filter(NotF, Pending),
	    {From, NewPending, Idle};
	[] ->
	    {self(), Pending, Idle};			% XXX bogus
	Res ->
	    {self(), Pending, Idle}			% XXX bogus
    end.

next_id(ID) ->
    if ID >= ?MAXIMUM_ID -> 0;
       true              -> ID + 1
    end.
