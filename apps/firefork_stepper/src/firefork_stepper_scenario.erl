%%%
%%%
%%%
-module(firefork_stepper_scenario).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([start_link/0, start/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%% ============================================================================
%%% API functions
%%% ============================================================================

%%  @doc
%%  Starts the server.
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).


%%
%%
%%
start(FromMS) ->
    gen_server:call(?MODULE, {start, FromMS}).


%%
%%
%%
stop() ->
    gen_server:call(?MODULE, stop).



%%% ============================================================================
%%% Internal state.
%%% ============================================================================

-record(step, {
    id      :: integer(),   % Step identifier.
    time    :: integer(),   % Offset from the start in MS.
    channel :: integer(),   % Number of the relay channel.
    relay   :: integer(),   % Number of a relay in the channel.
    tref    :: reference()  % Timer referece.
}).

-record(scenario, {
    name    :: string(),
    crc     :: integer(),
    steps   :: [#step{}]
}).

%%
%%
%%
-record(state, {
    status   :: waiting | running,          % Status of the scenario.
    scenario :: #scenario{},                % The scenario, as it was read from the file.
    pending  :: #{reference() => #step{}},  % A set of steps not fired yet.
    from_ms  :: integer(),  % From which MS (relative to script start) the script was started last time.
    start_ms :: integer(),  % Unix seconds, when the script was started.
    stop_ms  :: integer(),  % Unix seconds, when the script was stopped.
    run_ref  :: reference() % Used to avoid old fire events when starting/stopping the script.
}).



%%% ============================================================================
%%% Callbacks for the `gen_server'.
%%% ============================================================================

%%
%%  Initialization.
%%
init({}) ->
    {ok, Scenario} = load_scenario_file(firefork_stepper_app:get_env(scenario_file, "scenario.csv")),
    State = #state{
        status   = waiting,
        scenario = Scenario,
        pending  = #{}
    },
    {ok, State}.


%%
%%  Synchronous calls.
%%
handle_call({start, FromMS}, _From, State) ->
    {ok, TmpState} = do_stop(State),
    {ok, NewState} = do_start(FromMS, TmpState),
    {reply, ok, NewState};

handle_call(stop, _From, State) ->
    {ok, NewState} = do_stop(State),
    {reply, ok, NewState};

handle_call(Unknown, _From, State) ->
    lager:warning("Dropping unknown call: ~p", [Unknown]),
    {reply, undefined, State}.


%%
%%  Asynchronous calls.
%%
handle_cast(Unknown, State) ->
    lager:warning("Dropping unknown cast: ~p", [Unknown]),
    {noreply, State}.


%%
%%  Other messages.
%%
handle_info({fire, FRef, RunRef, Step}, State = #state{run_ref = RunRef}) when RunRef =/= undefined ->
    {ok, NewState} = do_fire(FRef, Step, State),
    {noreply, NewState};

handle_info({fire, _FRef, _RunRef, _Step}, State) ->
    % Drop outdated fire events (after the stop).
    {noreply, State};

handle_info(Unknown, State) ->
    lager:warning("Dropping unknown info: ~p", [Unknown]),
    {noreply, State}.


%%
%%  Process termination.
%%
terminate(_Reason, _State) ->
    ok.


%%
%%  Code upgrades.
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%% ============================================================================
%%% Internal functions.
%%% ============================================================================

%%
%%
%%
load_scenario_file(FileName) ->
    {ok, FileContent} = file:read_file(FileName),
    load_scenario(FileName, FileContent).


load_scenario(FileName, FileContent) ->
    Lines = re:split(FileContent, "\r?\n"),
    ParseLine = fun
        (<<>>) ->
            false;
        (Line) ->
            [Time, Channel, Cue | _] = re:split(Line, "[\t,;]"),
            {true, #step{
                time    = erlang:binary_to_integer(Time),
                channel = erlang:binary_to_integer(Channel),
                relay   = erlang:binary_to_integer(Cue) % NOTE: Relay = Cue
            }}
    end,
    Steps = lists:filtermap(ParseLine, Lines),
    StepsWithIds = lists:map(
        fun ({Id, Step}) -> Step#step{id = Id} end,
        lists:seq(1, length(Steps)),
        Steps
    ),
    Scenario = #scenario{
        name  = FileName,
        crc   = erlang:crc32(FileContent),
        steps = StepsWithIds
    },
    {ok, Scenario}.


%%
%%
%%
do_start(FromMS, State = #state{scenario = #scenario{steps = Steps}}) ->
    Pending = lists:sort(Steps),
    StartMS = erlang:system_time(millisecond),
    RunRef  = erlang:make_ref(),
    NewPending = maps:from_list(lists:filtermap(fun
        (Step = #step{time = Time}) when Time >= FromMS ->
            FRef = erlang:make_ref(),
            TRef = erlang:send_after(Time, self(), {fire, FRef, RunRef, Step}),
            {true, {FRef, Step#step{
                tref = TRef
            }}};
        (#step{}) ->
            % Drop steps, that are in the past.
            false
    end, Pending)),
    NewState = State#state{
        status   = running,
        pending  = NewPending,
        from_ms  = FromMS,
        start_ms = StartMS,
        run_ref  = RunRef
    },
    {ok, NewState}.


%%
%%
%%
do_stop(State = #state{pending = Pending}) ->
    StopMS = erlang:system_time(millisecond),
    NewPending = maps:filter(fun % used as maps:foreach/2
        (_FRef, #step{tref = undefined}) ->
            false;
        (_FRef, #step{tref = TRef}) ->
            _ = erlang:cancel_timer(TRef),
            false
    end, Pending),
    NewState = State#state{
        status   = waiting,
        pending  = NewPending, % =:= #{}
        stop_ms  = StopMS,
        run_ref  = undefined
    },
    {ok, NewState}.


%%
%%
%%
do_fire(FRef, Step, State = #state{pending = Pending}) ->
    #step{
        channel = Channel,
        relay   = Relay
    } = Step,
    ok = firefork_stepper_relay:fire(Channel, Relay),
    NewState = State#state{
        pending = maps:remove(FRef, Pending)
    },
    {ok, NewState}.


