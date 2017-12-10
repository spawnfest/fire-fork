%%%
%%% Controller for the GPIO based relays.
%%%
-module(firefork_stepper_relay_gpio).
-behaviour(firefork_stepper_relay).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([start_link/2]).
-export([fire/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%% ============================================================================
%%% API functions
%%% ============================================================================

%%
%%  Start the controller.
%%
start_link(Channel, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Channel, Opts}, []).



%%% ============================================================================
%%% Callbacks for the `firefork_stepper_relay'.
%%% ============================================================================

%%
%%
%%
fire(_Channel, Relay) ->
    gen_server:cast(?MODULE, {fire, Relay}).



%%% ============================================================================
%%% Internal state.
%%% ============================================================================

%%
%%
%%
-record(relay, {
    id      :: integer(),
    fref    :: reference(),     % Unique identifier for the fire event.
    tref    :: reference()      % Timer identifier (for clearing).
}).


%%
%%
%%
-record(state, {
    channel  :: integer(),      % Identifier of this channel.
    pins     :: [integer()],    % Managed GPIO pins.
    duration :: integer(),      % Duration for which a relay is kept open.
    relays   :: #{Id :: integer() => #relay{}}
}).



%%% ============================================================================
%%% Callbacks for the `gen_server'.
%%% ============================================================================

%%
%%  Initialization.
%%
init({Channel, Opts}) ->
    GpioPins = maps:get(gpio_pins, Opts, [2, 3]),
    ok = lists:foreach(fun (GpioPin) ->
        ok = gpio:init_direct(GpioPin),
        ok = gpio:set_direction(GpioPin, low),
        ok = gpio:clr(GpioPin)
    end, GpioPins),
    State = #state{
        channel  = Channel,
        pins     = GpioPins,
        duration = maps:get(duration, Opts, 1000)
    },
    {ok, State}.


%%
%%  Synchronous calls.
%%
handle_call(Unknown, _From, State) ->
    lager:warning("Dropping unknown call: ~p", [Unknown]),
    {reply, undefined, State}.


%%
%%  Asynchronous calls.
%%
handle_cast({fire, Relay}, State = #state{relays = Relays, duration = Duration}) ->
    case Relays of
        #{Relay := #relay{tref = OldTRef}} -> _ = erlang:cancel_timer(OldTRef);
        #{}                                -> ok
    end,
    ok = gpio:set(Relay),
    FRef = erlang:make_ref(),
    TRef = erlang:send_after(Duration, self(), {clear, Relay, FRef}),
    NewRelays = Relays#{Relay => #relay{
        fref = FRef,
        tref = TRef
    }},
    NewState = State#state{
        relays = NewRelays
    },
    {noreply, NewState};

handle_cast(Unknown, State) ->
    lager:warning("Dropping unknown cast: ~p", [Unknown]),
    {noreply, State}.


%%
%%  Other messages.
%%
handle_info({clear, Relay, FRef}, State = #state{relays = Relays}) ->
    NewRelays = case Relays of
        #{Relay := #relay{fref = FRef}} ->
            ok = gpio:clr(Relay),
            maps:remove(Relay, Relays);
        #{Relay := #relay{fref = OtherFRef}} when OtherFRef =/= FRef ->
            % Thats maybe a new fire event, overlapping with this event.
            Relays;
        #{} ->
            % Clear it anyway, its safier.
            ok = gpio:clr(Relay),
            maps:remove(Relay, Relays)
    end,
    NewState = State#state{
        relays = NewRelays
    },
    {noreply, NewState};

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


