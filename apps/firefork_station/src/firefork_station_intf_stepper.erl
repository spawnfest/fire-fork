%%% @doc
%%% Represents the UART interface for communication with
%%% the stepper running launch script.
%%%
-module(firefork_station_intf_stepper).
-behaviour(firefork_radio).
-compile([{parse_transform, lager_transform}]).
-export([start_link/0, send_check/2, send_start/3, send_stop/2]).
-export([handle_message/1]).


%%% ============================================================================
%%% API functions
%%% ============================================================================

%%  @doc
%%  Starts the server.
%%
start_link() ->
    firefork_radio:start_link({local, ?MODULE}, ?MODULE, #{}).


%%  @doc
%%  Send check command to stepper.
%%
send_check(ScenarioName, ScenarioCRC) ->
    CallRef = erlang:make_ref(),
    send({start, ScenarioName, ScenarioCRC, CallRef}).

%%  @doc
%%  Send start command to stepper.
%%
send_start(ScenarioName, ScenarioCRC, FromMS) ->
    CallRef = erlang:make_ref(),
    send({start, ScenarioName, ScenarioCRC, CallRef, FromMS}).

%%  @doc
%%  Send stop command to stepper.
%%
send_stop(ScenarioName, ScenarioCRC) ->
    CallRef = erlang:make_ref(),
    send({stop, ScenarioName, ScenarioCRC, CallRef}).

%%  @private
%%  Send a message via the radio link.
%%
send(Message) ->
    firefork_radio:send(?MODULE, Message).



%%% ============================================================================
%%% Callbacks for the `firefork_radio'.
%%% ============================================================================

%%
%%  Initialization.
%%
handle_message({ack, Command, CallRef, Status}) ->
    lager:warning("Command ~p send to stepper and retuned status ~p (CallRef=~p)", [Command, CallRef, Status]),
    % todo implement ACK handling    
    ok;

handle_message({completed, ScenarioName, _ScenarioCRC}) ->
    lager:warning("Received <completed> command for scenarion ~p. Its safe to go back.", [ScenarioName]),
    % todo implement completed message handling    
    ok;

handle_message(Unknown) ->
    lager:warning("Dropping unknown message: ~p", [Unknown]),
    ok.



%%% ============================================================================
%%% Internal Functions.
%%% ============================================================================


