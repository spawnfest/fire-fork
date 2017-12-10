%%% @doc
%%% Represents the UART interface for communication with
%%% the remote station.
%%%
-module(firefork_stepper_intf_station).
-behaviour(firefork_radio).
-compile([{parse_transform, lager_transform}]).
-export([start_link/0, send_completed/2]).
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
%%  Send notification to the station on the completion of the script.
%%  This message is asynchronous and requires no acknowledgement.
%%
send_completed(ScenarioName, ScenarioCRC) ->
    send({completed, ScenarioName, ScenarioCRC}).


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
handle_message({start, ScenarioName, ScenarioCRC, CallRef}) ->
    % TODO: Scenario start.
    ok = send({ack, start, CallRef, ok}),
    ok;

handle_message({stop, ScenarioName, ScenarioCRC, CallRef}) ->
    % TODO: Scenario stop.
    ok = send({ack, stop, CallRef, ok}),
    ok;

handle_message({check, ScenarioName, ScenarioCRC, CallRef}) ->
    % TODO: Scenario check.
    ok = send({ack, check, CallRef, ok}),
    ok;

handle_message(Unknown) ->
    lager:warning("Dropping unknown message: ~p", [Unknown]),
    ok.



%%% ============================================================================
%%% Internal Functions.
%%% ============================================================================


