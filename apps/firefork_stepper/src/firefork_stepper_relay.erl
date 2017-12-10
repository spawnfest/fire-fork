%%%
%%%
%%%
-module(firefork_stepper_relay).
-export([fire/2]).


%%% ============================================================================
%%% Callback definitions.
%%% ============================================================================

%%
%%  Fire the specific relay.
%%
-callback fire(
        Channel :: integer(),
        Relay   :: integer()
    ) ->
        ok.


%%% ============================================================================
%%% API functions
%%% ============================================================================

%%
%%  Fire the specific relay.
%%
fire(0 = Channel, Relay) ->
    ok = firefork_stepper_relay_gpio:fire(Channel, Relay);

fire(Channel, Relay) ->
    % TODO: Implement configurable set of possible relay conrtollers.
    lager:warning("Unsupported channel=~p, relay=~p", [Channel, Relay]),
    ok.


