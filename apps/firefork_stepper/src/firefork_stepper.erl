%%% @doc
%%% Main interface for the `firefork_stepper' appplication.
%%%
-module(firefork_stepper).
-export([start/0]).


%%% ============================================================================
%%% API functions
%%% ============================================================================

%%
%%  This can be used to start the application from the
%%  command line via the `-s' switch.
%%
start() ->
    {ok, _} = application:ensure_all_started(?MODULE),
    ok.


