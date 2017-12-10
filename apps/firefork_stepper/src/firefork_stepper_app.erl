%%%
%%% The OPT application.
%%%
-module(firefork_stepper_app).
-behaviour(application).
-export([get_env/2]).
-export([start/2, stop/1]).

-define(APP, firefork_stepper).


%%% ============================================================================
%%% Public API.
%%% ============================================================================

%%
%%
%%
get_env(Name, Default) ->
    application:get_env(?APP, Name, Default).


%%% ============================================================================
%%% Callbacks for the `application'.
%%% ============================================================================

%%
%%  Start the application.
%%
start(_StartType, _StartArgs) ->
    firefork_stepper_sup:start_link().


%%
%%  Stop the application.
%%
stop(_State) ->
    ok.


