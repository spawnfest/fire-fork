%%% @doc
%%% Supervisor for all the relay controllers.
%%%
-module(firefork_stepper_relay_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

%%% ============================================================================
%%% API functions
%%% ============================================================================

%%  @doc
%%  Starts the supervisor.
%%
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



%%% ============================================================================
%%% Supervisor callbacks
%%% ============================================================================

%%
%%
%%
init([]) ->
    SupFlags = #{
        strategy  => one_for_all,
        intensity => 600,
        period    => 60000
    },
    {ok, {SupFlags, [
        #{
            id    => firefork_stepper_relay_gpio,
            start => {firefork_stepper_relay_gpio, start_link, [_Channel = 0, #{}]}
        }
    ]}}.


