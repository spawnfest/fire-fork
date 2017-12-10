-module(firefork_station_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SupFlags = #{
        strategy  => one_for_all,
        intensity => 600,
        period    => 60000
    },
    {ok, {SupFlags, [
        #{
            id      => firefork_control_panel, 
            start   => {firefork_control_panel, start_link, []},
            restart => transient
        },
        % #{
        %     id    => firefork_audio_player_sup,
        %     start => {firefork_audio_player_sup, start_link, []},
        %     type  => supervisor
        % },
        #{
            id    => firefork_station_intf_sup,
            start => {firefork_station_intf_sup, start_link, []},
            type  => supervisor
        }

    ]} }.

