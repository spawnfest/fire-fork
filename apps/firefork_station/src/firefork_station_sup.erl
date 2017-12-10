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
    {ok, { {one_for_one, 5, 10}, [
        #{id => firefork_control_panel, 
          start => {firefork_control_panel, start_link, []}  }
    ]} }.

