%/--------------------------------------------------------------------
%| Copyright 2017 Kazimieras Senvaitis
%|
%| Licensed under the Apache License, Version 2.0 (the "License");
%| you may not use this file except in compliance with the License.
%| You may obtain a copy of the License at
%|
%|     http://www.apache.org/licenses/LICENSE-2.0
%|
%| Unless required by applicable law or agreed to in writing, software
%| distributed under the License is distributed on an "AS IS" BASIS,
%| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%| See the License for the specific language governing permissions and
%| limitations under the License.
%\--------------------------------------------------------------------

%%% @doc
%%% The main supervisor for the application.
%%%
-module(firefork_audio_player_sup).
-behaviour(supervisor).
-export([
    start_link/0,
    start_child/1
]).
-export([init/1]).


%%====================================================================
%% API functions
%%====================================================================

%%  @doc
%%  Create this supervisor.
%%
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%  @doc
%%  Starts child process with its spec.
%%
start_child(Module) ->
    {ok, _Pid} = supervisor:start_child(?MODULE, #{
            id       => firefork_audio_player,
            start    => {Module, start_link, [Module]},
            restart  => transient,
            shutdown => 5000,
            type     => worker,
            modules  => [Module]
        }
    ).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{
        strategy  => one_for_one,
        intensity => 10,
        period    => 10000
    },
    {ok, {SupFlags, []}}.
