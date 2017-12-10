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

-module(firefork_audio_player).
-export([
    play/1,
    stop/0,
    pause/0,
    resume/0,
    next/0,
    prev/0
]).
%%% ============================================================================
%%% Callback definitions.
%%% ============================================================================

%%
%%  These callbacks are used for music play management.
%%
%-callback start_link(Name :: atom(), Path :: string()) ->
%        ok.

-callback play(
        Pid   :: term(),
        Path  :: string()
    ) ->
        ok.

-callback stop(
        Pid   :: term()
    ) ->
        ok.

-callback pause(
        Pid   :: term()
    ) ->
        ok.

-callback resume(
        Pid   :: term()
    ) ->
        ok.

-callback next(
        Pid   :: term()
    ) ->
        ok.

-callback prev(
        Pid   :: term()
    ) ->
        ok.



%%% ============================================================================
%%% Public API.
%%% ============================================================================

%%
%%  Returns the default store reference.
%%
-spec play(Path :: string()) -> ok.

play(Path) ->
    {ok, Module} = firefork_audio_player_app:get_env(player),
    Module:play(Module, Path).


%%
%%  Returns the default store reference.
%%
-spec stop() -> ok.

stop() ->
    {ok, Module} = firefork_audio_player_app:get_env(player),
    Module:stop(Module).


%%
%%  Returns the default store reference.
%%
-spec pause() -> ok.

pause() ->
    {ok, Module} = firefork_audio_player_app:get_env(player),
    Module:pause(Module).


%%
%%  Returns the default store reference.
%%
-spec resume() -> ok.

resume() ->
    {ok, Module} = firefork_audio_player_app:get_env(player),
    Module:resume(Module).


%%
%%  Returns the default store reference.
%%
-spec next() -> ok.

next() ->
    {ok, Module} = firefork_audio_player_app:get_env(player),
    Module:next(Module).


%%
%%  Returns the default store reference.
%%
-spec prev() -> ok.

prev() ->
    {ok, Module} = firefork_audio_player_app:get_env(player),
    Module:prev(Module).
