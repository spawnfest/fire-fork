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

%%%
%%% Common Tests for `exometer_graphite' application.
%%%
-module(firefork_audio_player_SUITE).
-compile([{parse_transform, lager_transform}]).
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    test_single_mp3_play/1,
    test_playlist_play/1
]).

-define(APP, firefork_audio_player).

%%% ============================================================================
%%% Callbacks for `common_test'
%%% ============================================================================

%%  @doc
%%  CT API.
%%
all() -> [
    test_single_mp3_play,
    test_playlist_play
].


%%
%%  CT API, initialization.
%%
init_per_suite(Config) ->
    {ok, LagerApps} = application:ensure_all_started(lager),
    {ok, Apps} = application:ensure_all_started(?APP),
    [{itest_apps, LagerApps ++ Apps} | Config].


%%
%%  CT API, cleanup.
%%
end_per_suite(Config) ->
    ok = lists:foreach(
        fun (A) -> application:stop(A) end,
        proplists:get_value(itest_apps, Config)
    ),
    ok.


%%
%%  Log test case name at start
%%
init_per_testcase(TestCase, Config) ->
    lager:debug("---------------------- ~p start", [TestCase]),
    Config.


%%
%%  Log test case name at end.
%%
end_per_testcase(TestCase, _Config) ->
    lager:debug("---------------------- ~p end", [TestCase]),
    ok.



%%% ============================================================================
%%% Test cases.
%%% ============================================================================

%%  @doc
%%  Check, if play management of single mp3 works.
%%  Speakerst should make a sound.
%%
test_single_mp3_play(_Config) ->
    {error,audio_player_not_stated} = firefork_audio_player:stop(),
    ok = firefork_audio_player:play("../../../../itest/song1.mp3"),
    {error, already_playing} = firefork_audio_player:play("../../../../itest/song1.mp3"),
    timer:sleep(1000),
    ok = firefork_audio_player:pause(),
    {error, already_paused} = firefork_audio_player:pause(),
    ok = firefork_audio_player:resume(),
    {error, already_resumed} = firefork_audio_player:resume(),
    {error, {not_available_for_single_mp3, next}} = firefork_audio_player:next(),
    {error, {not_available_for_single_mp3, prev}} = firefork_audio_player:prev(),
    ok = firefork_audio_player:stop(),
    {error,audio_player_not_stated} = firefork_audio_player:stop().


%%  @doc
%%  Check, if play management of a playlist works.
%%  Speakerst should make a sound.
%%
test_playlist_play(_Config) ->
    {error,audio_player_not_stated} = firefork_audio_player:stop(),
    ok = firefork_audio_player:play("../../../../itest/playlist1.m3u"),
    {error, already_playing} = firefork_audio_player:play("../../../../itest/playlist1.m3u"),
    timer:sleep(1000),
    ok = firefork_audio_player:pause(),
    {error, already_paused} = firefork_audio_player:pause(),
    ok = firefork_audio_player:resume(),
    {error, already_resumed} = firefork_audio_player:resume(),
    ok = firefork_audio_player:next(),
    timer:sleep(1000),
    ok = firefork_audio_player:next(),
    ok = firefork_audio_player:prev(),
    ok = firefork_audio_player:prev(),
    ok = firefork_audio_player:stop(),
    {error,audio_player_not_stated} = firefork_audio_player:stop().
