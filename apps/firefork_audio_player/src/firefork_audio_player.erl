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

%%% ============================================================================
%%% Callback definitions.
%%% ============================================================================

%%
%%  These callbacks are used for music play management.
%%
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
