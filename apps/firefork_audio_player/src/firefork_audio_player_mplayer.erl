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
%%% This module allows to play music.
%%% It depends on OS software - mplayer.
%%% Single mp3 files and their playlists (.m3u) are supported.
%%%
-module(firefork_audio_player_mplayer).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    stop/1,
    pause/1,
    resume/1,
    next/1,
    prev/1
]).
-export([
    init/1,
    handle_info/2,
    handle_cast/2,
    handle_call/3,
    code_change/3,
    terminate/2
]).


%%% ============================================================================
%%% API functions.
%%% ============================================================================

%%  @doc
%%  Manage audio player.
%%
start_link(FilePath) ->
    gen_server:start_link(?MODULE, FilePath, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

pause(Pid) ->
    gen_server:call(Pid, pause).

resume(Pid) ->
    gen_server:call(Pid, resume).

next(Pid) ->
    gen_server:call(Pid, next).

prev(Pid) ->
    gen_server:call(Pid, prev).

%%% ============================================================================
%%% Internal state of the module.
%%% ============================================================================

-record(state, {
    port    :: term(),
    status  :: playing | paused,
    type    :: playlist | single
}).



%%% ============================================================================
%%% Callbacks for `gen_server'.
%%% ============================================================================

%% @doc
%% Sets up subscription configuration loop.
%%
init(FilePath) ->
    case filename:extension(FilePath) of
        ".m3u" ->
            Port = erlang:open_port({spawn, "mplayer -playlist " ++ FilePath}, [stream, binary, use_stdio]),
            {ok, #state{port = Port, type = playlist}};
        ".mp3" ->
            Port = erlang:open_port({spawn, "mplayer " ++ FilePath}, [stream, binary, use_stdio]),
            {ok, #state{port = Port, type = single}};
        Ext ->
            {stop, {error, {unsupported_extention, Ext}}}
    end.


%% @doc
%% Manage the audio play.
%%
handle_call(stop, _From, State = #state{port = Port}) ->
    true = erlang:port_command(Port, "q"),
    NewState = State#state{status = paused},
    {reply, ok, NewState};

handle_call(pause, _From, State = #state{port = Port}) ->
    true = erlang:port_command(Port, "p"),
    NewState = State#state{status = paused},
    {reply, ok, NewState};

handle_call(resume, _From, State = #state{port = Port}) ->
    true = erlang:port_command(Port, "p"),
    NewState = State#state{status = playing},
    {reply, ok, NewState};

handle_call(next, _From, State = #state{port = Port, type = playlist}) ->
    true = erlang:port_command(Port, ">"),
    NewState = State#state{status = playing},
    {reply, ok, NewState};

handle_call(prev, _From, State = #state{port = Port, type = playlist}) ->
    true = erlang:port_command(Port, "<"),
    NewState = State#state{status = playing},
    {reply, ok, NewState};

handle_call(next, _From, State = #state{type = single}) ->
    {reply, {error, {not_available_for_single_mp3, next}}, State};

handle_call(prev, _From, State = #state{type = single}) ->
    {reply, {error, {not_available_for_single_mp3, prev}}, State};

handle_call(Unknown, _From, State) ->
    {reply, {error, {unknown_command, Unknown}}, State}.


%% @doc
%% Unused.
%%
handle_cast(_Unknown, State) ->
    {noreply, State}.


%% @doc
%% Unused.
%%
handle_info(_Unknown, State) ->
    {noreply, State}.


%% @doc
%% Unused.
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @doc
%% Unused.
%%
terminate(_Reason, _State) ->
    ok.



%%% ============================================================================
%%% Internal functions.
%%% ============================================================================
