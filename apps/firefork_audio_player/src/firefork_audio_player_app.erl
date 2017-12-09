%%%-------------------------------------------------------------------
%% @doc firefork_audio_player public API
%% @end
%%%-------------------------------------------------------------------

-module(firefork_audio_player_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    firefork_audio_player_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================