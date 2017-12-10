%%%
%%% Implementation of the KISS protocol.
%%% See http://www.ax25.net/kiss.aspx for more details.
%%%
-module(firefork_radio_kiss).
-export([init/0, encode/2, decode/2]).

-define(FEND,   16#C0).
-define(FESC,   16#DB).
-define(TFEND,  16#DC).
-define(TFESC,  16#DD).
-define(FT_DATA,        0). % Frame type = data frame, port number = 0.
-define(FT_TXDELAY,     1).
-define(FT_P,           2).
-define(FT_SlotTime,    3).
-define(FT_TXtail,      4).
-define(FT_FullDuplex,  5).
-define(FT_SetHardware, 6).
-define(FT_Exit,    16#FF).


%%% ============================================================================
%%% Internal data structures.
%%% ============================================================================

-record(state, {
    name,       %% Current state name.
    type,       %% Type of the current frame: data | time
    part,       %% Partly decoded frame.
    frames      %% Decoded frames.
}).


%%% ============================================================================
%%% Public API.
%%% ============================================================================

%%
%%
%%
init() ->
    {ok, #state{
        name   = idle,
        type   = undefined,
        part   = [],
        frames = []
    }}.

%%
%%  Encode data into a single KISS frame.
%%
encode(Data, State) when is_binary(Data) ->
    encode(?FT_DATA, erlang:binary_to_list(Data), State);

encode(Data, State) when is_list(Data) ->
    encode(?FT_DATA, Data, State).

encode(Cmd, InitialList, State) when is_list(InitialList)->
    EncodedList = [?FEND, Cmd, lists:map(fun escape_byte/1, InitialList), ?FEND],
    {ok, [list_to_binary(EncodedList)], State}.


%%
%%  Decode stream of bytes to KISS frames.
%%
decode(Data, State) when is_binary(Data) ->
    decode(erlang:binary_to_list(Data), State);

decode(Data, State) when is_list(Data) ->
    NewState = #state{frames = Frames} = lists:foldl(fun decode_byte/2, State#state{frames = []}, Data),
    {ok, lists:reverse(Frames), NewState#state{frames = []}}.



%%% ============================================================================
%%% Internal Functions.
%%% ============================================================================

%%
%%  KISS escaping.
%%
escape_byte(?FEND) -> [?FESC, ?TFEND];
escape_byte(?FESC) -> [?FESC, ?TFESC];
escape_byte(Byte) -> Byte.


%%
%%  Encoding.
%%
decode_byte(?FEND, State = #state{name = idle}) -> State#state{name = frame_start};
decode_byte(_Byte, State = #state{name = idle}) -> State#state{name = idle};

decode_byte(?FT_DATA, State = #state{name = frame_start}) -> State#state{name = frame_data, type = data, part = []};
decode_byte(?FEND,    State = #state{name = frame_start}) -> State#state{name = frame_start};
decode_byte(_Byte,    State = #state{name = frame_start}) -> State#state{name = idle};

decode_byte(?FEND, State = #state{name = frame_data})              -> frame_decoded(State);
decode_byte(?FESC, State = #state{name = frame_data})              -> State#state{name = frame_esc};
decode_byte(Byte,  State = #state{name = frame_data, part = Part}) -> State#state{part = [Byte | Part]};

decode_byte(?TFESC, State = #state{name = frame_esc, part = Part}) -> State#state{name = frame_data, part = [?FESC | Part]};
decode_byte(?TFEND, State = #state{name = frame_esc, part = Part}) -> State#state{name = frame_data, part = [?FEND | Part]};
decode_byte(Byte,   State = #state{name = frame_esc, part = Part}) -> State#state{name = frame_data, part = [Byte  | Part]}.


frame_decoded(State = #state{type = data, frames = Frames, part = Part}) ->
    Frame = list_to_binary(lists:reverse(Part)),
    State#state{
        name = frame_start,
        type = undefined,
        part = [],
        frames = [Frame | Frames]
    };

frame_decoded(State) ->
    % Drop frames of unknown types.
    State.


