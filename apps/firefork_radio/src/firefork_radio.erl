%%% @doc
%%% Main interface for the `firefork_radio' appplication.
%%%
-module(firefork_radio).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([start_link/3, send/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%% ============================================================================
%%% Callback definitions.
%%% ============================================================================

%%
%%  This callback is called on each incoming message.
%%
-callback handle_message(
        Message :: term()
    ) ->
        ok.



%%% ============================================================================
%%% API functions
%%% ============================================================================

%%  @doc
%%  Starts the server.
%%
start_link(Name, CbModule, Opts) ->
    gen_server:start_link(Name, ?MODULE, {CbModule, Opts}, []).


%%
%%  Send the message via the radio interface.
%%
send(Name, Message) ->
    Payload = erlang:term_to_binary(Message),
    CRC = erlang:crc32(Payload),
    Frame = <<CRC:32/unsigned-big, Payload/binary>>,
    gen_server:cast(Name, {send, Frame}).



%%% ============================================================================
%%% Internal state.
%%% ============================================================================

%%
%%
%%
-record(state, {
    uart     :: term(),
    cb_mod   :: module(),
    codec    :: term()
}).



%%% ============================================================================
%%% Callbacks for the `gen_server'.
%%% ============================================================================

%%
%%  Initialization.
%%
init({CbModule, Opts}) ->
    {ok, Uart} = uart:open(
        maps:get(uart_device, Opts, "/dev/ttyS0"),
        maps:get(uart_opts, Opts, [
            {baud, 9600}, {csize, 8}, {parity, none}, {stopb, 1}, % 9600, 8N1
            {active, true}, {delay_send, false}
        ])
    ),
    {ok, Codec} = firefork_radio_kiss:init(),
    State = #state{
        uart   = Uart,
        cb_mod = CbModule,
        codec  = Codec
    },
    {ok, State}.


%%
%%  Synchronous calls.
%%
handle_call(Unknown, _From, State) ->
    lager:warning("Dropping unknown call: ~p", [Unknown]),
    {reply, undefined, State}.


%%
%%  Asynchronous calls.
%%
handle_cast({send, Frame}, State = #state{uart = Uart, codec = Codec}) ->
    {ok, EncodedFrames, NewCodec} = firefork_radio_kiss:encode(Frame, Codec),
    ok = lists:foreach(fun (EncodedFrame) ->
        ok = send:uart(Uart, EncodedFrame)
    end, EncodedFrames),
    NewState = State#state{
        codec = NewCodec
    },
    {noreply, NewState};

handle_cast(Unknown, State) ->
    lager:warning("Dropping unknown cast: ~p", [Unknown]),
    {noreply, State}.


%%
%%  Other messages.
%%
handle_info({uart, Uart, IncomingMsg}, State = #state{uart = Uart, codec = Codec, cb_mod = CbModule}) ->
    {ok, DecodedFrames, NewCodec} = firefork_radio_kiss:decode(IncomingMsg, Codec),
    ok = lists:foreach(fun (DecodedFrame) -> handle_frame(DecodedFrame, CbModule) end, DecodedFrames),
    NewState = State#state{
        codec = NewCodec
    },
    {noreply, NewState};

handle_info(Unknown, State) ->
    lager:warning("Dropping unknown info: ~p", [Unknown]),
    {noreply, State}.


%%
%%  Process termination.
%%
terminate(_Reason, _State) ->
    ok.


%%
%%  Code upgrades.
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%% ============================================================================
%%% Internal functions.
%%% ============================================================================

%%
%%
%%
handle_frame(Frame, CbModule) when is_binary(Frame) ->
    try
        <<CRC:32/unsigned-big, Payload/binary>> = Frame,
        CRC = erlang:crc32(Payload),
        ok = CbModule:handle_message(erlang:binary_to_term(Payload)),
        ok
    catch
        ErrClass:ErrReason ->
            lager:debug(
                "Dropping frame ~p, reason=~p:~p, stacktrace=~p",
                [Frame, ErrClass, ErrReason, erlang:get_stacktrace()]
            ),
            ok
    end.


