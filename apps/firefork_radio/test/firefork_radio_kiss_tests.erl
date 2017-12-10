%%%
%%% Unit tests for the `firefork_radio_kiss' module.
%%%
-module(firefork_radio_kiss_tests).
-compile([{parse_transform, lager_transform}]).
-include_lib("eunit/include/eunit.hrl").


%%
%%  Check, if encoding/decoding works.
%%
codec_test() ->
    Message = erlang:term_to_binary({message, param, param2}),
    {ok, Codec} = firefork_radio_kiss:init(),
    {ok, [EncodedFrame], CodecEnc} = firefork_radio_kiss:encode(Message, Codec),
    {ok, [Message],     _CodecDec} = firefork_radio_kiss:decode(EncodedFrame, CodecEnc),
    ok.


