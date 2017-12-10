REBAR := rebar3

all: compile

deps: _checkouts/firefork_radio _checkouts/firefork_audio_player
	$(REBAR) get-deps

_checkouts/firefork_radio:
	mkdir -p _checkouts && cd _checkouts && ln -s ../../firefork_radio

_checkouts/firefork_audio_player:
	mkdir -p _checkouts && cd _checkouts && ln -s ../../firefork_audio_player

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

.PHONY: deps all compile clean
