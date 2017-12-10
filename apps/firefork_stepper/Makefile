REBAR := rebar3

all: compile

deps: _checkouts/firefork_radio
	$(REBAR) get-deps

_checkouts/firefork_radio:
	mkdir -p _checkouts && cd _checkouts && ln -s ../../firefork_radio

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean


.PHONY: all compile clean
