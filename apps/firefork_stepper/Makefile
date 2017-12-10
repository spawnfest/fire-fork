REBAR := rebar3

all: compile

deps:
	mkdir -p _checkouts
	cd _checkouts && ln -s ../../firefork_radio
	$(REBAR) get-deps

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean


.PHONY: all compile clean
