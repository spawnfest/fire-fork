REBAR := rebar3

all: compile

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

check:
	$(REBAR) eunit

clean:
	$(REBAR) clean


.PHONY: deps all compile clean
