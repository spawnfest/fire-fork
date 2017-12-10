REBAR := rebar3

all: compile

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

rtest: compile
	mkdir -p logs
	env ERL_LIBS=deps erl -pa ebin -config test/sys

.PHONY: all compile clean
