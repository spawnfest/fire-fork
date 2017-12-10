REBAR := rebar3

all: compile

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

itest: compile
	mkdir -p logs
	env ERL_LIBS=deps ERL_FLAGS="-config test/sys.config" $(REBAR) ct --dir itest skip_deps=true $(CT_ARGS)

rtest: compile
	mkdir -p logs
	env ERL_LIBS=deps erl -pa ebin -config test/sys

.PHONY: all compile clean
