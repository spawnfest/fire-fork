REBAR := rebar3

all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean --all

release:
	$(REBAR) release --relname firefork_stepper
	$(REBAR) release --relname firefork_station


.PHONY: all compile clean release
