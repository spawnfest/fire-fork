REBAR := rebar3

all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

release:
	$(REBAR) release --relname firefork_stepper
	$(REBAR) release --relname firefork_station


.PHONY: all compile clean release
