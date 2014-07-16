.PHONY: deps generate

all: deps compile

deps:
	@./rebar get-deps

compile: deps
	@./rebar compile

generate: compile
	@./rebar generate

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

rel: deps compile generate

