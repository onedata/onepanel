.PHONY: test deps generate

all: deps compile

deps:
	@./rebar get-deps

compile:
	@./rebar compile

generate:
	@./rebar generate

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

rel: deps compile generate

relclean:
	rm -rf rel/spanel
