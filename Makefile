.PHONY: deps generate

all: deps compile

deps:
	@./rebar get-deps
	@git submodule init
	@git submodule update

compile: deps
	@cp -r veilprotocol/proto src
	@./rebar compile
	@rm -rf src/proto

generate: compile
	@./rebar generate

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

##
## Release targets
##

doc:
	@./rebar doc skip_deps=true

rel: deps compile generate

relclean:
	rm -rf rel/onepanel

##
## Dialyzer
##

# Builds .dialyzer.plt init file. This is internal target, call dialyzer_init instead
.dialyzer.plt:
	-dialyzer --build_plt --output_plt .dialyzer.plt --apps kernel stdlib sasl erts ssl tools runtime_tools crypto inets xmerl snmp public_key eunit syntax_tools compiler ./deps/*/ebin


# Starts dialyzer on whole ./ebin dir. If .dialyzer.plt does not exist, will be generated
dialyzer: compile .dialyzer.plt
	-dialyzer ./ebin --plt .dialyzer.plt -Werror_handling -Wrace_conditions


# Starts full initialization of .dialyzer.plt that is required by dialyzer
dialyzer_init: compile .dialyzer.plt
