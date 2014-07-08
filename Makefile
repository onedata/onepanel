.PHONY: deps generate

all: deps compile

deps:
	@./rebar get-deps

compile: deps
	@./rebar compile

generate: compile
	@sed -i s/"-name .*"/"-name onepanel@"`hostname`/g rel/files/vm.args
	@./rebar generate

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

rel: deps compile generate

