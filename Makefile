.PHONY: test deps generate

all: deps compile

deps:
	@./rebar get-deps

compile:
	@./rebar compile

generate:
	@sed -i s/"-name .*"/"-name spanel@"`hostname`/g rel/files/vm.args
	@./rebar generate

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

rel: deps compile generate

relclean:
	rm -rf rel/spanel
