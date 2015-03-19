REPO		        ?= oneprovider

PKG_REVISION    ?= $(shell git describe --tags --always)
PKG_VERSION	    ?= $(shell git describe --tags --always | tr - .)
PKG_ID           = oneprovider-$(PKG_VERSION)
PKG_BUILD        = 1
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar
OVERLAY_VARS    ?=

CONFIG=config/oneprovider.config

.PHONY: deps generate

all: rel

deps:
	@./rebar --config $(CONFIG) get-deps

compile:
	@./rebar --config $(CONFIG) compile

generate:
	@./rebar --config $(CONFIG) generate ${OVERLAY_VARS}

clean:
	@./rebar --config $(CONFIG) clean

distclean: clean
	@./rebar --config $(CONFIG) delete-deps

##
## Release targets
##

doc:
	@./rebar --config $(CONFIG) doc skip_deps=true

rel: deps compile generate

relclean:
ifeq ($(CONFIG),config/globalregistry.config)
	rm -rf rel_globalregistry/onepanel
else
	rm -rf rel_oneprovider/onepanel
endif

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
