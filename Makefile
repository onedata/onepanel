REL_TYPE         = globalregistry # This ca be: globalregistry | oneprovider

REPO		        ?= onepanel

PKG_REVISION    ?= $(shell git describe --tags --always)
PKG_VERSION	    ?= $(shell git describe --tags --always | tr - .)
PKG_BUILD        = 1
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar
OVERLAY_VARS    ?=

ifeq ($(REL_TYPE),globalregistry)
CONFIG           = config/globalregistry.config
PKG_VARS_CONFIG  = config/gr_pkg.vars.config
PKG_ID           = gr-onepanel-$(PKG_VERSION)
else
CONFIG           = config/oneprovider.config
PKG_VARS_CONFIG  = config/op_pkg.vars.config
PKG_ID           = op-onepanel-$(PKG_VERSION)
endif

##
## Export all variables to sub-invocation
##
export

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
ifeq ($(REL_TYPE),globalregistry)
	mv -f rel_globalregistry/gr_onepanel rel/
else
	mv -f rel_oneprovider/op_onepanel rel/
endif

relclean:
ifeq ($(REL_TYPE),globalregistry)
	rm -rf rel/gr_onepanel
	rm -rf rel_globalregistry/gr_onepanel
else
	rm -rf rel/op_onepanel
	rm -rf rel_oneprovider/op_onepanel
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

##
## Packaging targets
##

# export PKG_VERSION PKG_ID PKG_BUILD BASE_DIR ERLANG_BIN REBAR OVERLAY_VARS RELEASE REL_TYPE CONFIG PKG_VARS_CONFIG

package.src: deps
	mkdir -p package
	rm -rf package/$(PKG_ID)
	git archive --format=tar --prefix=$(PKG_ID)/ $(PKG_REVISION)| (cd package && tar -xf -)
	${MAKE} -C package/$(PKG_ID) deps
	mkdir -p package/$(PKG_ID)/priv
	git --git-dir=.git describe --tags --always >package/$(PKG_ID)/priv/vsn.git
	for dep in package/$(PKG_ID)/deps/*; do \
             echo "Processing dep: $${dep}"; \
             mkdir -p $${dep}/priv; \
             git --git-dir=$${dep}/.git describe --tags >$${dep}/priv/vsn.git; \
        done
	find package/$(PKG_ID) -depth -name ".git" -exec rm -rf {} \;
	tar -C package -czf package/$(PKG_ID).tar.gz $(PKG_ID)

dist: package.src
	cp package/$(PKG_ID).tar.gz .

package: package.src
	REL_TYPE=${REL_TYPE} ${MAKE} -C package -f $(PKG_ID)/deps/node_package/Makefile

pkgclean: distclean
	rm -rf package