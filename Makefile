REPO            ?= onepanel

# distro for package building (oneof: wily, fedora-23-x86_64)
DISTRIBUTION    ?= none
export DISTRIBUTION

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
PKG_ID           = gr-panel-$(PKG_VERSION)
else
CONFIG           = config/oneprovider.config
PKG_VARS_CONFIG  = config/op_pkg.vars.config
PKG_ID           = op-panel-$(PKG_VERSION)
endif

.PHONY: deps generate

all: rel

deps:
	@./rebar --config $(CONFIG) get-deps

compile:
	@./rebar --config $(CONFIG) compile

generate:
	@./rebar --config $(CONFIG) generate ${OVERLAY_VARS}

clean: relclean pkgclean
	@./rebar --config $(CONFIG) clean

distclean:
	@./rebar --config $(CONFIG) delete-deps

##
## Release targets
##

doc:
	@./rebar --config $(CONFIG) doc skip_deps=true

rel: deps compile generate
ifeq ($(REL_TYPE),globalregistry)
	rm -rf rel/gr_panel
	mv rel_globalregistry/gr_panel rel/
else
	rm -rf rel/op_panel
	mv rel_oneprovider/op_panel rel/
endif

relclean:
	rm -rf rel/gr_panel
	rm -rf rel/op_panel
	rm -rf rel_globalregistry/gr_panel
	rm -rf rel_oneprovider/op_panel

##
## Dialyzer targets local
##

PLT ?= .dialyzer.plt

# Builds dialyzer's Persistent Lookup Table file.
.PHONY: plt
plt:
	dialyzer --check_plt --plt ${PLT}; \
	if [ $$? != 0 ]; then \
		dialyzer --build_plt --output_plt ${PLT} --apps kernel stdlib sasl erts \
		ssl tools runtime_tools crypto inets xmerl snmp public_key eunit \
		common_test test_server syntax_tools compiler edoc mnesia hipe \
		ssh webtool -r deps; \
	fi; exit 0

# Dialyzes the project.
dialyzer: plt
	dialyzer ./ebin --plt ${PLT} -Werror_handling -Wrace_conditions --fullpath

##
## Packaging targets
##

export PKG_VERSION PKG_ID PKG_BUILD BASE_DIR ERLANG_BIN REBAR OVERLAY_VARS RELEASE REL_TYPE CONFIG PKG_VARS_CONFIG

check_distribution:
ifeq ($(DISTRIBUTION), none)
	@echo "Please provide package distribution. Oneof: 'wily', 'fedora-23-x86_64'"
	@exit 1
else
	@echo "Building package for distribution $(DISTRIBUTION)"
endif

package/$(PKG_ID).tar.gz: deps
	mkdir -p package
	rm -rf package/$(PKG_ID)
	git archive --format=tar --prefix=$(PKG_ID)/ $(PKG_REVISION) | (cd package && tar -xf -)
	${MAKE} -C package/$(PKG_ID) deps
	for dep in package/$(PKG_ID) package/$(PKG_ID)/deps/*; do \
	     echo "Processing dependency: `basename $${dep}`"; \
	     vsn=`git --git-dir=$${dep}/.git describe --tags 2>/dev/null`; \
	     mkdir -p $${dep}/priv; \
	     echo "$${vsn}" > $${dep}/priv/vsn.git; \
	     sed -i'' "s/{vsn,\\s*git}/{vsn, \"$${vsn}\"}/" $${dep}/src/*.app.src 2>/dev/null || true; \
	done
	find package/$(PKG_ID) -depth -name ".git" -exec rm -rf {} \;
	tar -C package -czf package/$(PKG_ID).tar.gz $(PKG_ID)

dist: package/$(PKG_ID).tar.gz
	cp package/$(PKG_ID).tar.gz .

package: check_distribution package/$(PKG_ID).tar.gz
	${MAKE} -C package -f $(PKG_ID)/deps/node_package/Makefile

pkgclean:
	rm -rf package
