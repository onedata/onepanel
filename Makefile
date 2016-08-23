REPO            ?= onepanel

# distro for package building (oneof: wily, fedora-23-x86_64)
DISTRIBUTION    ?= none
export DISTRIBUTION

PKG_REVISION    ?= $(shell git describe --tags --always)
PKG_VERSION     ?= $(shell git describe --tags --always | tr - .)
PKG_BUILD        = 1
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar
OVERLAY_VARS    ?=

ifeq ($(REL_TYPE),onezone)
CONFIG           = config/onezone.config
PKG_VARS_CONFIG  = config/oz_pkg.vars.config
PKG_ID           = oz-panel-$(PKG_VERSION)
else
CONFIG           = config/oneprovider.config
PKG_VARS_CONFIG  = config/op_pkg.vars.config
PKG_ID           = op-panel-$(PKG_VERSION)
endif

BASE_DIR         = $(shell pwd)
GIT_URL := $(shell git config --get remote.origin.url | sed -e 's/\(\/[^/]*\)$$//g')
GIT_URL := $(shell if [ "${GIT_URL}" = "file:/" ]; then echo 'ssh://git@git.plgrid.pl:7999/vfs'; else echo ${GIT_URL}; fi)
ONEDATA_GIT_URL := $(shell if [ "${ONEDATA_GIT_URL}" = "" ]; then echo ${GIT_URL}; else echo ${ONEDATA_GIT_URL}; fi)
export ONEDATA_GIT_URL

.PHONY: deps generate

all: rel

deps:
	@./rebar --config $(CONFIG) get-deps

compile:
	@./rebar --config $(CONFIG) compile

generate:
	@./rebar --config $(CONFIG) generate $(OVERLAY_VARS)

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
ifeq ($(REL_TYPE),onezone)
	rm -rf rel/oz_panel
	mv rel_onezone/oz_panel rel/
else
	rm -rf rel/op_panel
	mv rel_oneprovider/op_panel rel/
endif

relclean:
	rm -rf rel/oz_panel
	rm -rf rel/op_panel
	rm -rf rel_onezone/oz_panel
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
		common_test syntax_tools compiler edoc mnesia hipe \
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
