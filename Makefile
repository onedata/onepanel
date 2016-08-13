REPO            ?= onepanel

# distro for package building (oneof: wily, fedora-23-x86_64)
DISTRIBUTION    ?= none
export DISTRIBUTION

BASE_DIR        := $(shell pwd)
GIT_URL := $(shell git config --get remote.origin.url | sed -e 's/\(\/[^/]*\)$$//g')
GIT_URL := $(shell if [ "${GIT_URL}" = "file:/" ]; then echo 'ssh://git@git.plgrid.pl:7999/vfs'; else echo ${GIT_URL}; fi)
ONEDATA_GIT_URL := $(shell if [ "${ONEDATA_GIT_URL}" = "" ]; then echo ${GIT_URL}; else echo ${ONEDATA_GIT_URL}; fi)
export ONEDATA_GIT_URL

PKG_REVISION    ?= $(shell git describe --tags --always)
PKG_VERSION     ?= $(shell git describe --tags --always | tr - .)
PKG_BUILD       := 1
PKG_VARS_CONFIG := rel/pkg.vars.config
ERLANG_BIN      := $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar
TEMPLATE_SCRIPT := ./rel/templates/overlay.escript

ifeq ($(strip $(REL_TYPE)),onezone)
TEMPLATE_CONFIG := ./rel/templates/onezone.config
PKG_ID          := oz-panel-$(PKG_VERSION)
else ifeq ($(strip $(REL_TYPE)),oneprovider)
TEMPLATE_CONFIG := ./rel/templates/oneprovider.config
PKG_ID          := op-panel-$(PKG_VERSION)
else
TEMPLATE_CONFIG := ./rel/templates/dev.config
PKG_ID          := onepanel-$(PKG_VERSION)
endif

all: rel

.PHONY: deps
deps:
	./rebar get-deps

.PHONY: compile
compile:
	./rebar compile

.PHONY: generate
generate: template
	./rebar generate $(OVERLAY_VARS)

.PHONY: clean
clean: relclean pkgclean
	./rebar clean

.PHONY: distclean
distclean:
	./rebar delete-deps

.PHONY: template
template:
	$(TEMPLATE_SCRIPT) $(TEMPLATE_CONFIG) ./rel/pkg.vars.config.template
	$(TEMPLATE_SCRIPT) $(TEMPLATE_CONFIG) ./rel/reltool.config.template
	$(TEMPLATE_SCRIPT) $(TEMPLATE_CONFIG) ./rel/vars.config.template

##
## Testing targets
##

.PHONY: eunit
eunit:
	./rebar eunit skip_deps=true suites=${SUITES}
## Rename all tests in order to remove duplicated names (add _(++i) suffix to each test)
	@for tout in `find test -name "TEST-*.xml"`; do awk '/testcase/{gsub("_[0-9]+\"", "_" ++i "\"")}1' $$tout > $$tout.tmp; mv $$tout.tmp $$tout; done

.PHONY: coverage
coverage:
	$(BASE_DIR)/bamboos/docker/coverage.escript $(BASE_DIR)

##
## Release targets
##

.PHONY: doc
doc:
	@./rebar doc skip_deps=true

.PHONY: rel
rel: deps compile generate

.PHONY: relclean
relclean:
	rm -rf rel/onepanel

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
		mnesia edoc common_test test_server syntax_tools compiler ./deps/*/ebin; \
	fi; exit 0


# Dialyzes the project.
.PHONY: dialyzer
dialyzer: plt
	dialyzer ./ebin --plt ${PLT} -Werror_handling -Wrace_conditions --fullpath

##
## Packaging targets
##

export PKG_VERSION PKG_ID PKG_BUILD BASE_DIR ERLANG_BIN REBAR OVERLAY_VARS RELEASE PKG_VARS_CONFIG

check_distribution:
ifeq ($(DISTRIBUTION), none)
	@echo "Please provide package distribution. Oneof: 'wily', 'fedora-23-x86_64'"
	@exit 1
else
	@echo "Building package for distribution $(DISTRIBUTION)"
endif

package/$(PKG_ID).tar.gz:
	mkdir -p package
	rm -rf package/$(PKG_ID)
	git archive --format=tar --prefix=$(PKG_ID)/ $(PKG_REVISION) | (cd package && tar -xf -)
	${MAKE} -C package/$(PKG_ID) deps template
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
