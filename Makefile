.EXPORT_ALL_VARIABLES:

REPO            ?= onepanel

# distro for package building (e.g.: xenial, bionic, centos-7-x86_64)
DISTRIBUTION    ?= none
export DISTRIBUTION

BASE_DIR        := $(shell pwd)
GIT_URL := $(shell git config --get remote.origin.url | sed -e 's/\(\/[^/]*\)$$//g')
GIT_URL := $(shell if [ "${GIT_URL}" = "file:/" ]; then echo 'ssh://git@git.onedata.org:7999/vfs'; else echo ${GIT_URL}; fi)
ONEDATA_GIT_URL := $(shell if [ "${ONEDATA_GIT_URL}" = "" ]; then echo ${GIT_URL}; else echo ${ONEDATA_GIT_URL}; fi)
export ONEDATA_GIT_URL

RELEASE         ?= 2002
PKG_REVISION    ?= $(shell git describe --tags --always)
PKG_VERSION     ?= $(shell git describe --tags --always | tr - .)
PKG_BUILD       := 1
PKG_VARS_CONFIG := rel/pkg.vars.config
ERLANG_BIN      := $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar3
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

LIB_DIR          = _build/default/lib
REL_DIRS         = _build/default/rel
OVERLAY_VARS    ?= --overlay_vars=rel/vars.config

BUILD_VERSION := $(subst $(shell git describe --tags --abbrev=0)-,,$(shell git describe --tags --long))

all: rel

.PHONY: get-deps
get-deps: config
	$(REBAR) get-deps

.PHONY: upgrade
upgrade: config
	$(REBAR) upgrade

.PHONY: compile
compile: config
	$(REBAR) compile

.PHONY: generate
generate: template config inject-gui
	$(REBAR) release $(OVERLAY_VARS)

.PHONY: clean
clean: config relclean pkgclean
	$(REBAR) clean

.PHONY: distclean
distclean: config
	$(REBAR) clean --all

.PHONY: template
template:
	$(TEMPLATE_SCRIPT) $(TEMPLATE_CONFIG) ./rel/pkg.vars.config.template
	$(TEMPLATE_SCRIPT) $(TEMPLATE_CONFIG) ./rel/files/app.config.template
	sed "s/{build_version, \".*\"}/{build_version, \"${BUILD_VERSION}\"}/" ./rel/vars.config.template > ./rel/vars.config
	$(TEMPLATE_SCRIPT) rel/vars.config ./rel/files/vm.args.template

config:
	$(TEMPLATE_SCRIPT) $(TEMPLATE_CONFIG) ./rebar.config.template

inject-gui:
	$(LIB_DIR)/gui/pull-gui.sh gui-image.conf


##
## Submodules
##

submodules:
	git submodule sync --recursive ${submodule}
	git submodule update --init --recursive ${submodule}


##
## Testing targets
##

.PHONY: eunit
eunit: config
	$(REBAR) do eunit skip_deps=true --suite=${SUITES}
## Rename all tests in order to remove duplicated names (add _(++i) suffix to each test)
	@for tout in `find test -name "TEST-*.xml"`; do awk '/testcase/{gsub("_[0-9]+\"", "_" ++i "\"")}1' $$tout > $$tout.tmp; mv $$tout.tmp $$tout; done

eunit-with-cover: config
	$(REBAR) do eunit skip_deps=true --suite=${SUITES}, cover
## Rename all tests in order to remove duplicated names (add _(++i) suffix to each test)
	@for tout in `find test -name "TEST-*.xml"`; do awk '/testcase/{gsub("_[0-9]+\"", "_" ++i "\"")}1' $$tout > $$tout.tmp; mv $$tout.tmp $$tout; done

.PHONY: coverage
coverage:
	$(BASE_DIR)/bamboos/docker/coverage.escript $(BASE_DIR)

##
## Release targets
##

.PHONY: doc
doc: config
	@$(REBAR) edoc skip_deps=true

.PHONY: rel
rel: config compile generate

.PHONY: relclean
relclean:
	rm -rf _build/default/rel/onepanel

##
## Dialyzer targets local
##

# Dialyzes the project.
dialyzer: config
	$(REBAR) dialyzer

##
## Packaging targets
##

check_distribution:
ifeq ($(DISTRIBUTION), none)
	@echo "Please provide package distribution. Oneof: 'xenial', 'bionic', 'centos-7-x86_64'"
	@exit 1
else
	@echo "Building package for distribution $(DISTRIBUTION)"
endif

package/$(PKG_ID).tar.gz:
	mkdir -p package
	rm -rf package/$(PKG_ID)
	git archive --format=tar --prefix=$(PKG_ID)/ $(PKG_REVISION) | (cd package && tar -xf -)
	git submodule foreach --recursive "git archive --prefix=$(PKG_ID)/\$$path/ \$$sha1 | (cd \$$toplevel/package && tar -xf -)"
	${MAKE} -C package/$(PKG_ID) config get-deps inject-gui template
	for dep in package/$(PKG_ID) package/$(PKG_ID)/$(LIB_DIR)/*; do \
	     echo "Processing dependency: `basename $${dep}`"; \
	     vsn=`git --git-dir=$${dep}/.git describe --tags 2>/dev/null`; \
	     mkdir -p $${dep}/priv; \
	     echo "$${vsn}" > $${dep}/priv/vsn.git; \
	     sed -i'' "s/{vsn,\\s*git}/{vsn, \"$${vsn}\"}/" $${dep}/src/*.app.src 2>/dev/null || true; \
	done
	tar -C package -czf package/$(PKG_ID).tar.gz $(PKG_ID)

dist: package/$(PKG_ID).tar.gz
	cp package/$(PKG_ID).tar.gz .

package: check_distribution package/$(PKG_ID).tar.gz
	${MAKE} -C package -f $(PKG_ID)/node_package/Makefile

pkgclean:
	rm -rf package
