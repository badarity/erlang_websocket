REPO		?= erlang_websocket
ERLANG_WEBSOCKET_TAG	 = $(shell git describe --tags)
REVISION	?= $(shell echo $(ERLANG_WEBSOCKET_TAG) | sed -e 's/^$(REPO)-//')
PKG_VERSION	?= $(shell echo $(REVISION) | tr - .)

PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar

.PHONY: rel deps

all: clean deps compile xref test

deps:
	@$(REBAR) update-deps
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

xref:
	@$(REBAR) xref

clean:
	@$(REBAR) clean

edoc:
	@$(REBAR) doc

distclean: clean relclean ballclean
	@$(REBAR) delete-deps

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

##
## Release targets
##
rel: deps
	./rebar compile generate

rel4fedora:
	@mkdir -p deps
	./rebar -C package/rpm/rebar.config compile generate

relclean:
	rm -rf rel/erlang_websocket

build_plt:
	@$(REBAR) build-plt

dialyzer:
	@$(REBAR) dialyze

app:
	@$(REBAR) create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)

# Release tarball creation
# Generates a tarball that includes all the deps sources so no checkouts are necessary
archivegit = git archive --format=tar --prefix=$(1)/ HEAD | (cd $(2) && tar xf -)
archivehg = hg archive $(2)/$(1)
archive = if [ -d ".git" ]; then \
		$(call archivegit,$(1),$(2)); \
	    else \
		$(call archivehg,$(1),$(2)); \
	    fi

buildtar = mkdir distdir && \
		 git clone . distdir/erlang_websocket-clone && \
		 cd distdir/erlang_websocket-clone && \
		 git checkout $(ERLANG_WEBSOCKET_TAG) && \
		 $(call archive,$(ERLANG_WEBSOCKET_TAG),..) && \
		 mkdir ../$(ERLANG_WEBSOCKET_TAG)/deps && \
		 make deps; \
		 for dep in deps/*; do \
                     cd $${dep} && \
                     $(call archive,$${dep},../../../$(ERLANG_WEBSOCKET_TAG)) && \
                     mkdir -p ../../../$(ERLANG_WEBSOCKET_TAG)/$${dep}/priv && \
                     git rev-list --max-count=1 HEAD > ../../../$(ERLANG_WEBSOCKET_TAG)/$${dep}/priv/git.vsn && \
                     cd ../..; done

distdir:
	$(if $(ERLANG_WEBSOCKET_TAG), $(call buildtar), $(error "You can't generate a release tarball from a non-tagged revision. Run 'git checkout <tag>', then 'make dist'"))

dist $(ERLANG_WEBSOCKET_TAG).tar.gz: distdir
	cd distdir; \
	tar czf ../$(ERLANG_WEBSOCKET_TAG).tar.gz $(ERLANG_WEBSOCKET_TAG)

buildtar4f = mkdir distdir && \
		 git clone . distdir/erlang_websocket-clone && \
		 cd distdir/erlang_websocket-clone && \
		 git checkout $(ERLANG_WEBSOCKET_TAG) && \
		 $(call archive,$(ERLANG_WEBSOCKET_TAG),..)

distdir4f:
	$(if $(ERLANG_WEBSOCKET_TAG), $(call buildtar4f), $(error "You can't generate a release tarball from a non-tagged revision. Run 'git checkout <tag>', then 'make dist'"))

dist4fedora $(ERLANG_WEBSOCKET_TAG).tar.gz: distdir4f
	cd distdir; \
	tar czf ../$(ERLANG_WEBSOCKET_TAG).tar.gz $(ERLANG_WEBSOCKET_TAG)

ballclean:
	rm -rf $(ERLANG_WEBSOCKET_TAG).tar.gz distdir

package: dist
	$(MAKE) -C package package

pkgclean:
	$(MAKE) -C package pkgclean

.PHONY: package
export PKG_VERSION REPO REVISION ERLANG_WEBSOCKET_TAG
