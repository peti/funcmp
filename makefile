# build haddock documentation, and release archive

PACKAGE     := funcmp
RELEASE     := `date --iso-8601`
DISTARCHIVE := $(PACKAGE)-$(RELEASE).tar.gz
DISTFILES   := README COPYING ChangeLog src doc texmf
GHCURL      := http://haskell.org/ghc/docs/latest/html/libraries
GHCPREFIX   := /usr/local/ghc-current/share/ghc-6.3/html/libraries
GHCFLAGS    := -Wall -O2 -funbox-strict-fields -hidir .objs -odir .objs

.PHONY: all clean dist

all::	$(DISTFILES)

dist::		all index.html
	@rm -rf $(DISTARCHIVE) $(PACKAGE)-$(RELEASE)
	@mkdir $(PACKAGE)-$(RELEASE)
	@cp -rp $(DISTFILES) $(PACKAGE)-$(RELEASE)/
	@find $(PACKAGE)-$(RELEASE) -name CVS | xargs rm -rf
	@echo Created $(DISTARCHIVE).
	@tar cfvz $(DISTARCHIVE) $(PACKAGE)-$(RELEASE)
	@rm -rf $(PACKAGE)-$(RELEASE)

index.html:	README
	@lhs2html $<
	@sed -e 's%<p>\(.*Homepage\]</a></p>\)%<p class="title">\1%' <$<.html >$@
	@rm -f README.html

clean::
	@rm -rf docs
	@rm -f README.html index.html $(PACKAGE)-*.tar.gz

distclean::


redate::
	redate README

init-src::
	@-mkdir .objs
	@rm -f MT/monotonerc
	@ln -s ../.monotonerc MT/monotonerc
