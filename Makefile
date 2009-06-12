VERSION = `grep '^;;; Version:' tap-mode.el | cut -d: -f2 | sed 's/ //g'`
BASENAME = emacs-tap-mode
DISTNAME = $(BASENAME)-$(VERSION)

all:
	@echo Nothing to do. Try make dist.

dist:
	@mkdir -p $(DISTNAME)
	@V=$(VERSION) ; echo VERSION: $$V
	@V=$(VERSION) perl -pni -e 's/^(    |)version: \d+\.\d+/$$1version: $$ENV{V}/' META.yml
	@cp tap-mode.el README ChangeLog META.yml $(DISTNAME)/
	@tar czf $(DISTNAME).tgz $(DISTNAME)
	@/bin/rm -fr $(DISTNAME)

clean:
	/bin/rm -f $(BASENAME)-[0-9].[0-9]*.tgz
