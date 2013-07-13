OLDVER  = `head -n1 VERSION`
NEWVER  = $(OLDVER)
PKGNAME = grizzl
FILES   = grizzl-core.el grizzl-read.el grizzl.el grizzl-pkg.el README.md

all: units package

units:
	emacs -batch -l ert -l cl -l grizzl-core.el -l test/grizzl-core-test.el -f ert-run-tests-batch-and-exit

reversion:
	perl -pi -e "s/$(OLDVER)/$(NEWVER)/g" *.el
	echo $(NEWVER) > VERSION

package:
	mkdir -p $(PKGNAME)-$(NEWVER)
	cp $(FILES) $(PKGNAME)-$(NEWVER)/
	tar cvf $(PKGNAME)-$(NEWVER).tar $(PKGNAME)-$(NEWVER)
	rm -r $(PKGNAME)-$(NEWVER)
