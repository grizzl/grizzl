OLDVER  = `head -n1 VERSION`
NEWVER  = $(OLDVER)
PKGNAME = grizzl
FILES   = grizzl.el README.md

all: units package

units:
	emacs -batch -l ert -l cl -l grizzl.el -l test/grizzl-test.el -f ert-run-tests-batch-and-exit

reversion:
	perl -pi -e "s/$(OLDVER)/$(NEWVER)/g" *.el
	echo $(NEWVER) > VERSION

package:
	mkdir -p $(PKGNAME)-$(NEWVER)
	cp $(FILES) $(PKGNAME)-$(NEWVER)/
	tar cvf $(PKGNAME)-$(NEWVER).tar $(PKGNAME)-$(NEWVER)
	rm -r $(PKGNAME)-$(NEWVER)
