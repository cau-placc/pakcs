##############################################################################
# Installation of PAKCS shell scripts
##############################################################################

# the scripts to be installed
BINS = $(BINDIR)/pakcs $(BINDIR)/cleancurry $(BINDIR)/pakcs-fcypp


.PHONY: all
all:
	@if [ ! -d $(BINDIR) ] ; then mkdir -p $(BINDIR) ; fi
	$(MAKE) $(BINS)

# install some script of PAKCS in the bin directory:
$(BINDIR)/%: %.sh ../Makefile
	mkdir -p $(@D)
	cp $< $@
	chmod 755 $@

.PHONY: clean
clean:
	rm -f $(BINS)
