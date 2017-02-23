##############################################################################
# Installation of PAKCS shell scripts
##############################################################################

# the scripts to be installed
BINS = $(BINDIR)/pakcs $(BINDIR)/pakcs-makecgi \
       $(BINDIR)/cleancurry \
       $(BINDIR)/pakcs-fcypp


.PHONY: all
all:
	@if [ ! -d $(BINDIR) ] ; then mkdir -p $(BINDIR) ; fi
	$(MAKE) $(BINS)

# install some script of PAKCS in the bin directory:
$(BINDIR)/%: %.sh
	mkdir -p $(@D)
	cat $< | sed "s|^PAKCSBUILDDIR=.*$$|PAKCSBUILDDIR=$(ROOT)|" | \
	 sed "s|^PAKCSINSTALLDIR=.*$$|PAKCSINSTALLDIR=$(PAKCSINSTALLDIR)|" > $@
	chmod 755 $@

.PHONY: clean
clean:
	rm -f $(BINS)
