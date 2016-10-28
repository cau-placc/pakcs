##############################################################################
# Installation of PAKCS shell scripts
##############################################################################

# the scripts to be installed
BINS = $(BINDIR)/pakcs $(BINDIR)/makecurrycgi \
       $(BINDIR)/.makesavedstate $(BINDIR)/cleancurry \
       $(BINDIR)/fcypp # $(BINDIR)/runcurry


.PHONY: all
all:
	@if [ ! -d $(BINDIR) ] ; then mkdir -p $(BINDIR) ; fi
	$(MAKE) $(BINS)

# install some script of PAKCS in the bin directory:
$(BINDIR)/%: %.sh
	mkdir -p $(@D)
	sed "s|^PAKCSHOME=.*$$|PAKCSHOME=$(ROOT)|" < $< > $@
	chmod 755 $@

.PHONY: clean
clean:
	rm -f $(BINS)
