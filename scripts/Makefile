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
	# for backward compatibility
	rm -f $(BINDIR)/makecurrycgi
	ln -s $(BINDIR)/pakcs-makecgi $(BINDIR)/makecurrycgi

# install some script of PAKCS in the bin directory:
$(BINDIR)/%: %.sh
	mkdir -p $(@D)
	sed "s|^PAKCSHOME=.*$$|PAKCSHOME=$(ROOT)|" < $< > $@
	chmod 755 $@

.PHONY: clean
clean:
	rm -f $(BINS)
