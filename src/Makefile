########################################################################
# Makefile for the Curry->Prolog compiler system of PAKCS
########################################################################

# bin directory to store executables of PAKCS:
ifndef BINDIR
BINDIR = $(dir $(CURDIR))/bin
endif

# The SICStus Prolog system:
SICSTUS=$(BINDIR)/sicstusprolog

# The SWI-Prolog system:
SWI=$(BINDIR)/swiprolog

# Standard options for SWI-Prolog: quiet and optimized compilation
SWIOPTIONS=-q -O
# If there is a local stack overflow during compilation (e.g., reported
# by a message like "ERROR: local"), one should increase the stack sizes
# by redefining this definition according to version of SWI-Prolog:
# SWI-Prolog 7.*: use 2GB for the local stack:
#SWIOPTIONS=-q -L2G -G0 -T0 -O
# SWI-Prolog 8.*: use 4GB for all stacks:
#SWIOPTIONS=-q --stack_limit=4g -O

# library directory of PAKCS:
ifndef LIBDIR
LIBDIR = $(dir $(CURDIR))/lib
endif

# names of run-time libraries:
PRIMLIBSRC = `cd lib_src && echo *.pl`

.PHONY: install
install:
	$(MAKE) uninstall
	@if [ -x "$(SICSTUS)" ] ; then $(MAKE) sicstus ; elif [ -x "$(SWI)" ] ; then $(MAKE) swi ; else echo "No Prolog defined to generate Curry->Prolog compiler" ; exit 1 ; fi
	../scripts/makesavedstate c2p.state
	rm -f pakcs
	mv c2p.state pakcs
	cd $(BINDIR) && ln -s pakcs curry

.PHONY: uninstall
uninstall:
	rm -f $(BINDIR)/curry

# make everything if we have a SICStus-Prolog system:
.PHONY: sicstus
sicstus:
	echo "[sicstusbasics], generatePrologBasics." | "$(SICSTUS)"
	$(MAKE) c2p.state

# make everything if we have a SWI-Prolog system:
.PHONY: swi
swi:
	rm -f prologbasics.pl && cp swibasics.pl prologbasics.pl
	$(MAKE) swi.state
	mv -f swi.state c2p.state

#
# generate a saved state of the Curry->Prolog Compiler System w.r.t. SICStus-Prolog
#
c2p.state: prologbasics.pl basics.pl version.pl loader.pl evaluator.pl c2p.pl \
	      compiler.pl external.pl readPrimFile.pl readFlcFromFcy.pl \
	      readXml.pl $(LIBDIR)/.curry/pakcs-$(VERSION)/Prelude.fcy
	echo "compile(c2p),c2p('$(LIBDIR)/Prelude'),compile('$(LIBDIR)/.curry/pakcs-$(VERSION)/Prelude'),loadAndCompile('$(LIBDIR)/.curry/pakcs-$(VERSION)/Prelude.pl',[],create),saveprog_entry('c2p.state',user:pakcsMain)." \
           | "$(SICSTUS)"

#
# generate a saved state of the Curry->Prolog Compiler System w.r.t. SWI-Prolog
#
swi.state: prologbasics.pl basics.pl version.pl loader.pl evaluator.pl c2p.pl \
	      compiler.pl external.pl readPrimFile.pl readFlcFromFcy.pl \
	      readXml.pl $(LIBDIR)/.curry/pakcs-$(VERSION)/Prelude.fcy
	echo "compile(c2p). c2p('$(LIBDIR)/Prelude'), compile('$(LIBDIR)/.curry/pakcs-$(VERSION)/Prelude'),loader:loadAndCompile('$(LIBDIR)/.curry/pakcs-$(VERSION)/Prelude.pl',[],create). saveprog_entry('swi.state',user:pakcsMain)." | "$(SWI)" $(SWIOPTIONS)

$(LIBDIR)/.curry/pakcs-$(VERSION)/Prelude.fcy: $(LIBDIR)/Prelude.curry
	cd $(LIBDIR) && $(MAKE) .curry/pakcs-$(VERSION)/Prelude.fcy

# clean up everything
.PHONY: clean
clean: uninstall
	rm -f c2p.state pakcs $(LIBDIR)/.curry/pakcs-$(VERSION)/Prelude.pl prologbasics.pl
	rm -f pakcsversion.pl
