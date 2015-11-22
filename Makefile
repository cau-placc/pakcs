#****************************************************************************
# PAKCS: The Portland Aachen Kiel Curry System
# ============================================
#
# A Prolog-based implementation of the functional logic language Curry
# developed by
#
# Sergio Antoy, Bernd Brassel, Martin Engelke, Michael Hanus, Klaus Hoeppner,
# Johannes Koj, Philipp Niederau, Ramin Sadre, Frank Steiner
#
# (contact: pakcs@curry-language.org)
#****************************************************************************

# Some information about this installation
# ----------------------------------------

# The major version numbers:
MAJORVERSION=1
# The minor version number:
MINORVERSION=14
# The revision version number:
REVISIONVERSION=0
# The build version number:
BUILDVERSION=1
# Complete version:
VERSION=$(MAJORVERSION).$(MINORVERSION).$(REVISIONVERSION)
# The version date:
COMPILERDATE := $(shell git log -1 --format="%ci" | cut -c-10)
# The name of the Curry system, needed for installation of system libraries:
export CURRYSYSTEM=pakcs

# Paths used in this installation
# -------------------------------

# the root directory of the installation
export ROOT=$(CURDIR)
# binary directory and executables
export BINDIR=$(ROOT)/bin
# Directory where the libraries are located
export LIBDIR        = $(ROOT)/lib
# Directory where the libraries for meta-programming are located
export METADIR        = $(LIBDIR)/meta
# Directory where the documentation files are located
export DOCDIR        = $(ROOT)/docs
# The version information file for Curry2Prolog:
C2PVERSION=$(ROOT)/curry2prolog/pakcsversion.pl
# The version information file for the manual:
MANUALVERSION=$(DOCDIR)/src/version.tex
# Directory where local package installations are stored
export LOCALPKG      = $(ROOT)/pkg
# The path to the package database
export PKGDB         = $(LOCALPKG)/pakcs.conf.d

# Various executables used in the installation
# --------------------------------------------

# The REPL binary, used for building various tools
export REPL         = $(BINDIR)/$(CURRYSYSTEM)
# The default options for the REPL
export REPL_OPTS    = --noreadline :set -time
# The frontend binary
export CYMAKE       = $(BINDIR)/cymake
# The cleancurry binary
export CLEANCURRY   = $(BINDIR)/cleancurry

# Logfile for make:
MAKELOG=make.log

# GHC and CABAL configuration (for installing the front end)
# ----------------------------------------------------------
# The path to the Glasgow Haskell Compiler and Cabal
export GHC     := $(shell which ghc)
export GHC-PKG := $(shell dirname "$(GHC)")/ghc-pkg
export CABAL    = cabal

# Because of an API change in GHC 7.6,
# we need to distinguish GHC < 7.6 and GHC >= 7.6.
# GHC 7.6 renamed the option "package-conf" to "package-db".

# extract GHC version
GHC_MAJOR := $(shell "$(GHC)" --numeric-version | cut -d. -f1)
GHC_MINOR := $(shell "$(GHC)" --numeric-version | cut -d. -f2)
# Is the GHC version >= 7.6 ?
GHC_GEQ_76 = $(shell test $(GHC_MAJOR) -gt 7 -o \( $(GHC_MAJOR) -eq 7 \
              -a $(GHC_MINOR) -ge 6 \) ; echo $$?)
# package-db (>= 7.6) or package-conf (< 7.6)?
ifeq ($(GHC_GEQ_76),0)
GHC_PKG_OPT = package-db
else
GHC_PKG_OPT = package-conf
endif

# Command to unregister a package
export GHC_UNREGISTER = "$(GHC-PKG)" unregister --$(GHC_PKG_OPT)="$(PKGDB)"
# Command to install missing packages using cabal
export CABAL_INSTALL  = "$(CABAL)" install --with-compiler="$(GHC)"       \
                        --with-hc-pkg="$(GHC-PKG)" --prefix="$(LOCALPKG)" \
                        --global --package-db="$(PKGDB)" -O2

########################################################################
# The targets
########################################################################

#
# Install all components of PAKCS
#
.PHONY: all
all:
	@rm -f $(MAKELOG)
	@echo "Make started at `date`" > $(MAKELOG)
	$(MAKE) config  2>&1 | tee -a $(MAKELOG)
	$(MAKE) install 2>&1 | tee -a $(MAKELOG)
	@echo "Make finished at `date`" >> $(MAKELOG)
	@echo "Make process logged in file $(MAKELOG)"

#
# Install all components of PAKCS
#
.PHONY: install
install: installscripts copylibs
	@echo "PAKCS installation configuration (file pakcsinitrc):"
	@cat pakcsinitrc
	$(MAKE) frontend
	# pre-compile all libraries:
	@cd lib && $(MAKE) fcy
	# install the Curry2Prolog compiler as a saved system:
	$(MAKE) $(C2PVERSION)
	cd curry2prolog && $(MAKE)
	# compile all libraries:
	@cd lib && $(MAKE) acy
	# compile optimization tools:
	@cd currytools/optimize && $(MAKE)
	# prepare for separate compilation: compile all libraries to Prolog
	@if [ -r bin/pakcs ] ; then cd lib && $(MAKE) pl ; fi
	$(MAKE) tools
	$(MAKE) docs
	chmod -R go+rX .

# Clean old files that might be in conflict with newer versions of PAKCS:
.PHONY: cleanoldinfos
cleanoldinfos:
	# delete old RequiredValue analysis files since format has changed:
	@if [ -d $(HOME)/.curry/Analysis ] ; then \
	  find $(HOME)/.curry/Analysis -name \*.RequiredValue.p\* -exec rm -f \{\} \; ; fi

# Configure installation w.r.t. variables in pakcsinitrc:
.PHONY: config
config: installscripts
	@scripts/configure-pakcs

# install the scripts of PAKCS in the bin directory:
.PHONY: installscripts
installscripts:
	cd scripts && $(MAKE) all

# remove the scripts of PAKCS in the bin directory:
.PHONY: cleanscripts
cleanscripts:
	cd scripts && $(MAKE) clean

# install the library sources from the trunk directory:
.PHONY: copylibs
copylibs:
	@if [ -d lib-trunk ] ; then cd lib-trunk && $(MAKE) -f Makefile.$(CURRYSYSTEM).install ; fi

# create package database
$(PKGDB):
	"$(GHC-PKG)" init $@
	$(CABAL) update

# install front end (if sources are present):
.PHONY: frontend
frontend:
	@if [ -d frontend ] ; then $(MAKE) $(PKGDB) && cd frontend && $(MAKE) ; fi

# compile the tools:
.PHONY: tools
tools:
	# compile the Curry Port Name Server demon:
	@if [ -r bin/pakcs ] ; then cd cpns       && $(MAKE) ; fi
	# compile the event handler demon for dynamic web pages:
	@if [ -r bin/pakcs ] ; then cd www        && $(MAKE) ; fi
	@if [ -r bin/pakcs ] ; then cd currytools && $(MAKE) ; fi
	@if [ -r bin/pakcs ] ; then cd tools      && $(MAKE) ; fi

# compile CASS analysis environment:
.PHONY: cass
cass:
	@if [ -r bin/pakcs ] ; then cd currytools/CASS && $(MAKE) ; fi

# compile documentation, if necessary:
.PHONY: docs
docs:
	@if [ -d $(DOCDIR)/src ] ; \
	 then $(MAKE) $(MANUALVERSION) && cd $(DOCDIR)/src && $(MAKE) install ; fi

# Create file with version information for Curry2Prolog:
$(C2PVERSION): Makefile
	echo ':- module(pakcsversion,[compilerVersion/1, compilerMajorVersion/1, compilerMinorVersion/1, compilerRevisionVersion/1, buildVersion/1, buildDate/1, installDir/1]).' > $@
	echo "compilerVersion('PAKCS$(MAJORVERSION).$(MINORVERSION)')." >> $@
	echo 'compilerMajorVersion($(MAJORVERSION)).' >> $@
	echo 'compilerMinorVersion($(MINORVERSION)).' >> $@
	echo 'compilerRevisionVersion($(REVISIONVERSION)).' >> $@
	echo 'buildVersion($(BUILDVERSION)).' >> $@
	echo "buildDate('$(COMPILERDATE)')." >> $@
	echo "installDir('$(ROOT)')." >> $@

# Create file with version information for the manual:
$(MANUALVERSION): Makefile
	echo '\\newcommand{\\pakcsversion}{$(VERSION)}' > $@
	echo '\\newcommand{\\pakcsversiondate}{Version of $(COMPILERDATE)}' >> $@

#
# Create documentation for system libraries:
#
.PHONY: libdoc
libdoc:
	@if [ ! -r bin/currydoc ] ; then \
	  echo "Cannot create library documentation: currydoc not available!" ; exit 1 ; fi
	@rm -f $(MAKELOG)
	@echo "Make libdoc started at `date`" > $(MAKELOG)
	@cd lib && $(MAKE) doc 2>&1 | tee -a ../$(MAKELOG)
	@echo "Make libdoc finished at `date`" >> $(MAKELOG)
	@echo "Make libdoc process logged in file $(MAKELOG)"

# run the test suites to check the installation
.PHONY: runtest
runtest: testsuite/doTest
	cd testsuite && ./doTest --nogui
	cd currytools/optimize/binding_optimization/Examples && ./test.sh
	cd currytools/currypp  && $(MAKE) runtest
	cd currytools/runcurry/Examples && ./test.sh
	cd examples/CHR && ./test.sh

$(CLEANCURRY):
	cd scripts && $(MAKE) $@

# Clean the system files, i.e., remove the installed PAKCS components
# except for the front end
.PHONY: clean
clean: $(CLEANCURRY)
	rm -f $(MAKELOG)
	$(MAKE) cleantools
	if [ -d lib ] ; then cd lib && $(MAKE) clean ; fi
	cd examples && $(CLEANCURRY) -r
	if [ -d $(DOCDIR)/src ] ; then cd $(DOCDIR)/src && $(MAKE) clean ; fi
	cd bin && rm -f sicstusprolog swiprolog
	cd scripts && $(MAKE) clean
	-cd frontend && $(MAKE) clean

# Clean the generated PAKCS tools
.PHONY: cleantools
cleantools: $(CLEANCURRY)
	cd curry2prolog && $(MAKE) clean
	cd currytools && $(MAKE) uninstall
	cd tools && $(MAKE) clean
	cd cpns && $(MAKE) clean
	cd www && $(MAKE) clean
	cd bin && rm -f pakcs

# Clean everything (including the front end)
.PHONY: cleanall
cleanall: clean
	rm -rf $(LIBDIR)
	rm -rf $(LOCALPKG)
	-cd frontend && $(MAKE) cleanall
	if [ -d frontend ]; then rm -rf $(BINDIR); fi
	rm -f pakcsinitrc pakcsinitrc.bak


#################################################################################
# Create distribution versions of the complete system as tar files pakcs*.tar.gz:

# directory name of distribution
FULLNAME    = pakcs-$(VERSION)
# temporary directory to create distribution version
PAKCSDIST   = $(FULLNAME)
# architecture name
ARCH        = $(shell dpkg-architecture -qDEB_BUILD_ARCH)-$(shell uname -s)
# Files to be excluded for source distribution
SRC_EXCLUDE = --exclude=bin
# Files to be excluded for binary distribution
BIN_EXCLUDE = --exclude=frontend
# date of distribution
DIST_DATE   = $(shell date +%Y%m%d)

.PHONY: dist
dist:
	rm -rf pakcs*.tar.gz $(PAKCSDIST) # remove any old distribution
	git clone . $(PAKCSDIST)          # create copy of git version
	cd $(PAKCSDIST) && git submodule init && git submodule update
	cd $(PAKCSDIST) && $(MAKE) copylibs
	cd $(PAKCSDIST) && $(MAKE) cleandist # delete unnessary files
	mkdir -p $(PAKCSDIST)/bin && cp -p $(CYMAKE) $(PAKCSDIST)/bin
	cp -p docs/Manual.pdf docs/markdown_syntax.html $(PAKCSDIST)/docs
	cat Makefile | sed -e "/distribution/,\$$d" \
	             | sed 's|^COMPILERDATE *:=.*$$|COMPILERDATE =$(COMPILERDATE)|' \
	             > $(PAKCSDIST)/Makefile
	tar cfvz $(FULLNAME)-src.tar.gz     $(SRC_EXCLUDE) $(PAKCSDIST)
	tar cfvz $(FULLNAME)-$(ARCH).tar.gz $(BIN_EXCLUDE) $(PAKCSDIST)
	rm -rf $(PAKCSDIST)
	@echo "----------------------------------------------------------------"
	@echo "Distribution files pakcs*.tar.gz generated."

.PHONY: distdated
distdated: dist
	mv $(FULLNAME)-src.tar.gz     $(FULLNAME)-$(DIST_DATE)-src.tar.gz
	mv $(FULLNAME)-$(ARCH).tar.gz $(FULLNAME)-$(DIST_DATE)-$(ARCH).tar.gz

# Clean all files that should not be included in a distribution
.PHONY: cleandist
cleandist:
	rm -rf .git .gitmodules .gitignore
	rm -rf lib-trunk
	rm -rf currytools/.git currytools/.gitignore
	cd frontend/curry-base     && rm -rf .git .gitignore dist
	cd frontend/curry-frontend && rm -rf .git .gitignore dist
	rm -rf docs/src
	rm -f KNOWN_BUGS CHANGELOG.html
