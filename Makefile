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

# Some parameters for this installation
# --------------------------------------
# (these parameters might be passed to `make`)

# If the parameter CURRYFRONTEND is set to an executable,
# this executable will be used as the front end for PAKCS.
# Otherwise, the front end will be compiled from the sources
# in subdir "frontend" (if it exists).
export CURRYFRONTEND =

# Is this an installation for a distribution (Debian) package (yes|no)?
# In case of "yes":
# - nothing will be stored during the installation in the home directory
# - the documentation will not be built (since this takes a lot of time)
# - the paramters CURRYLIBSDIR and CURRYTOOLSDIR must be defined and
#   refer to the directories containing the Curry system libraries and tools
export DISTPKGINSTALL = no

# In order to build the system in a place different from the place of
# the final installation (e.g., when building it as a (Debian) package),
# the variable PAKCSINSTALLDIR should be set to the location where it
# will be finally installed after the build (e.g., /usr/lib/pakcs).
# It is required that during the build, this directory does not exist,
# otherwise the build fails. If this variable is set and the
# installed system will be moved to this location after the build, it will be
# used as the root directory for all generated components of the system.
export PAKCSINSTALLDIR =

########################################################################
# The name of the Curry system, needed for the installation of
# the system libraries and tools:
export CURRYSYSTEM=pakcs

# The major version number:
export MAJORVERSION=2
# The minor version number:
export MINORVERSION=1
# The revision version number:
export REVISIONVERSION=2
# The build version number (if >0, then it is a pre-release)
BUILDVERSION=6
# Complete version:
VERSION=$(MAJORVERSION).$(MINORVERSION).$(REVISIONVERSION)
# The version date:
ifeq ($(DISTPKGINSTALL),yes)
COMPILERDATE := $(shell date "+%Y-%m-%d")
else
COMPILERDATE := $(shell git log -1 --format="%ci" | cut -c-10)
endif

# Paths used in this installation
# -------------------------------

# the root directory of the installation
export ROOT=$(CURDIR)

# Directories of the sources of the standard libraries and tools
ifeq ($(DISTPKGINSTALL),yes)
export CURRYLIBSDIR  = $(error "CURRYLIBSDIR is undefined!")
export CURRYTOOLSDIR = $(error "CURRYTOOLSDIR is undefined!")
else
export CURRYLIBSDIR  = $(ROOT)/lib-trunk
export CURRYTOOLSDIR = # not used
endif

# binary directory and executables
export BINDIR=$(ROOT)/bin
# Directory where the front end is located
export FRONTENDDIR   = $(ROOT)/frontend
# Directory where the actual libraries are located
export LIBDIR        = $(ROOT)/lib
# Directory where the documentation files are located
export DOCDIR        = $(ROOT)/docs

# The file containing the version number of the base libraries:
BASEVERSIONFILE = $(LIBDIR)/VERSION

# Executable of CurryCheck:
CURRYCHECK := $(shell which curry-check)
# Executable of CurryDoc:
CURRYDOC := $(shell which curry-doc)
# Executable of the markdown translator (required for documentation generation):
MD2PDF := $(shell which md2pdf)

# The version information file for PAKCS:
PAKCSVERSION=$(ROOT)/src/pakcsversion.pl
# The version information file for the manual:
MANUALVERSION=$(DOCDIR)/src/version.tex

# Various executables used in the installation
# --------------------------------------------

# The REPL binary, used for building various tools
export REPL         = $(BINDIR)/$(CURRYSYSTEM)
# The default options for the REPL
export REPL_OPTS    = --noreadline :set -time
# The front end binary
export CYMAKE       = $(BINDIR)/$(CURRYSYSTEM)-frontend
# The cleancurry binary
export CLEANCURRY   = $(BINDIR)/cleancurry

# Logfile for make:
MAKELOG=make.log

########################################################################
# The targets
########################################################################

#
# Install all components of PAKCS
#
.PHONY: all
all:
ifeq ($(DISTPKGINSTALL),yes)
	$(MAKE) config
	$(MAKE) build
	# if we build a package, we compile all libraries at the end
	# so that their intermediate files are up to date:
	$(REPL) --nocypm $(REPL_OPTS) :load AllLibraries :eval "3*13+3" :quit
else
	@rm -f $(MAKELOG)
	@echo "Make started at `date`" > $(MAKELOG)
	$(MAKE) config 2>&1 | tee -a $(MAKELOG)
	$(MAKE) build  2>&1 | tee -a $(MAKELOG)
	@echo "Make finished at `date`" >> $(MAKELOG)
	@echo "Make process logged in file $(MAKELOG)"
endif

# Check whether the value of PAKCSINSTALLDIR, if defined, is a non-existing
# directory
.PHONY: checkinstalldir
checkinstalldir:
	@if [ -n "$(PAKCSINSTALLDIR)" -a -d "$(PAKCSINSTALLDIR)" ] ; then \
	  echo "ERROR: Variable PAKCSINSTALLDIR points to an existing directory!" && exit 1 ; \
	fi

#
# Build all components of PAKCS
#
.PHONY: build
build: checkinstalldir
	$(MAKE) kernel
	$(MAKE) tools
	$(MAKE) manual
	chmod -R go+rX .

# install the kernel system (binaries and libraries)
.PHONY: kernel
kernel: scripts copylibs copytools
	@echo "PAKCS installation configuration (file pakcsinitrc):"
	@cat pakcsinitrc
	# install front end:
	$(MAKE) frontend
	# pre-compile all libraries:
	@cd lib && $(MAKE) fcy
	# build the PAKCS compiler as a saved system:
	$(MAKE) $(PAKCSVERSION)
	cd src && $(MAKE)
	# compile all libraries:
	@cd lib && $(MAKE) acy
	# compile optimization tools:
	@cd currytools/optimize && $(MAKE)
	# prepare for separate compilation: compile all libraries to Prolog
	@if [ -r bin/pakcs ] ; then cd lib && $(MAKE) pl ; fi

# Clean old files that might be in conflict with newer versions of PAKCS:
.PHONY: cleanoldinfos
cleanoldinfos:
	# delete old RequiredValue analysis files since format has changed:
	@if [ -d $(HOME)/.curry/Analysis ] ; then \
	  find $(HOME)/.curry/Analysis -name \*.RequiredValue.p\* -exec rm -f \{\} \; ; fi

# Configure installation w.r.t. variables in pakcsinitrc:
.PHONY: config
config: scripts
	@scripts/configure-pakcs

# install the scripts of PAKCS in the bin directory:
.PHONY: scripts
scripts:
	cd scripts && $(MAKE) all

# remove the scripts of PAKCS in the bin directory:
.PHONY: cleanscripts
cleanscripts:
	cd scripts && $(MAKE) clean

# install the library sources from the trunk directory:
.PHONY: copylibs
copylibs:
	@if [ -d $(CURRYLIBSDIR) ] ; then cd $(CURRYLIBSDIR) && $(MAKE) -f Makefile_$(CURRYSYSTEM)_install ; fi

# if the directory `currytools` is not present, copy it from the sources:
# (only necessary for the installation of a (Debian) packages, otherwise
# `currytools` is a submodule of the repository)
.PHONY: copytools
copytools:
ifeq ($(DISTPKGINSTALL),yes)
	@if [ ! -f currytools/Makefile ] ; then $(MAKE) forcecopytools ; fi
endif

.PHONY: forcecopytools
forcecopytools:
	mkdir -p currytools
	# Copying currytools from $(CURRYTOOLSDIR)
	cp -pr $(CURRYTOOLSDIR)/* currytools

# install front end (from environment variable or sources):
.PHONY: frontend
frontend:
ifeq ($(shell test -x "$(CURRYFRONTEND)" ; echo $$?),0)
	rm -f $(CYMAKE)
	ln -s $(CURRYFRONTEND) $(CYMAKE)
else
	@if [ -d $(FRONTENDDIR) ] ; then $(MAKE) compilefrontend ; fi
endif

.PHONY: compilefrontend
compilefrontend:
	rm -f $(CYMAKE)
	cd $(FRONTENDDIR) && $(MAKE)
	cd $(BINDIR) && ln -s ../frontend/bin/curry-frontend $(notdir $(CYMAKE))

# compile the tools:
.PHONY: tools
tools:
	@if [ -r bin/pakcs ] ; then cd currytools && $(MAKE) ; fi

# compile analysis tool only:
.PHONY: CASS
CASS:
	@if [ -r bin/pakcs ] ; then cd currytools && $(MAKE) CASS ; fi

# compile documentation if sources are available and it is not a
# separate package distribution:
.PHONY: manual
manual:
	@if [ -d $(DOCDIR)/src -a $(DISTPKGINSTALL) = "no" ] ; then \
	   if [ -x "$(CURRYDOC)" -a -x "$(MD2PDF)" ] ; then \
	     $(MAKE) $(MANUALVERSION) && cd $(DOCDIR)/src && $(MAKE) install ; \
	   else echo "Executable 'curry-doc' or 'md2pdf' not found!" ; \
	        echo "To generate the manual, install them by:" ; \
                echo "> cypm install currydoc && cypm install markdown" ; \
           fi \
         fi

# Create file with version information for PAKCS:
$(PAKCSVERSION): Makefile $(BASEVERSIONFILE)
	echo ':- module(pakcsversion,[compilerVersion/1, compilerMajorVersion/1, compilerMinorVersion/1, compilerRevisionVersion/1, buildVersion/1, buildDate/1, buildDir/1, pkgInstallDir/1, baseVersion/1]).' > $@
	echo "compilerVersion('PAKCS$(MAJORVERSION).$(MINORVERSION)')." >> $@
	echo 'compilerMajorVersion($(MAJORVERSION)).' >> $@
	echo 'compilerMinorVersion($(MINORVERSION)).' >> $@
	echo 'compilerRevisionVersion($(REVISIONVERSION)).' >> $@
	echo 'buildVersion($(BUILDVERSION)).' >> $@
	echo "buildDate('$(COMPILERDATE)')." >> $@
	echo "buildDir('$(ROOT)')." >> $@
	echo "pkgInstallDir('$(PAKCSINSTALLDIR)')." >> $@
	echo "baseVersion('$(shell cat $(BASEVERSIONFILE))')." >> $@

# Create file with version information for the manual:
$(MANUALVERSION): Makefile
	echo '\\newcommand{\\pakcsversion}{$(VERSION)}' > $@
	echo '\\newcommand{\\pakcsversiondate}{Version of $(COMPILERDATE)}' >> $@

#
# Create documentation for system libraries:
#
.PHONY: libdoc
libdoc:
	@if [ ! -x "$(CURRYDOC)" ] ; then \
	  echo "Executable 'curry-doc' is not installed!" && echo "Install it by > cpm install currydoc" ; \
	else $(MAKE) genlibdoc ; \
	fi

.PHONY: genlibdoc
genlibdoc:
	@rm -f $(MAKELOG)
	@echo "Make libdoc started at `date`" > $(MAKELOG)
	@cd lib && $(MAKE) htmldoc 2>&1 | tee -a ../$(MAKELOG)
	@echo "Make libdoc finished at `date`" >> $(MAKELOG)
	@echo "Make libdoc process logged in file $(MAKELOG)"

########################################################################
# Testing: run test suites to check the installation
#
ifeq ($(DISTPKGINSTALL),yes)
# for a package installation, we run the tests in verbose mode:
export RUNTESTPARAMS=-v
else
export RUNTESTPARAMS=
endif

# run the test suites to check the installation
.PHONY: runtest
runtest:
	@if [ ! -x "$(CURRYCHECK)" ] ; then \
	  echo "Executable 'curry-check' is not installed!" && echo "To run the tests, install it by > cypm install currycheck" ; \
	else $(MAKE) runalltests ; fi

.PHONY: runalltests
runalltests: testsuite/test.sh
	cd testsuite && ./test.sh $(RUNTESTPARAMS)
	cd lib && ./test.sh $(RUNTESTPARAMS)
	cd currytools && $(MAKE) runtest
	# remove .curry (might contain analysis results if home is missing)
	rm -rf .curry

# run the test suites in verbose mode so that all output is shown:
.PHONY: runtestverbose
runtestverbose:
	$(MAKE) runtest RUNTESTPARAMS=-v

########################################################################
# Cleaning:
#

# Build the cleancurry script:
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
	if [ -d $(FRONTENDDIR) ] ; then cd $(FRONTENDDIR) && $(MAKE) clean ; fi

# Clean the generated PAKCS tools
.PHONY: cleantools
cleantools: $(CLEANCURRY)
	cd src && $(MAKE) clean
	cd currytools && $(MAKE) uninstall
	cd bin && rm -f pakcs

# Clean everything (including the front end)
.PHONY: cleanall
cleanall: clean
	rm -rf $(LIBDIR)
	if [ -d $(FRONTENDDIR) ]; then cd $(FRONTENDDIR) && $(MAKE) cleanall; fi
	if [ -d $(FRONTENDDIR) ]; \
	  then rm -rf $(BINDIR) ; \
	  else rm -f $(CYMAKE) ; \
	fi
	rm -f pakcsinitrc pakcsinitrc.bak


################################DISTRIBUTION##################################
# Create distribution versions of the complete system as tar files
# pakcs*.tar.gz:

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
	@if [ -f docs/Manual.pdf ] ; then cp -p docs/Manual.pdf docs/markdown_syntax.html $(PAKCSDIST)/docs ; fi
	cat Makefile | sed -e "/#DISTRIBUTION#/,\$$d" \
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
	rm -rf $(CURRYLIBSDIR)
	rm -rf currytools/.git currytools/.gitignore
	rm -f currytools/download_tools.sh
	cd $(FRONTENDDIR)/curry-base     && rm -rf .git .gitignore dist
	cd $(FRONTENDDIR)/curry-frontend && rm -rf .git .gitignore dist
	rm -rf docs/src
	rm -rf debian
	rm -f KNOWN_BUGS CHANGELOG.html
