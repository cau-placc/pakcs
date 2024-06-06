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
# (contact: pakcs@curry-lang.org)
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

# Set to true during CI build
export CI_BUILD = no

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
export MAJORVERSION=3
# The minor version number:
export MINORVERSION=8
# The revision version number:
export REVISIONVERSION=0
# The build version number (if >0, then it is a pre-release)
BUILDVERSION=1
# Complete version:
export VERSION=$(MAJORVERSION).$(MINORVERSION).$(REVISIONVERSION)
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
CURRYCHECK := $(shell sh -c 'command -v curry-check')
# Executable of CurryDoc:
CURRYDOC := $(shell sh -c 'command -v curry-doc')

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
export FRONTEND     = $(BINDIR)/$(CURRYSYSTEM)-frontend
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
else ifeq ($(CI_BUILD),yes)
	# don't need the log file and the pipe swallows non 0 exit codes
	$(MAKE) config
	$(MAKE) build
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
ifneq ($(CI_BUILD),yes)
	$(MAKE) manual
endif
	chmod -R go+rX .

# install the kernel system (binaries and libraries)
.PHONY: kernel
kernel: scripts copylibs copytools
	# install front end:
	$(MAKE) frontend
	# pre-compile all libraries:
	$(MAKE) -C lib fcy
	# build the PAKCS compiler as a saved system:
	$(MAKE) $(PAKCSVERSION)
	$(MAKE) -C src
	# compile optimization tools:
	@$(MAKE) -C currytools/optimize
	# compile all libraries:
	$(MAKE) -C lib AllLibraries.curry
	scripts/compile-all-libs.sh
	# prepare for separate compilation: compile all libraries to Prolog
	@if [ -r bin/pakcs ] ; then $(MAKE) -C lib pl ; fi

# build the PAKCS compiler and REPL as an executable:
.PHONY: repl
repl: $(PAKCSVERSION)
	$(MAKE) -C src

# Clean old files that might be in conflict with newer versions of PAKCS:
.PHONY: cleanoldinfos
cleanoldinfos:
	# delete old RequiredValue analysis files since format has changed:
	@if [ -d $(HOME)/.curry/Analysis ] ; then \
	  find $(HOME)/.curry/Analysis -name \*.RequiredValue.p\* -exec rm -f \{\} \; ; fi

# Configure installation, i.e., set the Prolog back end and install scripts:
.PHONY: config
config: scripts
	@scripts/configure-prolog.sh

# install the scripts of PAKCS in the bin directory:
.PHONY: scripts
scripts:
	$(MAKE) -C scripts all

# remove the scripts of PAKCS in the bin directory:
.PHONY: cleanscripts
cleanscripts:
	$(MAKE) -C scripts clean

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
	rm -f $(FRONTEND)
	ln -s $(CURRYFRONTEND) $(FRONTEND)
else
	@if [ -d $(FRONTENDDIR) ] ; then $(MAKE) compilefrontend ; fi
endif

.PHONY: compilefrontend
compilefrontend:
	rm -f $(FRONTEND)
	$(MAKE) -C $(FRONTENDDIR)
	cd $(BINDIR) && ln -s ../frontend/bin/curry-frontend $(notdir $(FRONTEND))

# compile the tools:
.PHONY: tools
tools:
	@if [ -r bin/pakcs ] ; then $(MAKE) -C currytools ; fi

# compile documentation if sources are available and it is not a
# separate package distribution:
.PHONY: manual
manual:
ifeq ($(CI_BUILD),yes)	
	@if [ -d $(DOCDIR)/src -a $(DISTPKGINSTALL) = "no" ] ; then \
		if [ -x "$(CURRYDOC)" ] ; then \
			$(MAKE) $(MANUALVERSION) && $(MAKE) -C $(DOCDIR)/src install ; \
		else \
			echo "Executable 'curry-doc' not found!" ; \
	        echo "To generate the manual, install them by:" ; \
			echo "> cypm install currydoc" ; \
			exit 1 ; \
		fi \
	fi
else	
	@if [ -d $(DOCDIR)/src -a $(DISTPKGINSTALL) = "no" ] ; then \
	   if [ -x "$(CURRYDOC)" ] ; then \
	     $(MAKE) $(MANUALVERSION) && $(MAKE) -C $(DOCDIR)/src install ; \
	   else \
			echo "Executable 'curry-doc' not found!" ; \
			echo "To generate the manual, install them by:" ; \
			echo "> cypm install currydoc" ; \
		fi \
	fi
endif

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

########################################################################
#
# Installing base libraries
#

# directory containing the sources of the Curry system libraries:
CURRYLIBSSRCDIR = $(CURRYLIBSDIR)/src

# copy the library sources from the lib-trunk directory:
.PHONY: copylibs
copylibs:
ifneq ("$(wildcard $(CURRYLIBSSRCDIR))", "")
	$(MAKE) $(LIB_MODULE_FOLDERS)
	$(MAKE) $(LIB_CURRYONLY_FILES)
	$(MAKE) $(LIB_PAKCS_CURRY_FILES)
	$(MAKE) $(LIB_PAKCS_PL_FILES)
	$(MAKE) $(LIBDIR)/Makefile
	@$(MAKE) require-jq
	$(MAKE) $(LIBDIR)/VERSION

MODULE_FOLDERS       =$(shell cd $(CURRYLIBSSRCDIR) && find * -type d)
CURRY_FILES          =$(shell cd $(CURRYLIBSSRCDIR) && find * -name "*.curry")
PAKCS_FILES          =$(shell cd $(CURRYLIBSSRCDIR) && find * -name "*.pakcs")
PAKCS_CURRY_FILES    =$(addsuffix .curry, $(basename $(PAKCS_FILES)))
PAKCS_PL_FILES       =$(shell cd $(CURRYLIBSSRCDIR) && find * -name "*.pakcs.pl")
NON_PAKCS_BASENAMES  =$(basename $(filter-out $(CURRY_PAKCS_FILES), $(CURRY_FILES)))
CURRYONLY_FILES      =$(addsuffix .curry, $(filter-out $(basename $(PAKCS_FILES)), $(NON_PAKCS_BASENAMES)))

LIB_MODULE_FOLDERS   =$(addprefix lib/, $(MODULE_FOLDERS))
LIB_CURRYONLY_FILES  =$(addprefix lib/, $(CURRYONLY_FILES))
LIB_PAKCS_FILES      =$(addprefix lib/, $(PAKCS_FILES))
LIB_PAKCS_CURRY_FILES=$(addprefix lib/, $(PAKCS_CURRY_FILES))
LIB_PAKCS_PL_FILES   =$(addprefix lib/, $(PAKCS_PL_FILES))

$(LIB_MODULE_FOLDERS): lib/%: $(CURRYLIBSSRCDIR)/%
	mkdir -p $@

$(LIB_CURRYONLY_FILES): lib/%.curry: $(CURRYLIBSSRCDIR)/%.curry
	cp $< $@

$(LIB_PAKCS_FILES): lib/%.pakcs: $(CURRYLIBSSRCDIR)/%.pakcs
	cp $< $@

$(LIB_PAKCS_CURRY_FILES): lib/%.curry: $(CURRYLIBSSRCDIR)/%.curry lib/%.pakcs
	cp $< $@

$(LIB_PAKCS_PL_FILES): lib/%.pakcs.pl: $(CURRYLIBSSRCDIR)/%.pakcs.pl
	cp $< $@

$(LIBDIR)/VERSION: $(CURRYLIBSDIR)/package.json
	$(JQ) -r '.version' $(CURRYLIBSDIR)/package.json > $@

endif

$(LIBDIR)/Makefile: lib_Makefile
	mkdir -p $(LIBDIR)
	cp $< $@

# compile all libraries:
.PHONY: compile-all-libs
compile-all-libs:
	scripts/compile-all-libs.sh

########################################################################
#
# Create documentation for system libraries:
#
.PHONY: libdoc
libdoc:
ifeq ($(CI_BUILD),yes)
	@if [ ! -x "$(CURRYDOC)" ] ; then \
		echo "Executable 'curry-doc' is not installed!" ; \
		echo "Install it by > cypm install currydoc" ; \
		exit 1 ; \
	else \
		$(MAKE) genlibdoc ; \
	fi
else
	@if [ ! -x "$(CURRYDOC)" ] ; then \
		echo "Executable 'curry-doc' is not installed!" ; \
		echo "Install it by > cypm install currydoc" ; \
	else \
		$(MAKE) genlibdoc ; \
	fi
endif

.PHONY: genlibdoc
genlibdoc:
ifeq ($(CI_BUILD),yes) 
	$(MAKE) -C lib htmldoc
else
	@rm -f $(MAKELOG)
	@echo "Make libdoc started at `date`" > $(MAKELOG)
	$(MAKE) -C lib htmldoc 2>&1 | tee -a ../$(MAKELOG)
	@echo "Make libdoc finished at `date`" >> $(MAKELOG)
	@echo "Make libdoc process logged in file $(MAKELOG)"
endif

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
ifeq ($(CI_BUILD),yes)
	@if [ ! -x "$(CURRYCHECK)" ] ; then \
		echo "Executable 'curry-check' is not installed!" ; \
		echo "To run the tests, install it by > cypm install currycheck" ; \
		exit 1 ; \
	else \
		$(MAKE) runalltests ; \
	fi
else
	@if [ ! -x "$(CURRYCHECK)" ] ; then \
		echo "Executable 'curry-check' is not installed!" ; \
		echo "To run the tests, install it by > cypm install currycheck" ; \
	else \
		$(MAKE) runalltests ; \
	fi
endif

.PHONY: runalltests
runalltests: testsuite/test.sh
	cd testsuite && ./test.sh $(RUNTESTPARAMS)
	$(MAKE) -C currytools runtest
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
	$(MAKE) -C scripts $@

# clean components only used for installation, i.e.,
# `stack` related stuff in the front end
.PHONY: cleaninstall
cleaninstall:
	if [ -d $(FRONTENDDIR) ] ; then $(MAKE) -C $(FRONTENDDIR) cleaninstall ; fi

# Clean the system files, i.e., remove the installed PAKCS components
# except for the front end and the reference to the Prolog back end.
.PHONY: cleansystem
cleansystem: $(CLEANCURRY)
	rm -f $(MAKELOG)
	$(MAKE) cleantools
	if [ -d lib ] ; then $(MAKE) -C lib clean ; fi
	cd examples && $(CLEANCURRY)
	if [ -d $(DOCDIR)/src ] ; then $(MAKE) -C $(DOCDIR)/src clean ; fi
	cd scripts && $(MAKE) clean
	if [ -d $(FRONTENDDIR) ] ; then $(MAKE) -C $(FRONTENDDIR) clean ; fi

# Clean the generated PAKCS tools
.PHONY: cleantools
cleantools: $(CLEANCURRY)
	$(MAKE) -C src clean
	$(MAKE) -C currytools uninstall
	cd bin && rm -f pakcs

# Clean front end
.PHONY: cleanfrontend
cleanfrontend:
	if [ -d $(FRONTENDDIR) ]; then $(MAKE) -C $(FRONTENDDIR) cleanall; fi
	if [ -d $(FRONTENDDIR) ]; \
	  then rm -rf $(BINDIR) ; \
	  else rm -f $(FRONTEND) ; \
	fi

# Clean everything (including the front end, reference to Prolog back end)
.PHONY: clean
clean:
	rm -f $(BINDIR)/sicstusprolog $(BINDIR)/swiprolog
	rm -rf $(LIBDIR)
	$(MAKE) cleanfrontend
	$(MAKE) cleansystem

.PHONY: cleanall
cleanall: clean


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
	cd $(PAKCSDIST) && git submodule update --init
	cd $(PAKCSDIST) && $(MAKE) copylibs
	mkdir -p $(PAKCSDIST)/bin && cp -p $(FRONTEND) $(PAKCSDIST)/bin
	@if [ -f docs/Manual.pdf ] ; then cp -p docs/Manual.pdf $(PAKCSDIST)/docs ; fi
	cat Makefile | sed -e "/#DISTRIBUTION#/,\$$d" \
	             | sed 's|^COMPILERDATE *:=.*$$|COMPILERDATE =$(COMPILERDATE)|' \
	             > $(PAKCSDIST)/Makefile
	tar cfvz $(FULLNAME)-src.tar.gz     --exclude-vcs --exclude-from=./.tarignore $(SRC_EXCLUDE) $(PAKCSDIST)
	tar cfvz $(FULLNAME)-$(ARCH).tar.gz --exclude-vcs --exclude-from=./.tarignore $(BIN_EXCLUDE) $(PAKCSDIST)
	rm -rf $(PAKCSDIST)
	@echo "----------------------------------------------------------------"
	@echo "Distribution files pakcs*.tar.gz generated."

.PHONY: distdated
distdated: dist
	mv $(FULLNAME)-src.tar.gz     $(FULLNAME)-$(DIST_DATE)-src.tar.gz
	mv $(FULLNAME)-$(ARCH).tar.gz $(FULLNAME)-$(DIST_DATE)-$(ARCH).tar.gz


##############################################################################
# Required tools:

# Executable of JSON command-line processor:
JQ := $(shell sh -c 'command -v jq')

.PHONY: require-jq
require-jq:
ifeq ("$(JQ)","")
	@echo "Executable 'jq' not found!"
	@echo "Install it, e.g.,  by 'sudo apt install jq'"
	@exit 1
endif

##############################################################################
