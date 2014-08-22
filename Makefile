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
MINORVERSION=11
# The revision version number:
REVISIONVERSION=4
# The build version number:
BUILDVERSION=2
# Complete version:
VERSION=$(MAJORVERSION).$(MINORVERSION).$(REVISIONVERSION)
# The version date:
COMPILERDATE := $(shell git log -1 --format="%ci" | cut -c-10)
# The name of the Curry system, needed for installation of currytools
export CURRYSYSTEM=pakcs

# Paths used in this installation
# -------------------------------

# the root directory of the installation
export ROOT=$(CURDIR)
# binary directory and executables
export BINDIR=$(ROOT)/bin
# Directory where the libraries are located
export LIBDIR        = $(ROOT)/lib
# Directory where the documentation files are located
export DOCDIR        = $(ROOT)/docs
# The version information file for Curry2Prolog:
C2PVERSION=$(ROOT)/curry2prolog/pakcsversion.pl
# The version information file for the manual:
MANUALVERSION=$(DOCDIR)/src/version.tex

# Various executables used in the installation
# --------------------------------------------

# The REPL binary, used for building various tools
export REPL         = $(BINDIR)/$(CURRYSYSTEM)
# The default options for the REPL
export REPL_OPTS    = 
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
# Command to unregister a package
export GHC_UNREGISTER = "$(GHC-PKG)" unregister
# Command to install missing packages using cabal
export CABAL_INSTALL  = "$(CABAL)" install --with-compiler="$(GHC)" -O2


########################################################################
# The targets
########################################################################

#
# Install all components of PAKCS
#
.PHONY: all
all: config
	@rm -f $(MAKELOG)
	@echo "Make started at `date`" > $(MAKELOG)
	$(MAKE) install 2>&1 | tee -a $(MAKELOG)
	@echo "Make finished at `date`" >> $(MAKELOG)
	@echo "Make process logged in file $(MAKELOG)"

#
# Install all components of PAKCS
#
.PHONY: install
install: installscripts copylibs
	$(MAKE) frontend
	# pre-compile all libraries:
	@cd lib && $(MAKE) fcy
	# install the Curry2Prolog compiler as a saved system:
	@if [ -r bin/sicstusprolog -o -r bin/swiprolog ] ; \
	 then $(MAKE) $(C2PVERSION) && cd curry2prolog && $(MAKE) ; \
	 else rm -f bin/pakcs ; fi
	# compile all libraries:
	@cd lib && $(MAKE) acy
	# prepare for separate compilation by compiling all librariers to Prolog code:
	@if [ -r bin/pakcs ] ; then cd lib && $(MAKE) pl ; fi
	$(MAKE) tools
	$(MAKE) docs
	chmod -R go+rX .

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

# install front end (if sources are present):
.PHONY: frontend
frontend:
	@if [ -d frontend ] ; then cd frontend && $(MAKE) ; fi

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

# install required cabal packages required by the front end
# (only necessary if the front end is installed for the first time)
.PHONY: installhaskell
installhaskell:
	$(CABAL) update
	$(CABAL_INSTALL) mtl

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

# run the test suite to check the installation
.PHONY: runtest
runtest: examples/doTest
	cd examples && ./doTest --nogui

# Clean the system files, i.e., remove the installed PAKCS components
# except for the front end
.PHONY: clean
clean:
	rm -f $(MAKELOG)
	$(MAKE) cleantools
	if [ -d lib ] ; then cd lib && $(MAKE) clean ; fi
	cd examples && $(CLEANCURRY) -r
	if [ -d $(DOCDIR)/src ] ; then cd $(DOCDIR)/src && $(MAKE) clean ; fi
	cd bin && rm -f sicstusprolog swiprolog
	cd scripts && $(MAKE) clean

# Clean the generated PAKCS tools
.PHONY: cleantools
cleantools:
	cd curry2prolog && $(MAKE) clean
	cd currytools && $(MAKE) clean
	cd tools && $(MAKE) clean
	cd cpns && $(MAKE) clean
	cd www && $(MAKE) clean
	cd bin && rm -f pakcs

# Clean everything (including the front end)
.PHONY: cleanall
cleanall:
	$(MAKE) clean
	rm -rf $(LIBDIR)


#################################################################################
# Create distribution versions of the complete system as tar files pakcs*.tar.gz:

# directory name of distribution
FULLNAME=pakcs-$(VERSION)
# temporary directory to create distribution version
PAKCSDIST=/tmp/$(FULLNAME)
# temporary directory to create binary distribution version
BINDISTDIR=/tmp/pakcsbin
PAKCSBINDIST=$(BINDISTDIR)/$(FULLNAME)
# architecture name
ARCH=`dpkg-architecture -qDEB_BUILD_ARCH`-`uname -s`

.PHONY: dist
dist:
	rm -rf pakcs*.tar.gz $(PAKCSDIST) # remove old distributions
	git clone . $(PAKCSDIST)                   # create copy of git version
	cd $(PAKCSDIST) && git submodule init && git submodule update
	cd $(PAKCSDIST) && $(MAKE) installscripts
	cp pakcsinitrc $(PAKCSDIST)/pakcsinitrc
	cd $(PAKCSDIST) && $(MAKE) frontend
	cd $(PAKCSDIST) && $(MAKE) copylibs
	cd $(PAKCSDIST)/lib && $(MAKE) fcy
	cd $(PAKCSDIST)/lib && $(MAKE) acy
	cd $(PAKCSDIST) && $(MAKE) cleandist  # delete unnessary files
	# copy documentation:
	@if [ -f docs/Manual.pdf ] ; \
	 then cp docs/Manual.pdf $(PAKCSDIST)/docs ; fi
	@if [ -f docs/markdown_syntax.html ] ; \
	 then cp docs/markdown_syntax.html $(PAKCSDIST)/docs ; fi
	cd docs && cp -p Manual.pdf markdown_syntax.html $(PAKCSDIST)/docs
	sed -e "/PAKCS developers/,\$$d" < $(PAKCSDIST)/scripts/pakcsinitrc.sh > $(PAKCSDIST)/pakcsinitrc
	rm $(PAKCSDIST)/scripts/pakcsinitrc.sh
	# generate binary distributions on remote hosts:
	$(MAKE) dist_$(USER)@lussac.informatik.uni-kiel.de # Linux 32bit dist
	$(MAKE) dist_$(USER)@siran.informatik.uni-kiel.de  # Linux 64bit dist
	#$(MAKE) dist_$(USER)@mickey.informatik.uni-kiel.de # SunOS distribution
	# generate source distribution:
	cp Makefile $(PAKCSDIST)/Makefile
	cd $(PAKCSDIST)/lib && $(MAKE) clean # delete precompiled libraries
	cat Makefile | sed -e "/distribution/,\$$d" \
	             | sed 's|^COMPILERDATE *:=.*$$|COMPILERDATE =$(COMPILERDATE)|' \
	             > $(PAKCSDIST)/Makefile
	cd $(PAKCSDIST) && $(MAKE) cleanscripts # remove local scripts
	cd /tmp && tar cf $(FULLNAME)-src.tar $(FULLNAME) && gzip $(FULLNAME)-src.tar
	mv /tmp/$(FULLNAME)-src.tar.gz .
	chmod 644 pakcs*.tar.gz
	rm -rf $(PAKCSDIST)
	@echo "----------------------------------------------------------------"
	@echo "Distribution files pakcs*.tar.gz generated."

# generate distribution on a remote host:
dist_%:
	cat Makefile | sed 's|^COMPILERDATE *:=.*$$|COMPILERDATE =$(COMPILERDATE)|' \
	             > Makefile.dated
	cat Makefile.dated | sed -e "/distribution/,\$$d" \
	                   > $(PAKCSDIST)/Makefile
	ssh $* rm -rf $(PAKCSBINDIST)
	ssh $* mkdir -p $(BINDISTDIR)
	scp -p -q -r $(PAKCSDIST) $*:$(PAKCSBINDIST)
	scp -q Makefile.dated $*:$(PAKCSBINDIST)/../Makefile
	rm Makefile.dated
	ssh $* "cd $(PAKCSBINDIST) && $(MAKE) -f ../Makefile genbindist"
	scp -p $*:$(BINDISTDIR)/pakcs\*.tar.gz .
	ssh $* rm -rf $(BINDISTDIR)

# compile front end from the sources, replace them by binaries
# and put everything into a .tar.gz file:
.PHONY: genbindist
genbindist:
	rm -f pakcs*.tar.gz
	PATH=/opt/ghc/bin:/home/haskell/bin:$(PATH) && export PATH && $(MAKE) frontend
	rm -rf frontend
	$(MAKE) cleanscripts # remove local scripts
	cd $(BINDISTDIR) && tar cf $(FULLNAME)-$(ARCH).tar $(FULLNAME) && gzip $(FULLNAME)-$(ARCH).tar


#
# Clean all files that should not be included in a distribution
#
.PHONY: cleandist
cleandist:
	rm -rf .git .gitmodules .gitignore
	rm -rf lib-trunk
	rm -rf currytools/.git currytools/.gitignore
	rm -f $(CYMAKE)
	cd frontend/curry-base     && rm -rf .git .gitignore dist
	cd frontend/curry-frontend && rm -rf .git .gitignore dist
	rm -rf docs/src
	rm -f KNOWN_BUGS CHANGELOG.html
