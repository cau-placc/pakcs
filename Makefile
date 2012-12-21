#****************************************************************************
# PAKCS: The Portland Aachen Kiel Curry System
# ============================================
#
# An Prolog-based implementation of the functional logic language Curry
# developed by
#
# Sergio Antoy, Bernd Brassel, Martin Engelke, Michael Hanus, Klaus Hoeppner,
# Johannes Koj, Philipp Niederau, Ramin Sadre, Frank Steiner
#
# (contact: mh@informatik.uni-kiel.de)
#****************************************************************************

# The major version numbers:
MAJORVERSION=1
# The minor version number:
MINORVERSION=11
# The revision version number:
REVISIONVERSION=1
# The build version number:
BUILDVERSION=1
# Complete version:
VERSION=$(MAJORVERSION).$(MINORVERSION).$(REVISIONVERSION)
# The version date:
COMPILERDATE=21/12/12

# Logfile for make:
MAKELOG=make.log
# The name of the Curry System
export CURRYSYSTEM=pakcs
# the root directory
export ROOT=$(CURDIR)
# binary directory and executables
export BINDIR=$(ROOT)/bin
# Directory where local executables are stored:
export LOCALBIN=$(BINDIR)/.local
# The version information file for Curry2Prolog:
C2PVERSION=$(ROOT)/curry2prolog/pakcsversion.pl
# The version information file for the manual:
MANUALVERSION=$(ROOT)/docs/src/version.tex

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
install: installscripts
	# install cabal front end if sources are present:
	@if [ -d frontend ] ; then $(MAKE) frontend ; fi
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
	# compile the Curry Port Name Server demon:
	@if [ -r bin/pakcs ] ; then cd cpns && $(MAKE) ; fi
	# compile the event handler demon for dynamic web pages:
	@if [ -r bin/pakcs ] ; then cd www && $(MAKE) ; fi
	# compile the tools:
	@if [ -r bin/pakcs ] ; then cd currytools && $(MAKE) ; fi
	@if [ -r bin/pakcs ] ; then cd tools && $(MAKE) ; fi
	# compile documentation, if necessary:
	@if [ -d docs/src ] ; \
	 then $(MAKE) $(MANUALVERSION) && cd docs/src && $(MAKE) install ; fi
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

# install front end:
.PHONY: frontend
frontend:
	cd frontend && $(MAKE)

# install required cabal packages required by the front end
# (only necessary if the front end is installed for the first time)
.PHONY: installhaskell
installhaskell:
	cabal update
	cabal install mtl

# Create file with version information for Curry2Prolog:
$(C2PVERSION): Makefile
	echo ':- module(pakcsversion,[compilerVersion/1, compilerMajorVersion/1, compilerMinorVersion/1, compilerRevisionVersion/1, buildVersion/1, buildDate/1]).' > $@
	echo "compilerVersion('PAKCS$(MAJORVERSION).$(MINORVERSION)')." >> $@
	echo 'compilerMajorVersion($(MAJORVERSION)).' >> $@
	echo 'compilerMinorVersion($(MINORVERSION)).' >> $@
	echo 'compilerRevisionVersion($(REVISIONVERSION)).' >> $@
	echo 'buildVersion($(BUILDVERSION)).' >> $@
	echo "buildDate('$(COMPILERDATE)')." >> $@

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

# Clean the system files, i.e., remove the installed PAKCS components
.PHONY: clean
clean:
	rm -f $(MAKELOG)
	$(MAKE) cleantools
	cd lib && $(MAKE) clean
	cd examples && ../bin/cleancurry -r
	if [ -d docs/src ] ; then cd docs/src && $(MAKE) clean ; fi
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
	rm -rf $(LOCALBIN)

#################################################################################
# Create distribution versions of the complete system as tar files pakcs*.tar.gz:

# directory name of distribution
FULLNAME=pakcs-$(VERSION)
# temporary directory to create distribution version
PAKCSDIST=/tmp/$(FULLNAME)
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
	$(MAKE) dist_mh@climens.informatik.uni-kiel.de # Linux 32bit dist
	$(MAKE) dist_mh@mouton.informatik.uni-kiel.de  # Linux 64bit dist
	#$(MAKE) dist_mh@mickey.informatik.uni-kiel.de # SunOS distribution
	# generate source distribution:
	cp Makefile $(PAKCSDIST)/Makefile
	cd $(PAKCSDIST)/lib && $(MAKE) clean # delete precompiled libraries
	sed -e "/distribution/,\$$d" < Makefile > $(PAKCSDIST)/Makefile
	cd $(PAKCSDIST) && $(MAKE) cleanscripts # remove local scripts
	cd /tmp && tar cf $(FULLNAME)-src.tar $(FULLNAME) && gzip $(FULLNAME)-src.tar
	mv /tmp/$(FULLNAME)-src.tar.gz .
	chmod 644 pakcs*.tar.gz
	rm -rf $(PAKCSDIST)
	@echo "----------------------------------------------------------------"
	@echo "Distribution files pakcs*.tar.gz generated."

# generate distribution on a remote host:
dist_%:
	cp Makefile $(PAKCSDIST)/Makefile
	sed -e "/distribution/,\$$d" < Makefile > $(PAKCSDIST)/Makefile
	scp -p -q -r $(PAKCSDIST) $*:$(PAKCSDIST)
	scp -q Makefile $*:$(PAKCSDIST)/../Makefile
	ssh $* "cd $(PAKCSDIST) && $(MAKE) -f ../Makefile genbindist"
	scp -p $*:/tmp/pakcs\*.tar.gz .
	ssh $* rm -rf $(PAKCSDIST) /tmp/pakcs\*.tar.gz /tmp/Makefile

# compile cabal parser from the sources, replace them by binaries
# and put everything into a .tar.gz file:
.PHONY: genbindist
genbindist:
	rm -f pakcs*.tar.gz
	PATH=/opt/ghc/bin:/home/haskell/bin:$(PATH) && export PATH && make frontend
	rm -rf frontend
	$(MAKE) cleanscripts # remove local scripts
	cd /tmp && tar cf $(FULLNAME)-$(ARCH).tar $(FULLNAME) && gzip $(FULLNAME)-$(ARCH).tar


#
# Clean all files that should not be included in a distribution
#
.PHONY: cleandist
cleandist:
	rm -rf $(LOCALBIN)
	rm -rf .git .gitmodules .gitignore
	rm -rf lib/.git lib/.gitignore
	rm -rf currytools/.git currytools/.gitignore
	cd frontend/curry-base     && rm -rf .git .gitignore dist
	cd frontend/curry-frontend && rm -rf .git .gitignore dist
	rm -rf docs/src
	rm -f KNOWN_BUGS CHANGELOG.html
