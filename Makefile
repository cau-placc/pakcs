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
MINORVERSION=10
# The revision version number:
REVISIONVERSION=1
# The build version number:
BUILDVERSION=1
# The version date:
COMPILERDATE=11/07/12

# Logfile for make:
MAKELOG=make.log
# the root directory
ROOT=${CURDIR}
# binary directory and executables
BINDIR=${ROOT}/bin
# Directory where local executables are stored:
LOCALBIN=${BINDIR}/.local
# The version information file for Curry2Prolog:
C2PVERSION=curry2prolog/pakcsversion.pl
# The version information file for the manual:
MANUALVERSION=docs/src/version.tex

#
# Install all components of PAKCS
#
.PHONY: all
all: config
	@rm -f ${MAKELOG}
	@echo "Make started at `date`" > ${MAKELOG}
	${MAKE} install 2>&1 | tee -a ${MAKELOG}
	@echo "Make finished at `date`" >> ${MAKELOG}
	@echo "Make process logged in file ${MAKELOG}"

#
# Install all components of PAKCS
#
.PHONY: install
install: installscripts
	# install cabal front end if sources are present:
	@if [ -d frontend ] ; then ${MAKE} installfrontend ; fi
	# install the front-end if necessary:
	cd bin && rm -f parsecurry && ln -s .pakcs_wrapper parsecurry
	# pre-compile all libraries:
	@cd lib && ${MAKE} fcy
	# install the Curry2Prolog compiler as a saved system:
	rm -f bin/pakcs
	@if [ -r bin/sicstusprolog -o -r bin/swiprolog ] ; \
	 then ${MAKE} ${C2PVERSION} && cd curry2prolog && ${MAKE} ; fi
	# compile all libraries:
	@cd lib && ${MAKE} acy
	# prepare for separate compilation by compiling all librariers to Prolog code:
	@if [ -r bin/pakcs ] ; then cd lib && ${MAKE} pl ; fi
	# compile the Curry Port Name Server demon:
	@if [ -r bin/pakcs ] ; then cd cpns && ${MAKE} ; fi
	# compile the event handler demon for dynamic web pages:
	@if [ -r bin/pakcs ] ; then cd www && ${MAKE} ; fi
	# compile the tools:
	@if [ -r bin/pakcs ] ; then cd tools && ${MAKE} ; fi
	# compile documentation, if necessary:
	@if [ -d docs/src ] ; \
	 then ${MAKE} ${MANUALVERSION} && cd docs/src && ${MAKE} install ; fi
	chmod -R go+rX .

# Configure installation w.r.t. variables in bin/.pakcs_variables:
.PHONY: config
config:
	@./update-pakcsrc
	@./configure-pakcs

# install some scripts of PAKCS in the bin directory:
.PHONY: installscripts
installscripts:
	@if [ ! -d ${BINDIR} ] ; then mkdir -p ${BINDIR} ; fi
	sed "s|^PAKCSHOME=.*$$|PAKCSHOME=${ROOT}|" < scripts/pakcs_wrapper.sh > ${BINDIR}/.pakcs_wrapper
	sed "s|^PAKCSHOME=.*$$|PAKCSHOME=${ROOT}|" < scripts/makecurrycgi.sh > ${BINDIR}/makecurrycgi
	sed "s|^PAKCSHOME=.*$$|PAKCSHOME=${ROOT}|" < scripts/makesavedstate.sh > ${BINDIR}/.makesavedstate
	cd ${BINDIR} && chmod 755 .pakcs_wrapper makecurrycgi .makesavedstate

# install new front end:
.PHONY: installfrontend
installfrontend:
	@if [ ! -d ${LOCALBIN} ] ; then mkdir ${LOCALBIN} ; fi
	cabal update
	cabal install mtl
	cd frontend/curry-base && cabal install
	cd frontend/curry-frontend && cabal install
	# copy cabal installation of front end into local directory
	@if [ -f ${HOME}/.cabal/bin/cymake ] ; then cp -p ${HOME}/.cabal/bin/cymake ${LOCALBIN} ; fi

# Create file with version information for Curry2Prolog:
${C2PVERSION}: Makefile
	echo ':- module(pakcsversion,[compilerVersion/1, compilerMajorVersion/1, compilerMinorVersion/1, compilerRevisionVersion/1, buildVersion/1, buildDate/1]).' > $@
	echo "compilerVersion('PAKCS${MAJORVERSION}.${MINORVERSION}')." >> $@
	echo 'compilerMajorVersion(${MAJORVERSION}).' >> $@
	echo 'compilerMinorVersion(${MINORVERSION}).' >> $@
	echo 'compilerRevisionVersion(${REVISIONVERSION}).' >> $@
	echo 'buildVersion(${BUILDVERSION}).' >> $@
	echo "buildDate('${COMPILERDATE}')." >> $@

# Create file with version information for the manual:
${MANUALVERSION}: Makefile
	echo '\\newcommand{\\pakcsversion}{${MAJORVERSION}.${MINORVERSION}.${REVISIONVERSION}}' > $@
	echo '\\newcommand{\\pakcsversiondate}{Version of ${COMPILERDATE}}' >> $@

#
# Create documentation for system libraries:
#
.PHONY: libdoc
libdoc:
	@if [ ! -r bin/currydoc ] ; then \
	  echo "Cannot create library documentation: currydoc not available!" ; exit 1 ; fi
	@rm -f ${MAKELOG}
	@echo "Make libdoc started at `date`" > ${MAKELOG}
	@cd lib && ${MAKE} doc 2>&1 | tee -a ../${MAKELOG}
	@echo "Make libdoc finished at `date`" >> ${MAKELOG}
	@echo "Make libdoc process logged in file ${MAKELOG}"

# Clean the system files, i.e., remove the installed PAKCS components
.PHONY: clean
clean:
	rm -f ${MAKELOG}
	${MAKE} cleantools
	cd lib && ${MAKE} clean
	cd examples && ../bin/cleancurry -r
	if [ -d docs/src ] ; then cd docs/src && ${MAKE} clean ; fi
	cd bin && rm -f sicstusprolog swiprolog

# Clean the generated PAKCS tools
.PHONY: cleantools
cleantools:
	cd curry2prolog && ${MAKE} clean
	cd tools && ${MAKE} clean
	cd cpns && ${MAKE} clean
	cd www && ${MAKE} clean
	cd bin && rm -f pakcs
	rm -rf ${LOCALBIN}

#################################################################################
# Create distribution versions of the complete system as tar files pakcs*.tar.gz:

# temporary directory to create distribution version
PAKCSDIST=/tmp/pakcs
# repository with new front-end:
FRONTENDREPO=git://git-ps.informatik.uni-kiel.de/curry

# install the sources of the front end from its repository
.PHONY: frontendsources
frontendsources:
	if [ -d frontend ] ; then \
	 cd frontend/curry-base && git pull && cd ../curry-frontend && git pull ; \
	 else mkdir frontend && cd frontend && \
	      git clone ${FRONTENDREPO}/curry-base.git && \
	      git clone ${FRONTENDREPO}/curry-frontend.git ; fi

.PHONY: dist
dist:
	rm -rf pakcs*.tar.gz ${PAKCSDIST} # remove old distributions
	cp -r -p . ${PAKCSDIST}           # create complete copy of this version
	# install front end sources if they are not present:
	if [ ! -d frontend ] ; then \
	  cd ${PAKCSDIST} && ${MAKE} frontendsources ; fi
	cd ${PAKCSDIST} && ${MAKE} cleandist  # delete unnessary files
	sed -e "/PAKCS developers/,\$$d" < ${PAKCSDIST}/bin/.pakcs_variables.init > ${PAKCSDIST}/bin/.pakcs_variables
	rm ${PAKCSDIST}/bin/.pakcs_variables.init
	# delete Prolog files of libraries
	cd ${PAKCSDIST}/lib && ${MAKE} cleanpl
	# generate binary distributions on remote hosts:
	${MAKE} dist_mh@climens.informatik.uni-kiel.de # Linux distribution
	#${MAKE} dist_mh@mickey.informatik.uni-kiel.de # SunOS distribution
	# generate source distribution:
	cp Makefile ${PAKCSDIST}/Makefile
	cd ${PAKCSDIST}/lib && ${MAKE} clean # delete precompiled libraries
	sed -e "/distribution/,\$$d" < Makefile > ${PAKCSDIST}/Makefile
	cd /tmp && tar cf pakcs_src.tar pakcs && gzip pakcs_src.tar
	mv /tmp/pakcs_src.tar.gz .
	chmod 644 pakcs_src.tar.gz pakcs_`uname -s`.tar.gz
	rm -rf ${PAKCSDIST}
	@echo "----------------------------------------------------------------"
	@echo "Distribution files pakcs_*.tar.gz generated."

# generate distribution on a remote host:
dist_%:
	cp Makefile ${PAKCSDIST}/Makefile
	sed -e "/distribution/,\$$d" < Makefile > ${PAKCSDIST}/Makefile
	scp -p -q -r ${PAKCSDIST} $*:${PAKCSDIST}
	scp -q Makefile $*:${PAKCSDIST}/../Makefile
	ssh $* "cd ${PAKCSDIST} && ${MAKE} -f ../Makefile genbindist"
	scp -p $*:/tmp/pakcs_\*.tar.gz .
	ssh $* rm -rf ${PAKCSDIST} /tmp/pakcs_\*.tar.gz /tmp/Makefile

# compile cabal parser from the sources, replace them by binaries
# and put everything into a .tar.gz file:
.PHONY: genbindist
genbindist:
	rm -f pakcs*.tar.gz
	PATH=/opt/ghc/bin:/home/haskell/bin:${PATH} && export PATH && make installfrontend
	rm -rf frontend
	cd /tmp && tar cf pakcs_`uname -s`.tar pakcs && gzip pakcs_`uname -s`.tar


#
# Clean all files that should not be included in a distribution
#
.PHONY: cleandist
cleandist:
	rm -f ${MAKELOG}
	${MAKE} cleantools
	rm -rf .git .gitmodules lib/.git .gitignore lib/.gitignore
	rm -rf frontend/curry-base/.git frontend/curry-base/.gitignore
	rm -rf frontend/curry-frontend/.git frontend/curry-frontend/.gitignore
	rm -rf frontend/curry-base/dist frontend/curry-frontend/dist
	rm -rf docs/src
	rm -rf lib/CDOC lib/TEXDOC
	rm -f CVS-description KNOWN_BUGS VERSIONS CHANGELOG.html
	rm -rf CVS */CVS */*/CVS */*/*/CVS
	rm -f *~ */*~ */*/*~ */*/*/*~ */*/*/*/*~
	cd bin && rm -f sicstusprolog swiprolog .??*~ .??*.bak
	cd examples && ../bin/cleancurry -r
