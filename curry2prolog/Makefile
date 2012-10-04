########################################################################
# Makefile for the Curry->Prolog compiler system of PAKCS
########################################################################

# define environment variable PAKCSHOME:
PAKCSHOME=`pwd`/..

# The SICStus Prolog interpreter:
SICSTUS=`pwd`/../bin/sicstusprolog

# The SWI-Prolog interpreter:
SWI=`pwd`/../bin/swiprolog

# Standard options for SWI-Prolog
SWIOPTIONS=-q -L0 -G0 -T0

.PHONY: all
all:
	@if [ -x "${SICSTUS}" ] ; then ${MAKE} sicstus ; elif [ -x "${SWI}" ] ; then ${MAKE} swi ; else echo "No Prolog defined to generate Curry->Prolog compiler" ; exit 1 ; fi
	../bin/.makesavedstate -error c2p.state

# make everything if we have a SICStus-Prolog system:
.PHONY: sicstus
sicstus:
	echo "[sicstusbasics], generatePrologBasics." | "${SICSTUS}"
	${MAKE} c2p.state

# make everything if we have a SWI-Prolog system:
.PHONY: swi
swi:
	${MAKE} libswi
	rm -f prologbasics.pl ; cp swibasics.pl prologbasics.pl
	${MAKE} swi.state
	mv -f swi.state c2p.state

#
# generate a saved state of the Curry->Prolog Compiler System w.r.t. SICStus-Prolog
#
c2p.state: prologbasics.pl basics.pl version.pl loader.pl evaluator.pl c2p.pl \
	      compiler.pl external.pl readFlcFromFcy.pl \
	      lib_src/prim_readshowterm.pl \
	      readXml.pl ../lib/.curry/Prelude.fcy
	PAKCSHOME=${PAKCSHOME} && export PAKCSHOME && \
	  PAKCSLIBPATH=${PAKCSHOME}/lib:${PAKCSHOME}/lib/meta && \
	  export PAKCSLIBPATH && \
	  echo "compile(c2p),c2p('../lib/Prelude'),compile('../lib/.curry/pakcs/Prelude'),loadAndCompile('../lib/.curry/pakcs/Prelude.pl',[],create),saveprog_entry('c2p.state',user:pakcsMain)." \
           | "${SICSTUS}"

#
# generate a saved state of the Curry->Prolog Compiler System w.r.t. SWI-Prolog
#
swi.state: prologbasics.pl basics.pl version.pl loader.pl evaluator.pl c2p.pl \
	      compiler.pl external.pl readFlcFromFcy.pl \
	      lib_src/prim_readshowterm.pl \
	      readXml.pl ../lib/.curry/Prelude.fcy
	PAKCSHOME=${PAKCSHOME} && export PAKCSHOME && \
	  PAKCSLIBPATH=${PAKCSHOME}/lib:${PAKCSHOME}/lib/meta && \
	  export PAKCSLIBPATH && \
	  echo "compile(c2p). c2p('../lib/Prelude'), compile('${PAKCSHOME}/lib/.curry/pakcs/Prelude'),loader:loadAndCompile('${PAKCSHOME}/lib/.curry/pakcs/Prelude.pl',[],create). saveprog_entry('swi.state',user:pakcsMain)." | "${SWI}" ${SWIOPTIONS}

../lib/.curry/Prelude.fcy: ../lib/Prelude.curry
	cd ../lib ; ${MAKE} .curry/Prelude.fcy

#
# generate correct versions of run-time libraries for SWI-Prolog
#
PRIMLIBSRC = `cd lib_src ; echo *.pl`

.PHONY: libswi
libswi:
	@mkdir -p libswi
	@for i in $(PRIMLIBSRC) ; do ${MAKE} libswi/$$i ; done

libswi/%.pl: lib_src/%.pl
	echo "[block2freeze]. transFile('$*.pl')." | "${SWI}" ${SWIOPTIONS}

# clean up everything
.PHONY: clean
clean: 
	rm -f c2p.state ../lib/.curry/pakcs/Prelude.pl ../bin/pakcs prologbasics.pl
	rm -f pakcsversion.pl
	rm -rf libswi/*
