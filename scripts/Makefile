##############################################################################
# Installation of PAKCS shell scripts
##############################################################################

# the scripts to be installed
BINS = ${BINDIR}/pakcs ${BINDIR}/makecurrycgi \
       ${BINDIR}/.makesavedstate ${BINDIR}/parsecurry ${BINDIR}/cleancurry

.PHONY: all
all:
	@if [ ! -d ${BINDIR} ] ; then mkdir -p ${BINDIR} ; fi
	${MAKE} ${BINS}

# install some script of PAKCS in the bin directory:
${BINDIR}/%: %.sh
	sed "s|^PAKCSHOME=.*$$|PAKCSHOME=${ROOT}|" < $< > $@
	chmod 755 $@

.PHONY: clean
clean:
	rm -f ${BINS}
