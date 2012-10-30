# generate various tools for handling Curry programs:

# Required:
# - installed curry2prolog system

.PHONY: all
all:    AddTypes.state check.state curry2xml.state \
	  currytest CreateMakefile.state SourceProgGUI.state
	@cd browser   ; ${MAKE}
	@cd optimize  ; ${MAKE}
	@cd xmldata   ; ${MAKE}
	@cd Peval     ; ${MAKE}
	@cd coosy     ; ${MAKE}
	@cd curry2js  ; ${MAKE}
	@cd erd2curry ; ${MAKE}
	@cd ui        ; ${MAKE}


# generate saved state for addtypes program:
AddTypes.state: AddTypes.curry
	../bin/pakcs -s AddTypes
	(cd ../bin ; rm -f addtypes ; \
	             ln -s ../tools/AddTypes.state addtypes)

# generate saved state for check program:
check.state: check.curry ../lib/meta/FlatCurry.curry \
	     ../currytools/analysis/AnaOverlapping.curry \
	     ../currytools/analysis/AnaIndeterminism.curry
	../bin/pakcs -set path ../currytools/analysis -set -error -s check

# generate saved state for currycreatemake program:
CreateMakefile.state: CreateMakefile.curry ../lib/meta/FlatCurryRead.curry
	../bin/pakcs -s CreateMakefile
	(cd ../bin ; rm -f currycreatemake ; \
	             ln -s ../tools/CreateMakefile.state currycreatemake)

# generate saved state for currytest program:
.PHONY: currytest
currytest: currytest.state

currytest.state: currytest.curry ../lib/meta/FlatCurry.curry \
	         ../lib/System.curry ../lib/Assertion.curry \
	         ../lib/Dynamic.curry ../lib/Distribution.curry ../lib/GUI.curry
	../bin/pakcs -s currytest
	(cd ../bin ; rm -f currytest ; \
	             ln -s ../tools/currytest.state currytest)

# generate saved state for curry2xml program:
curry2xml.state: curry2xml.curry ../lib/meta/FlatXML.curry \
	         ../lib/meta/Flat.curry \
	         ../lib/meta/Flat2Fcy.curry ../lib/XML.curry
	../bin/pakcs -set -error -s curry2xml

# generate saved state for interface generator:
ImportGraph.state: ImportGraph.curry ../lib/meta/FlatCurry.curry \
	      ../lib/System.curry ../lib/DaVinci.curry ../lib/List.curry
	../bin/pakcs -s ImportGraph

SourceProgGUI.state: SourceProgGUI.curry ../lib/GUI.curry
	../bin/pakcs -s SourceProgGUI

.PHONY: clean
clean: 
	../bin/cleancurry
	(cd ../bin ; rm -f addtypes currycreatemake currytest)
	cd Peval     ; ${MAKE} clean
	cd coosy     ; ${MAKE} clean
	cd browser   ; ${MAKE} clean
	cd optimize  ; ${MAKE} clean
	cd xmldata   ; ${MAKE} clean
	cd curry2js  ; ${MAKE} clean
	cd erd2curry ; ${MAKE} clean
	cd ui        ; ${MAKE} clean
