# Makefile for ERD->Curry compiler:

LIB=../../lib
PLDIR=.curry/pakcs

.PHONY: all
all: Main ${PLDIR}/ERDGeneric.pl

Main: Main.curry \
	  CodeGeneration.curry \
	  Transformation.curry \
	  XML2ERD.curry \
	  ERDGoodies.curry ERD2Graph.curry \
	  ERD.curry \
	  ${LIB}/meta/AbstractCurryPrinter.curry \
	  ${LIB}/Sort.curry \
	  ${LIB}/meta/AbstractCurry.curry \
	  ${LIB}/FiniteMap.curry \
	  ${LIB}/Dynamic.curry \
	  ${LIB}/Read.curry \
	  ${LIB}/XML.curry \
	  ${LIB}/Distribution.curry \
	  ${LIB}/ReadShowTerm.curry \
	  ${LIB}/Maybe.curry \
	  ${LIB}/AllSolutions.curry \
	  ${LIB}/Time.curry \
	  ${LIB}/FileGoodies.curry \
	  ${LIB}/List.curry \
	  ${LIB}/PropertyFile.curry \
	  ${LIB}/Char.curry \
	  ${LIB}/Directory.curry \
	  ${LIB}/IOExts.curry \
	  ${LIB}/IO.curry \
	  ${LIB}/System.curry
	# create saved state for top-level function "main":
	../../bin/pakcs -s Main
	(cd ../../bin ; rm -f erd2curry \
	              ; ln -s ../tools/erd2curry/Main erd2curry)

${PLDIR}/ERDGeneric.pl: ERDGeneric.curry \
	  ${LIB}/Database.curry \
	  ${LIB}/List.curry \
	  ${LIB}/ReadShowTerm.curry \
	  ${LIB}/Dynamic.curry \
	  ${LIB}/Global.curry \
	  ${LIB}/AllSolutions.curry \
	  ${LIB}/Maybe.curry \
	  ${LIB}/Char.curry
	# compile ERDGeneric which is used in application programs:
	../../bin/pakcs -c ERDGeneric

.PHONY: clean
clean:
	../../bin/cleancurry
	rm -f Main ../../bin/erd2curry
