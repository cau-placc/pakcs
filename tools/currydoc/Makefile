# Makefile for generating CurryDoc tool

# Required:
# - installed curry2prolog system

LIB=../../lib
META=${LIB}/meta
ANADIR=../analysis

# generate saved state for currydoc program:
CurryDoc.state: CurryDoc.curry CurryDocRead.curry CurryDocHtml.curry \
	        CurryDocTeX.curry CurryDocParams.curry \
	        ${LIB}/Markdown.curry \
	        ${META}/FlexRigid.curry \
	        ${LIB}/CategorizedHtmlList.curry \
	        ${LIB}/Distribution.curry ${LIB}/PropertyFile.curry \
	        ${META}/FlatCurry.curry ${LIB}/HTML.curry \
	        ${ANADIR}/AnaOverlapping.curry ${ANADIR}/AnaIndeterminism.curry \
	        ${ANADIR}/AnaCompleteness.curry ${ANADIR}/AnaOpComplete.curry \
	        ${ANADIR}/AnaDependency.curry 
	(echo ":set path ${ANADIR}" ; echo ":l CurryDoc" ; echo ":set path" ; \
	 echo ":save main" ) | ../../bin/pakcs
	(cd ../../bin ; rm -f currydoc ; \
	                ln -s ../tools/currydoc/CurryDoc.state currydoc)


.PHONY: clean
clean: 
	../../bin/cleancurry
	rm -f ../../bin/currydoc
