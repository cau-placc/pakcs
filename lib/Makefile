# precompile all modules of the library
# to avoid recompilation when they are used:

PARSECURRY=`pwd`/../bin/parsecurry
PAKCS=`pwd`/../bin/pakcs

# directory for HTML documentation files:
DOCDIR=CDOC
# directory for LaTeX documentation files:
TEXDOCDIR=TEXDOC
# the currydoc program:
CURRYDOC=`pwd`/../bin/currydoc

LIB_CURRY = Prelude.curry \
	    AllSolutions.curry Array.curry Assertion.curry CategorizedHtmlList.curry \
            Char.curry CLPFD.curry CLPR.curry CLPB.curry Combinatorial.curry \
	    Constraint.curry CPNS.curry CSV.curry  \
            Database.curry DaVinci.curry Dequeue.curry Directory.curry \
	    Distribution.curry Dynamic.curry \
            FileGoodies.curry FiniteMap.curry Float.curry \
	    Global.curry GlobalVariable.curry GraphInductive.curry GUI.curry \
	    HTML.curry HtmlCgi.curry HtmlParser.curry \
	    IO.curry IOExts.curry Integer.curry \
	    JavaScript.curry \
            KeyDatabase.curry KeyDatabaseSQLite.curry KeyDB.curry \
	    List.curry Mail.curry Maybe.curry Markdown.curry \
	    NamedSocket.curry \
	    Parser.curry PlProfileData.curry Ports.curry Pretty.curry \
	    Profile.curry PropertyFile.curry \
            Random.curry Read.curry ReadNumeric.curry ReadShowTerm.curry \
            RedBlackTree.curry SetRBT.curry SetRBT0.curry \
	    SetFunctions.curry \
	    Socket.curry Sort.curry System.curry \
            TableRBT.curry TableRBT0.curry \
            Time.curry Tk.curry Traversal.curry \
            Unsafe.curry URL.curry WUI.curry WUIjs.curry \
	    XML.curry XmlConv.curry \
            meta/AbsCurry.curry meta/AbsCurryIO.curry \
	    meta/AbstractCurry.curry meta/AbstractCurryPrinter.curry \
	    meta/CurryStringClassifier.curry \
	    meta/Flat.curry meta/FlatTools.curry meta/Flat2Fcy.curry \
            meta/FlatCurry.curry \
	    meta/FlatCurryRead.curry meta/FlatCurryShow.curry \
	    meta/FlatCurryGoodies.curry meta/FlatCurryTools.curry \
	    meta/FlatXML.curry meta/FlatCurryXML.curry \
	    meta/FlexRigid.curry meta/CompactFlatCurry.curry \
	    meta/PrettyAbstract.curry 

LIB_FCY   = `echo $(LIB_CURRY:%.curry=.curry/%.fcy) | sed 's|\.curry/meta/|meta/.curry/|g'`
LIB_ACY   = `echo $(LIB_CURRY:%.curry=.curry/%.acy) | sed 's|\.curry/meta/|meta/.curry/|g'`
LIB_PL    = `echo $(LIB_CURRY:%.curry=.curry/pakcs/%.pl) | sed 's|\.curry/pakcs/meta/|meta/.curry/pakcs/|g'`
LIB_HTML  = $(LIB_CURRY:.curry=.html)
LIB_TEX   = $(LIB_CURRY:.curry=.tex)
LIB_NAMES = `echo $(LIB_CURRY) | sed 's|meta/||g'` # lib names without meta/ prefix

.PHONY: all
all: fcy acy

.PHONY: fcy
fcy:
	${MAKE} $(LIB_FCY)

.PHONY: acy
acy:
	${MAKE} $(LIB_ACY)

.PHONY: pl
pl:
	@${MAKE} $(LIB_PL)

# generate all FlatCurry files in subdirectory .curry:
.curry/%.fcy: %.curry
	"${PARSECURRY}" --flat --quiet $*

meta/.curry/%.fcy: meta/%.curry
	"${PARSECURRY}" --flat --quiet $*

# generate all AbstractCurry files in subdirectory .curry:
.curry/%.acy: %.curry
	"${PARSECURRY}" --acy --quiet $*

meta/.curry/%.acy: meta/%.curry
	"${PARSECURRY}" --acy --quiet $*

# generate all Prolog translations:
.curry/pakcs/%.pl: .curry/%.fcy
	rm -f $@ && "${PAKCS}" --quiet -c $*

meta/.curry/pakcs/%.pl: meta/.curry/%.fcy
	rm -f $@ && "${PAKCS}" --quiet -c $*

# create HTML documentation files for system libraries
.PHONY: doc
doc: $(LIB_CURRY)
	@mkdir -p "${DOCDIR}"
	@cd "${DOCDIR}" && rm -f meta DOINDEX && ln -s . meta
	@cd "${DOCDIR}" && ${MAKE} -f ../Makefile $(LIB_HTML)
	@if [ -f "${DOCDIR}/DOINDEX" ] ; then ${MAKE} htmlindex ; fi
	@cd "${DOCDIR}" && rm -f meta DOINDEX

.PHONY: htmlindex
htmlindex:
	@echo "Generating index pages for Curry libraries:"
	@echo $(LIB_NAMES)
	@"${CURRYDOC}" --onlyindexhtml "${DOCDIR}" $(LIB_NAMES)

# generate individual documentations for libraries:
%.html: ../%.curry
	@touch DOINDEX
	cd .. && "${CURRYDOC}" --noindexhtml "${DOCDIR}" $*

meta/%.html: ../meta/%.curry
	@touch DOINDEX
	cd .. && "${CURRYDOC}" --noindexhtml "${DOCDIR}" $*

# create LaTeX documentation files for system libraries
.PHONY: texdoc
texdoc: $(LIB_CURRY)
	@mkdir -p "${TEXDOCDIR}"
	@if [ ! -f "${TEXDOCDIR}/LAST" ] ; then touch "${TEXDOCDIR}/LAST" ; fi
	@cd "${TEXDOCDIR}" && rm -f meta && ln -s . meta
	@cd "${TEXDOCDIR}" && ${MAKE} -f ../Makefile $(LIB_TEX)
	@cd "${TEXDOCDIR}" && rm -f meta

# generate individual LaTeX documentations for libraries:
%.tex: ../%.curry
	cd .. && "${CURRYDOC}" --tex "${TEXDOCDIR}" $*
	touch LAST

meta/%.tex: ../meta/%.curry
	cd .. && "${CURRYDOC}" --tex "${TEXDOCDIR}" $*
	touch LAST


# clean all generated files
.PHONY: clean
clean:
	rm -f "${DOCDIR}"/*
	rm -f "${TEXDOCDIR}"/*
	../bin/cleancurry
	cd meta && ../../bin/cleancurry

# clean all generated Prolog files
.PHONY: cleanpl
cleanpl:
	rm -f .curry/pakcs/*.pl .curry/pakcs/*.po meta/.curry/pakcs/*.pl meta/.curry/pakcs/*.po \
	      *.pl *.po meta/*.pl meta/*.po
