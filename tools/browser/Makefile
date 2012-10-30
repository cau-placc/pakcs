# generate Curry browser:

# Required:
# - installed curry2prolog system

LIB=../../lib
META=${LIB}/meta
TOOLS=..
CURRYTOOLS=../../currytools

# generate saved state for currytest program:
BrowserGUI.state: BrowserGUI.curry ShowFlatCurry.curry Imports.curry \
	          AnalysisTypes.curry BrowserAnalysis.curry ShowGraph.curry \
	          FlatCurryGoodies.curry \
	          ${TOOLS}/AddTypes.curry \
	          ${CURRYTOOLS}/importcalls/ImportCalls.curry \
	          ${LIB}/GUI.curry ${LIB}/IOExts.curry  ${LIB}/System.curry \
	          ${META}/FlatCurry.curry ${META}/FlatCurryShow.curry \
	          analysis/*.curry
	(echo "patchReadmeVersion" ; echo ":set path" ; echo ":save main") | \
	           ../../bin/pakcs -set +error -set path analysis:$(TOOLS):$(CURRYTOOLS)/importcalls -l BrowserGUI
	(cd ../../bin ; rm -f currybrowse ; \
	                ln -s ../tools/browser/BrowserGUI.state currybrowse)

.PHONY: clean
clean: 
	../../bin/cleancurry -r
	rm -f ../../bin/currybrowse
