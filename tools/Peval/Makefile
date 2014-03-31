# generate executable of the partial evaluator:

# Required:
# - installed curry2prolog system

.PHONY: all
all:    peval

# generate executable for partial evaluator program:
peval: peval.curry Flat.curry Flat2Fcy.curry
	../../bin/pakcs :load peval :save :quit

clean: 
	rm -f peval
	../../bin/cleancurry
	rm -f *_pe.flc *_pe.pl	# delete generated peval files
