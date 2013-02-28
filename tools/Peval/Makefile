# generate a saved state with the partial evaluator:

# Required:
# - installed curry2prolog system

.PHONY: all
all:    peval

# generate saved state for check program:
peval: peval.curry Flat.curry Flat2Fcy.curry
	../../bin/pakcs -set -error -s peval

clean: 
	../../bin/cleancurry
	rm -f *_pe.flc *_pe.pl	# delete generated peval files
