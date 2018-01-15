# generate executable of the partial evaluator:

# Required:
# - installed Curry system (REPL)

.PHONY: all
all:    peval

# generate executable for partial evaluator program:
peval: peval.curry Flat.curry Flat2Fcy.curry
	$(REPL) --nocypm $(REPL_OPTS) :load peval :save :quit

clean: 
	rm -f peval
	$(CLEANCURRY)
	rm -f *_pe.flc *_pe.pl	# delete generated peval files
