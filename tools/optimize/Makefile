# Generate a saved state with the FlatCurry optimizer:

# Required:
# - installed PAKCS

.PHONY: all
all:    CompactFlat NonStrictOpt

# generate saved state for the FlatCurry optimizer:
CompactFlat: CompactFlat.curry
	$(REPL) $(REPL_OPTS) :load CompactFlat :save :quit

# generate saved state for the FlatCurry optimizer:
NonStrictOpt: NonStrictOpt.curry
	$(REPL) $(REPL_OPTS) :set path $(ROOT)/currytools/browser/analysis \
	                     :load NonStrictOpt :save :quit

clean: 
	rm -f CompactFlat NonStrictOpt
	$(CLEANCURRY)
	(cd ../../bin ; rm -f nonstrictopt)
