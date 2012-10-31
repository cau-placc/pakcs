# Generate a saved state with the FlatCurry optimizer:

# Required:
# - installed curry2prolog system

.PHONY: all
all:    CompactFlat.state NonStrictOpt.state

# generate saved state for the FlatCurry optimizer:
CompactFlat.state: CompactFlat.curry
	../../bin/pakcs -s CompactFlat

# generate saved state for the FlatCurry optimizer:
NonStrictOpt.state: NonStrictOpt.curry
	../../bin/pakcs -set path ../../currytools/browser/analysis -s NonStrictOpt

clean: 
	../../bin/cleancurry
	(cd ../../bin ; rm -f nonstrictopt)
