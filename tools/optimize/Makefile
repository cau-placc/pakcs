# Generate a saved state with the FlatCurry optimizer:

# Required:
# - installed PAKCS

.PHONY: all
all:    CompactFlat NonStrictOpt

# generate saved state for the FlatCurry optimizer:
CompactFlat: CompactFlat.curry
	../../bin/pakcs -s CompactFlat

# generate saved state for the FlatCurry optimizer:
NonStrictOpt: NonStrictOpt.curry
	../../bin/pakcs -set path ../../currytools/browser/analysis -s NonStrictOpt

clean: 
	rm -r CompactFlat NonStrictOpt
	../../bin/cleancurry
	(cd ../../bin ; rm -f nonstrictopt)
