# Generate a saved state for the Curry2JavaScript translator:

# Required:
# - installed curry2prolog system

LIB=../../lib

.PHONY: all
all:    Curry2JS.state

# generate saved state for the Curry2JavaScript translator:
Curry2JS.state: Curry2JS.curry ${LIB}/JavaScript.curry \
	        ${LIB}/meta/CompactFlatCurry.curry ${LIB}/meta/FlatCurry.curry
	../../bin/pakcs -s Curry2JS

clean: 
	../../bin/cleancurry
