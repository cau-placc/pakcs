# Makefile for generating Curry Observer:

.PHONY: all
all: CoosyGUI

# generate saved state for GUI:
CoosyGUI: CoosyGUI.curry CoosyShowObserve.curry Observe.curry \
	  CoosyTrace.curry CoosyDerive.curry
	../../bin/pakcs -s CoosyGUI

# clean up all auxiliary files:
.PHONY: clean
clean:
	../../bin/cleancurry
	rm -f ../../bin/coosy
