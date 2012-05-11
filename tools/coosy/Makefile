# Makefile for generating Curry Observer:

.PHONY: all
all: CoosyGUI.state

# generate saved state for GUI:
CoosyGUI.state: CoosyGUI.curry CoosyShowObserve.curry Observe.curry \
	        CoosyTrace.curry CoosyDerive.curry
	../../bin/pakcs -s CoosyGUI

# clean up all auxiliary files:
.PHONY: clean
clean:
	../../bin/cleancurry
	rm -f ../../bin/coosy
