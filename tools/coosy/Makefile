# Makefile for generating Curry Observer:

.PHONY: all
all: CoosyGUI

# generate executable for CoosyGUI:
CoosyGUI: CoosyGUI.curry CoosyShowObserve.curry Observe.curry \
	  CoosyTrace.curry CoosyDerive.curry
	$(REPL) $(REPL_OPTS) :load CoosyGUI :save :quit

# clean up all auxiliary files:
.PHONY: clean
clean:
	$(CLEANCURRY)
	rm -f CoosyGUI
