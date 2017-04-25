# generate various tools for handling Curry programs:

# Required:
# - installed Curry system

.PHONY: all
all:
	@cd optimize  ; $(MAKE)
	@cd Peval     ; $(MAKE)


.PHONY: clean
clean:
	$(CLEANCURRY)
	cd Peval          && $(MAKE) clean
	cd optimize       && $(MAKE) clean
	cd lib_deprecated && $(CLEANCURRY) -r
