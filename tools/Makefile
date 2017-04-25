# generate various tools for handling Curry programs:

# Required:
# - installed Curry system

# directory with deprecated libraries:
OLDLIBDIR=$(ROOT)/tools/lib_deprecated

.PHONY: all
all:
	@cd optimize  ; $(MAKE)
	@cd Peval     ; $(MAKE)
	@cd coosy     ; $(MAKE)


.PHONY: clean
clean:
	$(CLEANCURRY)
	rm -f curry2xml
	cd Peval          && $(MAKE) clean
	cd coosy          && $(MAKE) clean
	cd optimize       && $(MAKE) clean
	cd lib_deprecated && $(CLEANCURRY) -r
