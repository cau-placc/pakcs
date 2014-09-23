# generate various tools for handling Curry programs:

# Required:
# - installed Curry system

# directory with deprecated libraries:
OLDLIBDIR=$(ROOT)/tools/lib_deprecated

.PHONY: all
all:    curry2xml
	@cd optimize  ; $(MAKE)
	@cd Peval     ; $(MAKE)
	@cd coosy     ; $(MAKE)
	@cd ui        ; $(MAKE)


# generate saved state for curry2xml program:
curry2xml: curry2xml.curry $(OLDLIBDIR)/FlatXML.curry \
	         $(OLDLIBDIR)/Flat.curry \
	         $(OLDLIBDIR)/Flat2Fcy.curry $(LIBDIR)/XML.curry
	$(REPL) $(REPL_OPTS) :set path $(OLDLIBDIR) :load curry2xml :save :quit

.PHONY: clean
clean: 
	$(CLEANCURRY)
	rm -f curry2xml
	cd Peval     ; $(MAKE) clean
	cd coosy     ; $(MAKE) clean
	cd optimize  ; $(MAKE) clean
	cd ui        ; $(MAKE) clean
