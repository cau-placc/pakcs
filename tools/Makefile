# generate various tools for handling Curry programs:

# Required:
# - installed Curry system

.PHONY: all
all:    curry2xml
	@cd optimize  ; $(MAKE)
	@cd xmldata   ; $(MAKE)
	@cd Peval     ; $(MAKE)
	@cd coosy     ; $(MAKE)
	@cd ui        ; $(MAKE)


# generate saved state for curry2xml program:
curry2xml: curry2xml.curry $(LIBDIR)/meta/FlatXML.curry \
	         $(LIBDIR)/meta/Flat.curry \
	         $(LIBDIR)/meta/Flat2Fcy.curry $(LIBDIR)/XML.curry
	$(REPL) $(REPL_OPTS) :load curry2xml :save :quit

.PHONY: clean
clean: 
	$(CLEANCURRY)
	rm -f curry2xml
	cd Peval     ; $(MAKE) clean
	cd coosy     ; $(MAKE) clean
	cd optimize  ; $(MAKE) clean
	cd xmldata   ; $(MAKE) clean
	cd ui        ; $(MAKE) clean
