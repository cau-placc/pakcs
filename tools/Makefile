# generate various tools for handling Curry programs:

.PHONY: all
all:
	@cd optimize  && $(MAKE)
	@cd Peval     && $(MAKE)
	@cd coosy     && $(MAKE)
	@cd ui        && $(MAKE)

.PHONY: clean
clean:
	cd Peval          && $(MAKE) clean
	cd coosy          && $(MAKE) clean
	cd optimize       && $(MAKE) clean
	cd ui             && $(MAKE) clean
