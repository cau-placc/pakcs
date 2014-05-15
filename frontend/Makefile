##############################################################################
# Installation of frontend
##############################################################################

# Retrieve all Haskell modules as dependencies for cymake
DEPS := $(shell find curry-base/src/ curry-frontend/src/ -type f)

# install the sources of the front end from its repository
$(CYMAKE): $(DEPS)
	$(MAKE) unregister
	mkdir -p $(@D)
	cd curry-base     && $(CABAL_INSTALL)
	cd curry-frontend && $(CABAL_INSTALL) --bindir=$(@D)

# unregister previous version to avoid cabal install failures
.PHONY: unregister
unregister:
	-$(GHC_UNREGISTER) curry-frontend
	-$(GHC_UNREGISTER) curry-base

.PHONY: clean
clean:
	cd curry-base     && $(CABAL) clean
	cd curry-frontend && $(CABAL) clean
