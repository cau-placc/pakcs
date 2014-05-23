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
	$(MAKE) transformers # see below
	cd curry-frontend && $(CABAL_INSTALL) --bindir=$(@D)

# Temporary workaround for https://github.com/haskell/cabal/issues/1855
# see also http://www.haskell.org/pipermail/cabal-devel/2014-May/009795.html
.PHONY: transformers
transformers:
	$(CABAL_INSTALL) transformers-compat -f transformers3

# unregister previous version to avoid cabal install failures
.PHONY: unregister
unregister:
	-$(GHC_UNREGISTER) curry-frontend
	-$(GHC_UNREGISTER) curry-base

.PHONY: clean
clean:
	cd curry-base     && $(CABAL) clean
	cd curry-frontend && $(CABAL) clean
