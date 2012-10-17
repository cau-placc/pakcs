##############################################################################
# Installation of frontend
##############################################################################

CYMAKE_VERSION=0.3.3
CYMAKE_BIN    =$(LOCALBIN)/cymake

# install the sources of the front end from its repository
$(CYMAKE_BIN): unregister
	mkdir -p $(@D)
	cd curry-base     && cabal install
	cd curry-frontend && cabal install --bindir=$(@D)

# unregister previous version to avoid cabal install failures
.PHONY: unregister
unregister:
	-ghc-pkg unregister curry-frontend-$(CYMAKE_VERSION)
	-ghc-pkg unregister curry-base-$(CYMAKE_VERSION)

.PHONY: update
update:
	cd curry-base     && git pull
	cd curry-frontend && git pull

.PHONY: clean
clean:
	cd curry-base     && cabal clean
	cd curry-frontend && cabal clean
