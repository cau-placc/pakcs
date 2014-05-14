##############################################################################
# Installation of frontend
##############################################################################

CYMAKE           = $(BINDIR)/cymake
BASE_VERSION     := $(shell sed -n 's/^Version *: *\(.*\)$$/\1/p' curry-base/curry-base.cabal)
FRONTEND_VERSION := $(shell sed -n 's/^Version *: *\(.*\)$$/\1/p' curry-frontend/curry-frontend.cabal)

# install the sources of the front end from its repository
$(CYMAKE): unregister
	mkdir -p $(@D)
	cd curry-base     && cabal install
	cd curry-frontend && cabal install --bindir=$(@D)

# unregister previous version to avoid cabal install failures
.PHONY: unregister
unregister:
	-ghc-pkg unregister curry-frontend-$(FRONTEND_VERSION)
	-ghc-pkg unregister curry-base-$(BASE_VERSION)

.PHONY: update
update:
	cd curry-base     && git pull
	cd curry-frontend && git pull

.PHONY: clean
clean:
	cd curry-base     && cabal clean
	cd curry-frontend && cabal clean
