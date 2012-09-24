##############################################################################
# Installation of frontend
##############################################################################

CYMAKE = ../bin/.local/cymake

# copy cabal installation of front end into local directory
$(CYMAKE): $(HOME)/.cabal/bin/cymake
	mkdir -p $(@D)
	mv $< $@ # move cabal executable to local bin

# install the sources of the front end from its repository
$(HOME)/.cabal/bin/cymake: unregister
	cd curry-base     && cabal install
	cd curry-frontend && cabal install

# unregister previous version to avoid cabal install failures
.PHONY: unregister
unregister:
	-ghc-pkg unregister curry-frontend
	-ghc-pkg unregister curry-base

.PHONY: update
update:
	cd curry-base     && git pull
	cd curry-frontend && git pull

.PHONY: clean
clean:
	cd curry-base     && cabal clean
	cd curry-frontend && cabal clean
