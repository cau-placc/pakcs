##############################################################################
# Makefile for installing the Curry front end
##############################################################################

# the root directory of the installation
export ROOT=$(CURDIR)
# binary directory and executables
export BINDIR=$(ROOT)/bin
# The frontend binary
export CYMAKE = $(BINDIR)/cymake

# GHC and CABAL configuration (for installing the front end)
# ----------------------------------------------------------
# The path to the Glasgow Haskell Compiler and Cabal
export GHC     := $(shell which ghc)
export GHC-PKG := $(shell dirname "$(GHC)")/ghc-pkg
export CABAL    = cabal

# Because of an API change in GHC 7.6,
# we need to distinguish GHC < 7.6 and GHC >= 7.6.
# GHC 7.6 renamed the option "package-conf" to "package-db".

# extract GHC version
ifdef GHC
GHC_MAJOR := $(shell "$(GHC)" --numeric-version | cut -d. -f1)
GHC_MINOR := $(shell "$(GHC)" --numeric-version | cut -d. -f2)
# Is the GHC version >= 7.6 ?
GHC_GEQ_76 = $(shell test $(GHC_MAJOR) -gt 7 -o \( $(GHC_MAJOR) -eq 7 \
              -a $(GHC_MINOR) -ge 6 \) ; echo $$?)
# package-db (>= 7.6) or package-conf (< 7.6)?
ifeq ($(GHC_GEQ_76),0)
GHC_PKG_OPT = package-db
else
GHC_PKG_OPT = package-conf
endif
endif

# Command to unregister a package
export GHC_UNREGISTER = "$(GHC-PKG)" unregister --$(GHC_PKG_OPT)="$(PKGDB)"
# Command to install missing packages using cabal
export CABAL_INSTALL  = "$(CABAL)" install --with-compiler="$(GHC)"       \
                        --with-hc-pkg="$(GHC-PKG)" --prefix="$(LOCALPKG)" \
                        --global --package-db="$(PKGDB)" -O2

# Directory where local package installations are stored
export LOCALPKG      = $(ROOT)/pkg
# The path to the package database
export PKGDB         = $(LOCALPKG)/pakcs.conf.d

##############################################################################
# Retrieve all Haskell modules as dependencies for cymake
DEPS := $(shell find curry-base/src/ curry-frontend/src/ -type f)

# install front end (if sources are present):
.PHONY: frontend
frontend:
ifdef GHC
	$(MAKE) $(PKGDB)
	$(MAKE) $(CYMAKE)
else
	@echo "GHC missing, cannot build front end!"
	@exit 1
endif

# create package database
$(PKGDB):
	"$(GHC-PKG)" init $@
	$(CABAL) update

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

.PHONY: cleanall
cleanall:
	cd curry-base     && rm -rf dist/
	cd curry-frontend && rm -rf dist/
	rm -rf $(LOCALPKG)
	rm -f $(CYMAKE)   && rm -rf bin
