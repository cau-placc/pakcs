#!/bin/bash

set -ve

locale

swipl --version

gcc --version
# cabal --version
stack --version
#stack config set system-ghc --global true
#stack config set install-ghc --global false # if we need a different ghc the docker image should be updated

# write .cpmrc with expanded variables
# some are also defined as environment variables in .gitlab_ci.yml

cat >~/.cpmrc <<CPMRC
CURRY_BIN=${CI_PROJECT_DIR}/bin/pakcs
REPOSITORY_PATH=${CI_PROJECT_DIR}/.cpm/index
BIN_INSTALL_PATH=${CI_PROJECT_DIR}/.cpm/bin
APP_PACKAGE_PATH=${CI_PROJECT_DIR}/.cpm/apps_pakcs_${CPM_VERSION}
HOME_PACKAGE_PATH=${CI_PROJECT_DIR}/.cpm/pakcs-${CPM_VERSION}-homepackage
PACKAGE_INSTALL_PATH=${CI_PROJECT_DIR}/.cpm/packages
CPMRC

mkdir -p "${CABAL_DIR}/bin" # workaround https://github.com/haskell/cabal/issues/5240
