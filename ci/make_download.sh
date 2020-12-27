#!/usr/bin/env bash

set -ve

# Generate local version of CurryCheck:
gen_currycheck() {
  bin/cypm \
      -d curry_bin=$(pwd)/bin/pakcs \
      -d bin_install_path=$(pwd)/bin \
      -d app_package_path=$(pwd)/app_packages \
      update
  bin/cypm \
      -d curry_bin=$(pwd)/bin/pakcs \
      -d bin_install_path=$(pwd)/bin \
      -d app_package_path=$(pwd)/app_packages \
      install currycheck "${CURRY_CHECK_VERSION}"

  ln -s $(pwd)/bin/curry-check $(pwd)/bin/pakcs-check
  PATH=$(pwd)/bin:${PATH}
  export PATH
}

build_download_pakcs() {
  VERSION=$1   # version number
  DLVERSION=$2 # download version (src, amd64-Linux)

  PAKCSVERSION="pakcs-${VERSION}"

  rm -rf "${PAKCSVERSION}"

  # download dirtibution
  wget http://www.informatik.uni-kiel.de/~pakcs/download/${PAKCSVERSION}-${DLVERSION}.tar.gz
  tar xvzf ${PAKCSVERSION}-${DLVERSION}.tar.gz
  rm ${PAKCSVERSION}-${DLVERSION}.tar.gz

  pushd "${PAKCSVERSION}"

  # build system
  make CI_BUILD=yes
  bin/curry :load AllLibraries :eval "3*13+3" :quit

  # run unit tests
  gen_currycheck
  make CI_BUILD=yes runtestverbose

  popd
}

mkdir -p download
pushd download

build_download_pakcs "${DOWNLOAD_VERSION}" src
build_download_pakcs "${DOWNLOAD_VERSION}" amd64-Linux

popd
