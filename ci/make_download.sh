#!/usr/bin/env bash

set -ve

source ci/section_helper.sh

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

  mkdir -p download
  pushd download

  VERSION=$1   # version number
  DLVERSION=$2 # download version (src, amd64-Linux)

  PAKCSVERSION="pakcs-${VERSION}"

  rm -rf "${PAKCSVERSION}"

  start_section "download_${PAKCSVERSION}_${DLVERSION}" "Downloading ${PAKCSVERSION}-${DLVERSION}"

  # download distribution
  wget http://www.informatik.uni-kiel.de/~pakcs/download/${PAKCSVERSION}-${DLVERSION}.tar.gz
  tar xvzf ${PAKCSVERSION}-${DLVERSION}.tar.gz
  # change owner of distribution to avoid problems with Haskell's stack:
  chown -R --reference=${PAKCSVERSION}-${DLVERSION}.tar.gz "${PAKCSVERSION}"
  rm ${PAKCSVERSION}-${DLVERSION}.tar.gz

  end_section "download_${PAKCSVERSION}_${DLVERSION}"

  pushd "${PAKCSVERSION}"

  start_section "build_${PAKCSVERSION}_${DLVERSION}" "Building ${PAKCSVERSION}-${DLVERSION}"

  # build system
  make CI_BUILD=yes
  bin/curry --nocypm :load AllLibraries :eval "3*13+3" :quit

  end_section "build_${PAKCSVERSION}_${DLVERSION}"

  start_section "test_${PAKCSVERSION}_${DLVERSION}" "Testing ${PAKCSVERSION}-${DLVERSION}"

  # run unit tests
  gen_currycheck
  make CI_BUILD=yes runtestverbose

  end_section "test_${PAKCSVERSION}_${DLVERSION}"

  popd # pop "${PAKCSVERSION}"
  popd # pop download
}
