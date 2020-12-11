#!/usr/bin/env bash

VERSION=$("${CI_PROJECT_DIR}"/bin/pakcs --numeric-version)

# fullname and arch copied from makefile
fullname="pakcs-${VERSION}"
arch="$(dpkg-architecture -qDEB_BUILD_ARCH)-$(uname -s)"

case $1 in
release)
  upload_suffix=""
  ;;
nightly)
  upload_suffix="-nightly-${CI_COMMIT_SHORT_SHA}"
  ;;
*)
  echo "Expected first parameter have either value 'release' or 'nightly' got '$1'"
  exit 1
  ;;
esac

function upload() {
  local local_file=$1
  local upload_file=$2

  curl \
    --header "JOB-TOKEN: ${CI_JOB_TOKEN}" \
    --upload-file "${local_file}" \
    "${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${fullname}${upload_suffix}/${VERSION}/${upload_file}"
}

upload "pakcs-${VERSION}-src.tar.gz" "${fullname}${upload_suffix}-src.tar.gz"
upload "pakcs-${VERSION}-${arch}.tar.gz" "${fullname}${upload_suffix}-${arch}.tar.gz"
upload "docs/Manual.pdf" "${fullname}${upload_suffix}-manual.pdf"