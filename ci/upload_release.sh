#!/usr/bin/env bash

set -ve

function upload() {
  local local_file=$1
  local upload_file=$2

  curl \
    --header "JOB-TOKEN: ${CI_JOB_TOKEN}" \
    --upload-file "${local_file}" \
    "${PACKAGE_REGISTRY_URL}/${upload_file}"
}

# adjust linked assets in gitlab_release.sh when making changes to the uploaded files

source ci/release_helper.sh

release_helper_init "$1"

for index in ${!LOCAL_FILE_NAMES[*]} ; do
  upload "${LOCAL_FILE_NAMES[$index]}" "${UPLOAD_FILE_NAMES[$index]}"
done
