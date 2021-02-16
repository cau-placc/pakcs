#!/usr/bin/env bash

set -ve

source ci/release_helper.sh

# currently assuming that gitlab release
# will not be used for nightly releases
release_helper_init release

assets=()

for index in ${!LOCAL_FILE_NAMES[*]}; do
  entry="$(
    cat <<ENTRY
{
  "name":     "${UPLOAD_FILE_NAMES[$index]}",
  "url":      "${PACKAGE_REGISTRY_URL}/${UPLOAD_FILE_NAMES[$index]}",
  "filepath": "${UPLOAD_FILE_PATH[$index]}"
}
ENTRY
  )"
  assets+=(--assets-link "${entry}")
done

if [[ ${TEST_BUILD} == "yes" ]]; then
  tag="test-${CI_COMMIT_SHA}"
else
  tag="${CI_COMMIT_TAG}"
fi

release-cli create \
  --name "Release ${release_tag}" \
  --description "Created by the Release CI" \
  --tag-name "${release_tag}" \
  --ref "${CI_COMMIT_SHA}" \
  "${assets[@]}"
