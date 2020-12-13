#!/usr/bin/env bash

set -ve

source ci/release_helper.sh

# currently assuming that gitlab release
# will not be used for nightly releases
release_helper_init release

assets=()

for index in ${!LOCAL_FILE_NAMES[*]}; do
  assets+=(--assets-link "{\"name\":\"${UPLOAD_FILE_NAMES[$index]}\",\"url\":\"${PACKAGE_REGISTRY_URL}/${UPLOAD_FILE_NAMES[$index]}\", \"filepath\":\"${UPLOAD_FILE_NAMES[$index]}\"}")
done

if [[ ${TEST_BUILD} == "yes" ]] ; then
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
