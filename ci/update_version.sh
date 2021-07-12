#!/usr/bin/env bash

set -ve

# init variables used
# and setup the subtree
function init() {

  REPOSITORY="https://${CURRY_LANG_USER}:${CURRY_LANG_ORG_ACCESS_TOKEN}@${CI_SERVER_HOST}/${CURRY_LANG_PROJECT}.git"

  VERSION="${BUILD_PAKCS_VERSION}"

  # the path to the folder where the pakcs version files will be created/updated
  VERSION_FOLDER_PATH=./curry-lang.org/data/versions/pakcs

  VERSION_FILE="${VERSION_FOLDER_PATH}/v${VERSION}.version"
  NIGHTLY_FILE="${VERSION_FOLDER_PATH}/latest-nighly.version"
  LATEST_FILE="${VERSION_FOLDER_PATH}/latest.version"

  # configure git (user)name and email
  git config user.name "${CURRY_LANG_USER}"
  git config user.email "${CURRY_LANG_EMAIL}"

  # clone curry-lang.org repo into a subtree
  git subtree add --prefix=curry-lang.org "${REPOSITORY}" "${CURRY_LANG_BRANCH}"

}

# finish the version update by pushing the changes of the subtree back to the repo
function finish() {

  # push changed back up to curry-lang.org repo
  git subtree push --prefix=curry-lang.org "${REPOSITORY}" "${CURRY_LANG_BRANCH}"

}

# update the latest nightly version
function nightly() {

  release_helper_init nightly

  # replace the latest-nightly.version file
  install -D /dev/stdin "${NIGHTLY_FILE}" <<NIGHTLY_VERSION
---
# THIS FILE IS GENERATED AUTOMATICALLY BY THE PACKS NIGHTLY PIPELINE
# MANUAL CHANGES TO THIS FILE WILL GET LOST ON THE NEXT NIGHTLY RUN
date: ${BUILD_DATE}
version: Latest Nightly ${CI_COMMIT_SHORT_SHA} ($(date --date="${BUILD_DATE}" +%0d/%0m/%y))
source: ${nightly_src_download}
linux:  ${nightly_arch_download}
manual: ${nightly_manual_download}
commit_sha: ${CI_COMMIT_SHA}
commit_short_sha: ${CI_COMMIT_SHORT_SHA}
ci_job: ${CI_JOB_ID}
pipeline_url: ${CI_PIPELINE_URL}
---
NIGHTLY_VERSION

  # commit updated file
  git add "${NIGHTLY_FILE}"
  # regarding the mentioned issue it may still be wanted to keep it this way to keep the pipelines linked
  git commit -F - <<COMMIT_MSG
[Pakcs CI] Update Nightly

CI triggered later via Pipeline https://gitlab.com/gitlab-org/gitlab/-/issues/246784
[skip ci]
COMMIT_MSG

}

# update the latest release and create a release entry
function release() {

  release_helper_init release

  # replace the latest.version file
  install -D /dev/stdin "${LATEST_FILE}" <<LATEST_VERSION
---
# THIS FILE IS GENERATED AUTOMATICALLY BY THE PACKS RELEASE PIPELINE
# MANUAL CHANGES TO THIS FILE WILL GET LOST ON THE NEXT RELEASE RUN
date: ${BUILD_DATE}
version: Latest
source: ${src_download}
linux:  ${arch_download}
manual: ${manual_download}
commit_sha: ${CI_COMMIT_SHA}
commit_short_sha: ${CI_COMMIT_SHORT_SHA}
ci_job: ${CI_JOB_ID}
pipeline_url: ${CI_PIPELINE_URL}
---
LATEST_VERSION

  # don't include warning here as this file is usually not regenerated
  install -D /dev/stdin "${VERSION_FILE}" <<CURRENT_VERSION
---
# THIS FILE WAS GENERATED AUTOMATICALLY BY THE PACKS RELEASE PIPELINE
date: ${BUILD_DATE}
version: v${VERSION} ($(date --date="${BUILD_DATE}" +%0d/%0m/%y))
source: ${src_download}
linux:  ${arch_download}
manual: ${manual_download}
commit_sha: ${CI_COMMIT_SHA}
commit_short_sha: ${CI_COMMIT_SHORT_SHA}
ci_job: ${CI_JOB_ID}
pipeline_url: ${CI_PIPELINE_URL}
---
CURRENT_VERSION

  # commit updated file
  git add "${LATEST_FILE}" "${VERSION_FILE}"
  # regarding the mentioned issue it may still be wanted to keep it this way to keep the pipelines linked
  git commit -F - <<COMMIT_MSG
[Pakcs CI] Update Release

Releasing Version ${VERSION}

CI triggered later via Pipeline https://gitlab.com/gitlab-org/gitlab/-/issues/246784
[skip ci]
COMMIT_MSG

}

source ci/release_helper.sh

init

case $1 in
release)
  release
  ;;
nightly)
  nightly
  ;;
*)
  echo "Expected first parameter to be 'release' or 'nightly' got '$1'"
  exit 1
  ;;
esac

finish
