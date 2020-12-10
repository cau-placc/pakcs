#!/usr/bin/env bash

set -ve

function init() {

  REF="${CURRY_LANG_BRANCH}"
  TOKEN_USERNAME="${CURRY_LANG_USER}"
  TOKEN_USER_EMAIL="${CURRY_LANG_EMAIL}"
  REPOSITORY="https://${TOKEN_USERNAME}:${CURRY_LANG_ORG_ACCESS_TOKEN}@${CI_SERVER_HOST}/${CURRY_LANG_PROJECT}.git"

  VERSION=$("${CI_PROJECT_DIR}"/bin/pakcs --numeric-version)

  VERSION_FILE="./curry-lang.org/data/version/pakcs/v${VERSION}.version"
  NIGHTLY_FILE=./curry-lang.org/data/versions/pakcs/latest-nighly.version
  LATEST_FILE=./curry-lang.org/data/versions/pakcs/latest.version

  # configure git (user)name and email
  git config user.name "${TOKEN_USERNAME}"
  git config user.email "${TOKEN_USER_EMAIL}"

  # clone curry-lang.org repo into a subtree
  git subtree add --prefix=curry-lang.org "${REPOSITORY}" "${REF}"

}

function finish() {

  # push changed back up to curry-lang.org repo
  git subtree push --prefix=curry-lang.org "${REPOSITORY}" "${REF}"

}

function nightly() {

  # replace the latest-nightly.version file
  install -D /dev/stdin "${NIGHTLY_FILE}" <<NIGHTLY_VERSION
---
# THIS FILE IS GENERATED AUTOMATICALLY BY THE PACKS NIGHTLY PIPELINE
# MANUAL CHANGES TO THIS FILE WILL GET LOST ON THE NEXT NIGHTLY RUN
date: $(date +%Y-%0m-%0d)
version: Latest Nightly ${CI_COMMIT_SHORT_SHA} ($(date +%0d/%0m/%y))
source: https://git.ps.informatik.uni-kiel.de/curry/pakcs/-/jobs/${BUNDLE_JOB_ID}/artifacts/raw/pakcs-${VERSION}-src.tar.gz
linux:  https://git.ps.informatik.uni-kiel.de/curry/pakcs/-/jobs/${BUNDLE_JOB_ID}/artifacts/raw/pakcs-${VERSION}-amd64-Linux.tar.gz
manual: https://git.ps.informatik.uni-kiel.de/curry/pakcs/-/jobs/${MANUAL_JOB_ID}/artifacts/raw/docs/Manual.pdf
commit_sha: ${CI_COMMIT_SHA}
commit_short_sha: ${CI_COMMIT_SHORT_SHA}
ci_job: ${CI_JOB_ID}
pipeline_url: ${CI_PIPELINE_URL}
---
NIGHTLY_VERSION

  # commit updated file
  git add "${NIGHTLY_FILE}"
  git commit -F - <<COMMIT_MSG
[Pakcs CI] Update Nightly

CI triggered later via Pipeline https://gitlab.com/gitlab-org/gitlab/-/issues/246784
[skip ci]
COMMIT_MSG

}


function release() {

  # replace the latest.version file
  install -D /dev/stdin "${LATEST_FILE}" <<LATEST_VERSION
---
# THIS FILE IS GENERATED AUTOMATICALLY BY THE PACKS RELEASE PIPELINE
# MANUAL CHANGES TO THIS FILE WILL GET LOST ON THE NEXT RELEASE RUN
date: $(date +%Y-%0m-%0d)
version: Latest
source: https://www.informatik.uni-kiel.de/~pakcs/download/pakcs-${VERSION}-src.tar.gz
linux:  https://www.informatik.uni-kiel.de/~pakcs/download/pakcs-${VERSION}-amd64-Linux.tar.gz
manual: https://www.informatik.uni-kiel.de/~pakcs/download/pakcs-${VERSION}-manual.pdf
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
date: $(date +%Y-%0m-%0d)
version: v${VERSION} $(date +%Y-%0m-%0d)
source: https://www.informatik.uni-kiel.de/~pakcs/download/pakcs-${VERSION}-src.tar.gz
linux:  https://www.informatik.uni-kiel.de/~pakcs/download/pakcs-${VERSION}-amd64-Linux.tar.gz
manual: https://www.informatik.uni-kiel.de/~pakcs/download/pakcs-${VERSION}-manual.pdf
commit_sha: ${CI_COMMIT_SHA}
commit_short_sha: ${CI_COMMIT_SHORT_SHA}
ci_job: ${CI_JOB_ID}
pipeline_url: ${CI_PIPELINE_URL}
---
CURRENT_VERSION

  # commit updated file
  git add "${LATEST_FILE}" "${VERSION_FILE}"
  git commit -F - <<COMMIT_MSG
[Pakcs CI] Update Release

Releasing Version ${VERSION}

CI triggered later via Pipeline https://gitlab.com/gitlab-org/gitlab/-/issues/246784
[skip ci]
COMMIT_MSG

}

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
