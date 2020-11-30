#!/usr/bin/env bash

set -ve

REF="${CURRY_LANG_BRANCH}"
TOKEN_USERNAME=project_457_bot
TOKEN_USER_EMAIL=project457_bot@example.com #email without '_' before project id
REPOSITORY=https://${TOKEN_USERNAME}:${CURRY_LANG_ORG_ACCESS_TOKEN}@${CI_SERVER_HOST}/${CURRY_LANG_PROJECT}.git
LATEST_FILE=./curry-lang.org/data/versions/pakcs/latest.version

# configure git (user)name and email
git config user.name "${TOKEN_USERNAME}"
git config user.email "${TOKEN_USER_EMAIL}"

# clone curry-lang.org repo into a subtree
git subtree add --prefix=curry-lang.org ${REPOSITORY} ${REF}

VERSION=$(${CI_PROJECT_DIR}/bin/pakcs --numeric-version)

VERSION_FILE="./curry-lang.org/data/version/pakcs/v${VERSION}.version"

# replace the latest-nightly.version file
cat >"${LATEST_FILE}" <<LATEST_VERSION
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
cat >"${VERSION_FILE}" <<CURRENT_VERSION
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

# push changed back up to curry-lang.org repo
git subtree push --prefix=curry-lang.org ${REPOSITORY} ${REF}
