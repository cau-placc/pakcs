#!/usr/bin/env bash

set -ve

REF="${CURRY_LANG_BRANCH}"
TOKEN_USERNAME=project_457_bot
TOKEN_USER_EMAIL=project457_bot@example.com #email without '_' before project id
NIGHTLY_FILE=./curry-lang.org/data/versions/pakcs/latest-nighly.version
REPOSITORY=https://${TOKEN_USERNAME}:${CURRY_LANG_ORG_ACCESS_TOKEN}@${CI_SERVER_HOST}/${CURRY_LANG_PROJECT}.git

# configure git (user)name and email
git config user.name "${TOKEN_USERNAME}"
git config user.email "${TOKEN_USER_EMAIL}"

# clone curry-lang.org repo into a subtree
git subtree add --prefix=curry-lang.org ${REPOSITORY} ${REF}

VERSION=$(${CI_PROJECT_DIR}/bin/pakcs --numeric-version)

# replace the latest-nightly.version file
cat >"${NIGHTLY_FILE}" <<NIGHTLY_VERSION
---
# THIS FILE IS GENERATED AUTOMATICALLY BY THE PACKS NIGHLY PIPELINE
# MANUAL CHANGES TO THIS FILE WILL GET LOST ON THE NEXT NIGHTY RUN
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

# push changed back up to curry-lang.org repo
git subtree push --prefix=curry-lang.org ${REPOSITORY} ${REF}
