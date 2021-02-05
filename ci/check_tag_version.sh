#!/bin/bash

# if this is not a test release and the tag does not match the build pakcs version
# give an error
if  [[ $TEST_RELEASE != "yes"  && $CI_COMMIT_TAG != "v${BUILD_PAKCS_VERSION}" ]] ; then
    echo "Expected git tag ${CI_COMMIT_TAG} to match version v${BUILD_PAKCS_VERSION}"
    exit 1
fi

