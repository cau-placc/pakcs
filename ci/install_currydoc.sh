#!/bin/bash

set -ve
source ./ci/cypm_helper.sh

cypm_install_binary currydoc "${CURRY_DOC_VERSION}"