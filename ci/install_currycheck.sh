#!/bin/bash

set -ve
source ./ci/cypm_helper.sh

cypm_install_binary currycheck "${CURRY_CHECK_VERSION}"