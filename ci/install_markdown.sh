#!/bin/bash

set -ve
source ./ci/cypm_helper.sh

cypm_install_binary markdown "${MARKDOWN_VERSION}"