#!/bin/bash

set -ve

./bin/pakcs-cypm  \
    -d curry_bin="${CI_PROJECT_DIR}/bin/pakcs" \
    -d bin_install_path="${CI_PROJECT_DIR}/bin" \
    -d app_package_path="${CI_PROJECT_DIR}/app_packages" \
    install markdown