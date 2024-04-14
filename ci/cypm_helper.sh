# helper function to install packages using cypm
# takes the package name as the first parameter
# and the package version as the second parameter
function cypm_install_binary() {
  PACKAGE="$1"
  VERSION="$2"

  echo "Installing ${PACKAGE} at Version ${VERSION} !"

  ./bin/pakcs-cypm \
    -d curry_bin="${CI_PROJECT_DIR}/bin/pakcs" \
    -d bin_install_path="${CI_PROJECT_DIR}/bin" \
    -d app_package_path="${CI_PROJECT_DIR}/app_packages" \
    install "${PACKAGE}" "${VERSION}"
}

# helper function to run cypm update
function cypm_update() {
  ./bin/pakcs-cypm \
    -d curry_bin="${CI_PROJECT_DIR}/bin/pakcs" \
    -d bin_install_path="${CI_PROJECT_DIR}/bin" \
    -d app_package_path="${CI_PROJECT_DIR}/app_packages" \
    update
}
