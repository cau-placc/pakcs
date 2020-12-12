# initializes variables used by
#  - gitlab_release.sh
#  - upload_release.sh
#  - update_version.sh
# as they need to match
#
# takes one parameter indicating whether this is a nightly release or a release release
function release_helper_init() {

  version=$("${CI_PROJECT_DIR}"/bin/pakcs --numeric-version)
  arch="$(dpkg-architecture -qDEB_BUILD_ARCH)-$(uname -s)"

  # FULLNAME and ARCH copied from makefile
  package_name="pakcs"
  name="pakcs-${version}"

  case $1 in
  release)
    if [[ ${TEST_BUILD} == "yes" ]]; then
      package_suffix="-test"
      name_suffix="-test"
      release_tag="test-${CI_COMMIT_SHA}"
    elif [[ ${CI_COMMIT_TAG} =~ /^v\d*.\d*.\d*$/i ]]; then
      package_suffix="-release"
      name_suffix=""
      release_tag="${CI_COMMIT_TAG}"
    else
      # not a version number tag, don't occupy the version for the release package
      package_suffix="-release-${CI_COMMIT_TAG}"
      name_suffix="-${CI_COMMIT_TAG}"
      # this could result in a race condition when building nightly
      # around a date change e.g. midnight this script will run multiple times over the different stages
      release_tag="nightly-$(date +%F)"
    fi
    ;;
  nightly)
    # would like to have the commit hash be part of the version
    # but gitlab restricts the version to the regex /\A\d+.\d+.\d+.\z/
    # so we add it as a suffix to the package name and the file name
    # this sadly means we create a package per nightly
    package_suffix="-nightly-${CI_COMMIT_SHORT_SHA}"
    name_suffix="-nightly-${CI_COMMIT_SHORT_SHA}"
    ;;
  *)
    echo "Expected first parameter have either value 'release' or 'nightly' got '$1'"
    exit 1
    ;;
  esac

  full_name="${name}${name_suffix}"
  full_package_name="${package_name}${package_suffix}"

  PACKAGE_REGISTRY_URL="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${full_package_name}/${version}"

  # where to finde the file on the runners disk
  LOCAL_FILE_NAMES=(
    "${name}-src.tar.gz"
    "${name}-${arch}.tar.gz"
    "docs/Manual.pdf"
  )

  # name given to the file in the package and for the release asset
  UPLOAD_FILE_NAMES=(
    "${full_name}-src.tar.gz"
    "${full_name}-${arch}.tar.gz"
    "${full_name}-manual.pdf"
  )

}
