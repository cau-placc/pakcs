# initializes variables used by
#  - gitlab_release.sh
#  - upload_release.sh
#  - update_version.sh
# as they need to match
#
# takes one parameter indicating whether this is a nightly release or a release release
function release_helper_init() {

  arch="${BUILD_PAKCS_ARCH}"

  # FULLNAME and ARCH copied from makefile
  package_name="pakcs"
  name="pakcs-${BUILD_PAKCS_VERSION}"

  name_suffix=""
  release_tag=""
  pre_release=""
  build=""

  case $1 in
  release)
    if [[ ${TEST_RELEASE} == "yes" ]]; then
      release_tag="test-${CI_COMMIT_SHORT_SHA}"
      pre_release="test"
      build="${CI_COMMIT_SHORT_SHA}"
    elif [[ ${CI_COMMIT_TAG} =~ ^(v|V)[0-9]*\.[0-9]*\.[0-9]*$ ]]; then
      release_tag="${CI_COMMIT_TAG}"
    elif [[ -n ${CI_COMMIT_TAG} ]]; then
      name_suffix="-${CI_COMMIT_TAG}"
      release_tag="${CI_COMMIT_TAG}"
      pre_release="tag"
      build="${CI_COMMIT_TAG}"
    else
      echo "Release should be triggered by either TEST_RELEASE variable being 'yes'"
      echo "or by git tag, nighter is the case."
      echo "Value of TEST_RELEASE: ${TEST_RELEASE}"
      echo "Value of CI_COMMIT_TAG: ${CI_COMMIT_TAG}"
      exit 1
    fi
    ;;
  nightly)
    name_suffix="-${BUILD_DATE}-${CI_COMMIT_SHORT_SHA}"
    release_tag="nightly-${BUILD_DATE}"
    pre_release="nightly"
    build="${BUILD_DATE}.${CI_COMMIT_SHORT_SHA}"
    ;;
  *)
    echo "Expected first parameter have either value 'release' or 'nightly' got '$1'"
    exit 1
    ;;
  esac

  version="${BUILD_PAKCS_VERSION}${pre_release:+-$pre_release}${build:++${build}}"
  full_name="${name}${pre_release:+-$pre_release}${name_suffix}"

  PACKAGE_REGISTRY_URL="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${package_name}/${version}"

  # where to find the file on the runners disk
  LOCAL_FILE_NAMES=(
    "${name}-src.tar.gz"
    "${name}-${arch}.tar.gz"
    "docs/Manual.pdf"
  )

  # name given to the file in the package and for the release asset link
  UPLOAD_FILE_NAMES=(
    "${full_name}-src.tar.gz"
    "${full_name}-${arch}.tar.gz"
    "${full_name}-manual.pdf"
  )

  # for now place all files under other
  for index in ${!UPLOAD_FILE_NAMES[*]} ; do
    UPLOAD_FILE_PATH[$index]="/other/${UPLOAD_FILE_NAMES[$index]}"
  done

  DOWNLOAD_URL="${CI_PROJECT_URL}/-/releases/${release_tag}/downloads"

  # these variables are used by update_version.sh when doing a release
  # for the download links on curry-lang.org
  # these urls are meaningless for nightly releases as it does not create gitlab release
  src_download="${DOWNLOAD_URL}${UPLOAD_FILE_PATH[0]}"
  arch_download="${DOWNLOAD_URL}${UPLOAD_FILE_PATH[1]}"
  manual_download="${DOWNLOAD_URL}${UPLOAD_FILE_PATH[2]}"


  # These links will expire as the artifacts are by default only kept for a limited time
  # but as these are nightlies this should be fine.
  # When it is possible to lik to the generic packages, we create it would be best to change to links to point there
  # as the job id would then also be irrelevant <https://gitlab.com/gitlab-org/gitlab/-/issues/289848>
  BUNDLE_DOWNLOAD_URL="https://git.ps.informatik.uni-kiel.de/curry/pakcs/-/jobs/${BUNDLE_JOB_ID}/artifacts/raw"
  MANUAL_DOWNLOAD_URL="https://git.ps.informatik.uni-kiel.de/curry/pakcs/-/jobs/${MANUAL_JOB_ID}/artifacts/raw"

  # these variables are used by update_version.sh when doing a nightly
  # for the download links on curry-lang.org
  # these urls are meaningless for normal releases as the gitlab release links should be used as they don't expire
  nightly_src_download="${BUNDLE_DOWNLOAD_URL}/${LOCAL_FILE_NAMES[0]}"
  nightly_arch_download="${BUNDLE_DOWNLOAD_URL}/${LOCAL_FILE_NAMES[1]}"
  nightly_manual_download="${MANUAL_DOWNLOAD_URL}/${LOCAL_FILE_NAMES[2]}"

}
