CI Readme
=========

## Files

```text
.                                -- repo root
|
+-- .gitlab-ci.yml               -- Gitlab CI file, the root of all CI
+-- .tarignore                   -- Files ignored for distribution (`make dist`)
+-- Makefile                     -- The Projects Makefile invoked by CI
|
+-- ci
|   |
|   +-- before_make.sh           -- run before most CI jobs to setup the basic environment
|   +-- build_docker_image.sh    -- script to build the Dockerfile-swi image and tag it as pakcs-swi-ci
|   +-- check_tag_version.sh     -- for version tags check that the tag matches the build version
|   +-- cypm_helper.sh           -- script with helper function for interacting with cypm
|   +-- Dockerfile-swi           -- Dockerfile for the Docker Image used by most CI jobs
|   +-- gitlab_release.sh        -- used to create the gitlab (nightly) releases
|   +-- make_download.sh         -- script run to test the website download
|   +-- README.md                -- this Readme explaining the CI setup
|   +-- release_helper.sh        -- script with helper function to setup the release environment
|   +-- section_helper.sh        -- helper script for adding custom sections to the gitlab ci log
|   +-- update_version.sh        -- script to update the latest release/nightly referenced on curry-lang.org
|   +-- upload_release.sh        -- script to create a new generic package for releases and nightlies
```

## Configuration

### Repo Source
Most configuration is configured in one of the files listed above.
Things that one may want to override is where possible configured via variables in .gitlab-cyí.yml

#### .gitlab-ci.yml Docker
The default docker image used is defined by the top-level `image` entry,
as of writing this is `caups/pakcs-swi-ci:24.04` which was build from
the Dockerfile `Dockerfile-swi` and uploaded as follows (see also
script `build_docker_image.sh`):

    > docker build --build-arg UBUNTU_VERSION=24.04 -t caups/pakcs-swi-ci:24.04 -f Dockerfile-swi .
    > docker login --username ....
    > docker push caups/pakcs-swi-ci:24.04

The `tag_release` job uses a different image as it needs access to the gitlab release-cli and the template
job `.test_download` uses a different images as at least one of the derived jobs requires an older ghc.


All jobs will run on any available runner that is tagged with the `docker2` tag,
where possible they will run concurrently.
As the `link_nightly` and `link_release` jobs can interfere with each other, 
they are configured to not run concurrently by sharing a `resource_group` 

#### .gitlab-ci.yml Variables
- `DONWLOAD_VERSION`\: Used by `make_download.sh` to download the distribution
  from `http://www.informatik.uni-kiel.de/~pakcs/download/packs-${DOWNLOAD_VERSION}-<src|amd64-Linux>.tar.gz`
  for the nightly tests
- `CPM_VERSION`\: Used by `before_make.sh` to configure the cypm directories, 
   can't ask cypm as it might not be available at this point, and we don't want it to write to wrong places
- `CURRY_LANG_USER`\: The username for the Access Token (see Gitlab Section) [Username Doc](https://docs.gitlab.com/ee/user/project/settings/project_access_tokens.html)
- `CURRY_LANG_EMAIL` \: The email address for the Access Token, Username without first `_` @ domain, domain is `example.com`
   unless configured by a Gitlab Admin, no clue how I found that out 
- `CURRY_LANG_PROJECT`\: The Repo ID for the curry-lang.org repo, assumed to be in the same gitlab instance
- `CURRY_LANG_BRANCH` \: The branch to update for releases/nightlies in the curry-lang.org repo
- `CURRY_CHECK_VERSION`, `CURRY_DOC_VERSION`, `MARKDOWN_VERSION`\: the version of the corresponding package to install using cypm 

##### `gitlab-ci.yml` job `run_download_make` Variables
- `CURRY_CHECK_VERSION` override as this may need a different version than the other jobs

##### Values that usually don't need to be changed
`LANG` and `LC_ALL` cause problems when not set or set to incompatible values, therefore they are
set to known working values.

The variables `STACK_ROOT`, `CABAL_DIR`, `BIN_INSTALL_DIR` are there to move those folders to a place where they can be cached,
it should not be necessary to override them.

As this project contains submodules those are checkedout recursively by instructing Gitlab via
`GIT_SUBMODULE_STRATEGY`, this should not be changed as long as this project uses submodules.

#### `update_version.sh`
Here the content of the
`./data/versions/packs/{latest,latest-nightly,v${VERSION}}.version`
including the download link bases.
Also, the commit messages to curry-lang.org are defined here.

#### `release_helper.sh`
Defined the names used for tags and the generic packages.
Also, defined the mapping of paths to filenames for the files that are
added to a release.  `LOCAL_FILE_NAME` contains the name of the file
on disc relative to the `CI_PROJECT_DIR`.  `UPLOAD_FILE_NAMES`
contains the corresponding name for the file upload.

#### `make_download.sh`
Has a hard coded download URL

#### `gitlab_release.sh`
Contains the release name, description and tag

#### `before_make.sh`
Configures some Paths for cypm to have them under the `CI_PROJECT_DIR`
so that we can cache them.

### Gitlab
All variables defined in the .gitlab-ci script can be overridden
with Project CI Variables, Schedule Variables or Variables in Manual runs.

#### Access Token
The access token to commit to the curry-lang.org repo **MUST NOT** be configured
as part of the repo source,  but in one of the positions mentioned above
for override variables.
Usually this should be set in `pakcs >> Setting >> CI/CD Settings >> Variables`
and there `CURRY_LANG_ORG_ACCESS_TOKEN` should contain the access token `Masked`
should be set, as we don't use protected branches `Protected` should **NOT**
be set. To get an access token for the curry-lang.org repo go to
`curry-lang.org >> Settings >> Access Tokens` and create an access token
with `Developer` role and at least `write_repository` permission,
though more permissions may be required.
When changing Access Tokens change the variables `CURRY_LANG_USER` and
`CURRY_LANG_EMAIL` accordingly.

#### Schedules
##### Nightly Release Build
For the Schedule Variables `BUILD_NIGHTLY` should be set to `yes`
##### Nightly Download Test
For the Schedule Variables `TEST_DOWNLOAD` should be set tp `yes`


## CI Runs
### Commit
On commit a Pipeline runs unless the commit message contains `[skip ci]`.
For commit only the jobs `run_make` and `run_test` are executed, to keep the CI time short.

### Tag
On tag a Pipeline runs unless the commit message contains `[skip ci]`!
For tags the jobs: 
- `run_make`
- `run_test`,`check_version`
- `make_manual`
- `bundle_tar`
- `upload_release`, `upload_page_tar`
- `tag_release`
- `link_release`
- `downstream`
are (potentially) executed.

For tags that match the regex `/^v\d*\.\d*\.\d*$/i` the ci will check that it matches packs version number
and create a new release in gitlab and on curry-lang.org. The links on curry-lang.org will point to 
the assets links of the gitlab release which in turn points to the generic package created by the ci.
The links used by the gitlab release to point at the generic package are unfortunately only accessible while 
logged in to gitlab, accessible links are available, but not easily generated from CI, 
changing the links of the gitlab release manually it is therefore recommended.

To change the links edit the release under `packs >> Releases > Edit this release`,
the link urls should currently be of the form `https://git.ps.informatik.uni-kiel.de/api/v4/projects/88/packages/generic/<package>/<version>/<filename>`.
The accessible link will be of the form `https://git.ps.informatik.uni-kiel.de/curry/pakcs/-/package_files/<id>/download`,
to find the correct link navigate in another tab/window to `packs >> Package Registry > <package>`,
this most likely already opens the correct version, if not go further to `Other versions >> <version>`.
Now should see the generic package `<package>` in the version `<version>`, now you can hover over each filename corresponding to `<filename>` and copy the link into the corresponding URL Field of the release.

Changing the URLs can cause the Permalinks to break, to fix this run the `ci/fix_asset_links.sh` script.

### Schedule
#### Nightly Test
Only the jobs `test_download_src` and `test_download_amd64_linux` are run,
to test the distribution downloads.
The pipeline is run with Variable `TEST_DOWNLOAD` set to `yes`

#### Nightly Release Build
For nightly release build pipeline the jobs: 
- `run_make`
- `run_test`
- `make_manual`
- `bundle_tar`
- `upload_nightly`
- `link_nightly`
- `downstream`
are executed.
The pipeline is run with Variable `BUILD_NIGHTLY` set to `yes`

### Manual Builds
Manual pipelines are started from the Gitlab Web Interface under `packs >> CI/CD > Pipelines >> Run Pipeline`,
than the branch/tag that should be run can be selected and override Variables can be set.

#### Nightly Release Build
When a Nightly Build should be retried one can also just retry the existing nightly run
or manually trigger the scheduled pipeline under `packs >> CI/CD > Schedules >> Test Nightly > Play`.
To Manually Test the nightly release, create a manual Pipeline with Variable `BUILD_NIGHTLY` set to `yes`,
usually the `CURRY_LANG_BRANCH` variable should also be overridden,
note that the corresponding branch needs to exist 
in the curry-lang.org repo before the pipeline is started.

#### Nightly Download Test
Manually trigger the scheduled pipeline under `packs >> CI/CD > Schedules >> Test Download > Play`

#### Release Build
To create a release, create the tag `v<packs-version>` with `<packs-version>` replaced by the version that the release is for.
To test most of the release cycle, create a Manual pipline and set the Variable `TEST_RELEASE` to `yes` or create a tag 
that does not potentially match a past or futur version.

For more information on the Pipeline Job se above in the `CI Runs` Section under `Tag`

## CI Job Dependencies
Gitlab requires that job dependencies are acyclic (reasonable) and that jobs may only depend on jobs of prior stages. 
While keeping this in mind, the jobs are separated further in semantic stages.

```text

make            |                       run_make
                |                          |
                |  +-------------+---------+------------+------------+       
                |  |             |         |            |            |
test            |  |             |         |         run_test   check_version  test_download_src  test_download_amd64_linux   
                |  |             |         |                         
                |  |             |         +----------+-------------------+
                |  |             v         v          v                   v
                |  |             |         |          |                   |
doc             |  |             |     make_manual    |                   |
                |  |             |         |          |                   |
                |  |             +----<----+---->-----|-------------------|-+
package_release |  |       bundle_tar      v          |                   | |
                |  |             |         |          |                   | |
                |  |             |         |          |                   | |
                |  |   +---------+---->----+----------+                   | |
                |  |   |                   v                              | |
                |  |   |         +---------+----+--------------+          | |
                |  |   |         |              |              |          | |
upload_release  |  |   |   upload_release upload_nightly upload_page_tar  | |
                |  |   |         |                                        | |
                |  |   |         +-------<------+-------------------------+ |
                |  |   |         |              |                           |
tag_release     |  |   v      tag_release       v                           |
                |  |   |         |              |                           |
                |  |   +---------|--------------+---------------------------+
                |  |             |              |
                |  +-------------+              |
curry-lang      |            link_release   link_nightly
                |                |              |
                |                +--------------+               
                |                |
curry-lang-ci   |           curry-lang-ci
                |
```

`link_release`, `tag_release` and `upload_release` further depend on `check_version`, this is 
used to make sure that releases are only done if the check succeeds. There is no further artifact dependency.

## Makefile Interaction

All CI invocations of make should set `CI_BUILD=yes`,
this should cause most if not all warnings to be errors.
For example fail if the tool to build the documentation or run the test is missing,
rather than warn.
Also, this flag should cause more verbose output, for better debugging of traces for failed CI runs.

Files/Folders that should not be packed for distribution (`make dist`) 
should be listed in the `.tarignore`, files that only be ignored for some distribution artifacts 
are listed directly in the Makefile
