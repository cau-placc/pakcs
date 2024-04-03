GHC_VERSION=9.2.5

docker build \
    --build-arg GHC_VERSION=${GHC_VERSION} \
    -t pakcs-swi-ci:${GHC_VERSION} \
    -f ./Dockerfile-swi .
