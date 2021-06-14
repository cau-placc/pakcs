GHC_VERSION=8.8.3

docker build \
    --build-arg GHC_VERSION=${GHC_VERSION} \
    -t pakcs-swi-ci:${GHC_VERSION} \
    -f ./Dockerfile-swi .
