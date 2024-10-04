UBUNTU_VERSION=24.04

docker build \
    --build-arg UBUNTU_VERSION=${UBUNTU_VERSION} \
    -t pakcs-swi-ci:${UBUNTU_VERSION} \
    -f ./Dockerfile-swi .
