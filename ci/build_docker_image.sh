UBUNTU_VERSION=22.04

docker build \
    --build-arg UBUNTU_VERSION=${UBUNTU_VERSION} \
    -t pakcs-swi-ci:${UBUNTU_VERSION} \
    -f ./Dockerfile-swi .
