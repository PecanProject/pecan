#!/bin/bash

set -e

DEBUG=${DEBUG:-""}

IMAGES="web base executor model data"
MODELS="sipnet"

# some git variables
PECAN_GIT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
PECAN_GIT_CHECKSUM="$(git log --pretty=format:%H -1)"
PECAN_GIT_DATE="$(git log --pretty=format:%ad -1)"

# get version number
VERSION="$(awk '/Version:/ { print $2 }' base/all/DESCRIPTION)"

# not building dependencies image, following command will build this
echo "# docker image for dependencies is not build by default."
echo "# this image takes a long time to build."
echo "docker build --tag pecan/depends:latest --file docker/Dockerfile.depends ."

# build images
for i in ${IMAGES}; do
    rm -f build.${i}.log
    ${DEBUG} docker build \
        --build-arg PECAN_VERSION="${VERSION}" \
        --build-arg PECAN_GIT_BRANCH="${PECAN_GIT_BRANCH}" \
        --build-arg PECAN_GIT_CHECKSUM="${PECAN_GIT_CHECKSUM}" \
        --build-arg PECAN_GIT_DATE="${PECAN_GIT_DATE}" \
        --tag pecan/${i}:latest \
        --file docker/Dockerfile.${i} . | tee build.${i}.log
    echo $?
done
for i in ${MODELS}; do
    rm -f build.${i}.log
    ${DEBUG} docker build \
        --build-arg PECAN_VERSION="${VERSION}" \
        --build-arg PECAN_GIT_BRANCH="${PECAN_GIT_BRANCH}" \
        --build-arg PECAN_GIT_CHECKSUM="${PECAN_GIT_CHECKSUM}" \
        --build-arg PECAN_GIT_DATE="${PECAN_GIT_DATE}" \
        --tag pecan/model-${i}:latest \
        --file docker/Dockerfile.${i} . | tee build.${i}.log
done
