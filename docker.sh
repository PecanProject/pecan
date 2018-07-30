#!/bin/bash

set -e

DEBUG=${DEBUG:-""}
SERVER=${SERVER:-"hub.ncsa.illinois.edu/"}

IMAGES="web pecan executor data"
MODELS="sipnet"

# some git variables
PECAN_GIT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
PECAN_GIT_CHECKSUM="$(git log --pretty=format:%H -1)"
PECAN_GIT_DATE="$(git log --pretty=format:%ad -1)"

# get version number
VERSION="$(awk '/Version:/ { print $2 }' base/all/DESCRIPTION)"


# build images
for i in ${IMAGES}; do
    ${DEBUG} docker build \
        --build-arg PECAN_VERSION="${VERSION}" \
        --build-arg PECAN_GIT_BRANCH="${PECAN_GIT_BRANCH}" \
        --build-arg PECAN_GIT_CHECKSUM="${PECAN_GIT_CHECKSUM}" \
        --build-arg PECAN_GIT_DATE="${PECAN_GIT_DATE}" \
        --tag pecan/${i}:latest \
        --file docker/Dockerfile.${i} .
done
for i in ${MODELS}; do
    ${DEBUG} docker build \
        --build-arg PECAN_VERSION="${VERSION}" \
        --build-arg PECAN_GIT_BRANCH="${PECAN_GIT_BRANCH}" \
        --build-arg PECAN_GIT_CHECKSUM="${PECAN_GIT_CHECKSUM}" \
        --build-arg PECAN_GIT_DATE="${PECAN_GIT_DATE}" \
        --tag pecan/model-${i}:latest \
        --file docker/Dockerfile.${i} .
done

# check branch and set version
if [ "${PECAN_GIT_BRANCH}" = "master" ]; then
    TAGS="${VERSION} latest"
elif [ "${PECAN_GIT_BRANCH}" = "develop" ]; then
    TAGS="develop"
else
    TAGS="develop"
    #exit 0
fi

for i in ${IMAGES}; do
    for v in ${TAGS}; do
        if [ "$v" != "latest" -o "$SERVER" != "" ]; then
            ${DEBUG} docker tag pecan/${i}:latest ${SERVER}pecan/${i}:${v}
        fi
        #${DEBUG} docker push ${SERVER}pecan/${i}:${v}
    done
done
for i in ${MODELS}; do
    for v in ${TAGS}; do
        if [ "$v" != "latest" -o "$SERVER" != "" ]; then
            ${DEBUG} docker tag pecan/model-${i}:latest ${SERVER}pecan/model-${i}:${v}
        fi
        #${DEBUG} docker push ${SERVER}pecan/model-${i}:${v}
    done
done
