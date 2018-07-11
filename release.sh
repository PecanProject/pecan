#!/bin/bash

set -e

DEBUG=${DEBUG:-""}
SERVER=${SERVER:-""}

IMAGES="web depends base executor model data"
MODELS="sipnet"

# get version number
VERSION="$(awk '/Version:/ { print $2 }' base/all/DESCRIPTION)"

# build images first
#DEBUG=${DEBUG} ./docker.sh

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
        ${DEBUG} docker push ${SERVER}pecan/${i}:${v}
    done
done
for i in ${MODELS}; do
    for v in ${TAGS}; do
        if [ "$v" != "latest" -o "$SERVER" != "" ]; then
            ${DEBUG} docker tag pecan/model-${i}:latest ${SERVER}pecan/model-${i}:${v}
        fi
        ${DEBUG} docker push ${SERVER}pecan/model-${i}:${v}
    done
done
