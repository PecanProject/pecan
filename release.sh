#!/bin/bash

set -e

DEBUG=${DEBUG:-""}
SERVER=${SERVER:-""}
DEPEND=${DEPEND:-""}
IMAGE_VERSION=${IMAGE_VERSION:-"latest"}

# get version number
VERSION=${VERSION:-"$(awk '/Version:/ { print $2 }' base/all/DESCRIPTION)"}

# build images first
VERSION=${VERSION} DEBUG=${DEBUG} DEPEND=${DEPEND} IMAGE_VERSION=${IMAGE_VERSION} ./docker.sh

# check branch and set version
PECAN_GIT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
if [ "${PECAN_GIT_BRANCH}" = "master" ]; then
    TAGS="latest"
    TMPVERSION="${VERSION}"
    OLDVERSION=""
    while [ "$OLDVERSION" != "$TMPVERSION" ]; do
       TAGS="${TAGS} ${TMPVERSION}"
       OLDVERSION="${TMPVERSION}"
       TMPVERSION=$(echo ${OLDVERSION} | sed 's/\.[0-9]*$//')
    done
elif [ "${PECAN_GIT_BRANCH}" = "develop" ]; then
    TAGS="develop"
fi
if [ "${TAGS}" == "" ]; then
    echo "No tags specified, not pushing to server."
    exit 1
fi

# --------------------------------------------------------------------------------
# PUSH NEW IMAGES
# --------------------------------------------------------------------------------

if [ "${DEPEND}" == "build" ]; then
    OLDEST="pecan/depends"
else
    OLDEST="pecan/base"
fi

# push all images
for image in ${OLDEST}  $( docker image ls pecan/*:${IMAGE_VERSION} --filter "since=${OLDEST}:${IMAGE_VERSION}" --format "{{ .Repository }}" ); do
    for v in ${TAGS}; do
        if [ "$v" != "${IMAGE_VERSION}" -o "$SERVER" != "" ]; then
            ${DEBUG} docker tag ${image}:${IMAGE_VERSION} ${SERVER}${image}:${v}
        fi
        ${DEBUG} docker push ${SERVER}${image}:${v}
    done
done
