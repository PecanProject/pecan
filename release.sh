#!/bin/bash

set -e

DEBUG=${DEBUG:-""}
SERVER=${SERVER:-""}
DEPEND=${DEPEND:-""}

# get version number
VERSION=${VERSION:-"$(awk '/Version:/ { print $2 }' base/all/DESCRIPTION)"}

# build images first
VERSION=${VERSION} DEBUG=${DEBUG} DEPEND=${DEPEND} ./docker.sh

# check branch and set version
if [ "${PECAN_GIT_BRANCH}" = "master" ]; then
    TAGS="${VERSION} latest"
elif [ "${PECAN_GIT_BRANCH}" = "develop" ]; then
    TAGS="develop"
fi
if [ "${TAGS}" == "" ]; then
    echo "No tags specified, not pushing to server."
    exit 1
fi

# push pecan images
for i in depends base executor web data; do
    for v in ${TAGS}; do
        if [ "$v" != "latest" -o "$SERVER" != "" ]; then
            ${DEBUG} docker tag pecan/${i}:latest ${SERVER}pecan/${i}:${v}
        fi
        ${DEBUG} docker push ${SERVER}pecan/${i}:${v}
    done
done

# push model images
for i in ls; do
    for v in ${TAGS}; do
        if [ "$v" != "latest" -o "$SERVER" != "" ]; then
            ${DEBUG} docker tag pecan/${i}:latest ${SERVER}pecan/${i}:${v}
        fi
        ${DEBUG} docker push ${SERVER}pecan/${i}:${v}
    done
done

for i in model-sipnet-136 model-ed2-git; do
    for v in ${TAGS}; do
        if [ "$v" != "latest" -o "$SERVER" != "" ]; then
            ${DEBUG} docker tag pecan/${i}:latest ${SERVER}pecan/${i}:${v}
        fi
        ${DEBUG} docker push ${SERVER}pecan/${i}:${v}
    done
done
