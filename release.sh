#!/bin/bash

# Use release.sh -h for instructions on how to use this script.

set -e

DEBUG=${DEBUG:-""}
SERVER=${SERVER:-""}
DEPEND=${DEPEND:-""}

# get version number
VERSION=${VERSION:-"$(awk '/Version:/ { print $2 }' base/all/DESCRIPTION)"}

# check for branch and set IMAGE_VERSION
PECAN_GIT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
if [ "${PECAN_GIT_BRANCH}" == "main" ]; then
    IMAGE_VERSION=${IMAGE_VERSION:-"latest"}
elif [ "${PECAN_GIT_BRANCH}" == "develop" ]; then
    IMAGE_VERSION=${IMAGE_VERSION:-"develop"}
else
    IMAGE_VERSION=${IMAGE_VERSION:-"testing"}
fi

# check branch and set tags
if [ "${TAGS}" == "" ]; then
    if [ "${PECAN_GIT_BRANCH}" = "main" ]; then
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
fi

# read command line options, overriding any environment variables
while getopts dfhi:r:s:t: opt; do
    case $opt in
    d)
        DEBUG="echo"
        ;;
    f)
        DEPEND="build"
        ;;
    h)
        cat << EOF
$0 [-dfh] [-i <IMAGE_VERSION>] [-r <R VERSION] [-s <SERVER>] [-t <TAGS>]

This script is used to create docker images and push these images to 
a docker repository. This script behind the scenes will use the
docker.sh script to create the actual images. And will push them to
the docker repo.

To specify the tags to push, use the TAGS environment option, or use
the -t option. If this is the main branch it will tag all versions
specified in the base/all/DESCRIPTION file as well as latest. For
example for version 1.7.1 it would tag latest, 1, 1.7 and 1.7.1. In 
case of the develop branch it will push the image with the tag
develop.

Following is the help text from docker.sh.

EOF
        ./docker.sh -h
        exit 1
        ;;
    i)
        IMAGE_VERSION="$OPTARG"
        ;;
    n)
        DEBUG="echo"
        ;;
    r)
        R_VERSION="$OPTARG"
        DEPEND="build"
        ;;
    s)
        SERVER="$OPTARG"
        ;;
    t)
        TAGS="$OPTARG"
        ;;
    esac
done

if [ "${TAGS}" == "" ]; then
    echo "No tags specified, not pushing to server."
    exit 1
fi

# build images first
VERSION=${VERSION} DEBUG=${DEBUG} DEPEND=${DEPEND} R_VERSION=${R_VERSION} IMAGE_VERSION=${IMAGE_VERSION} ./docker.sh

# --------------------------------------------------------------------------------
# PUSH NEW IMAGES
# --------------------------------------------------------------------------------

echo ""
echo "# ----------------------------------------------------------------------"
echo "# Pushing images"
echo "# ----------------------------------------------------------------------"

if [ "${DEPEND}" == "build" ]; then
    OLDEST="pecan/depends"
else
    OLDEST="pecan/base"
fi

# push all images
for image in pecan/check ${OLDEST} $( docker image ls pecan/*:${IMAGE_VERSION} --filter "since=${OLDEST}:${IMAGE_VERSION}" --format "{{ .Repository }}" ); do
    for v in ${TAGS}; do
        if [ "$v" != "${IMAGE_VERSION}" -o "$SERVER" != "" ]; then
            ${DEBUG} docker tag ${image}:${IMAGE_VERSION} ${SERVER}${image}:${v}
        fi
        ${DEBUG} docker push ${SERVER}${image}:${v}
    done
done
