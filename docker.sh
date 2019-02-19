#!/bin/bash

# exit script if error occurs (-e) in any command of pipeline (pipefail)
set -o pipefail
set -e

cd $(dirname $0)

# Can set the following variables
DEBUG=${DEBUG:-""}
DEPEND=${DEPEND:-""}
R_VERSION=${R_VERSION:-"3.5"}

# --------------------------------------------------------------------------------
# PECAN BUILD SECTION
# --------------------------------------------------------------------------------

# some git variables
PECAN_GIT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
PECAN_GIT_CHECKSUM="$(git log --pretty=format:%H -1)"
PECAN_GIT_DATE="$(git log --pretty=format:%ad -1)"

# get version number
VERSION=${VERSION:-"$(awk '/Version:/ { print $2 }' base/all/DESCRIPTION)"}

# check for branch and set IMAGE_VERSION
if [ "${PECAN_GIT_BRANCH}" == "master" ]; then
    IMAGE_VERSION=${IMAGE_VERSION:-"latest"}
elif [ "${PECAN_GIT_BRANCH}" == "develop" ]; then
    IMAGE_VERSION=${IMAGE_VERSION:-"develop"}
else
    IMAGE_VERSION=${IMAGE_VERSION:-"testing"}
fi

# information for user before we build things
echo "# ----------------------------------------------------------------------"
echo "# Building PEcAn"
echo "#  PECAN_VERSION      : ${VERSION}"
echo "#  PECAN_GIT_BRANCH   : ${PECAN_GIT_BRANCH}"
echo "#  PECAN_GIT_DATE     : ${PECAN_GIT_DATE}"
echo "#  PECAN_GIT_CHECKSUM : ${PECAN_GIT_CHECKSUM}"
echo "#  IMAGE_VERSION      : ${IMAGE_VERSION}"
echo "#"
echo "# Created images will be tagged with '${IMAGE_VERSION}'. If you want to"
echo "# test this build you can use:"
echo "# PECAN_VERSION='${IMAGE_VERSION}' docker-compose up"
echo "#"
echo "# The docker image for dependencies takes a long time to build. You"
echo "# can use a prebuild version (default) or force a new versin to be"
echo "# build locally using: DEPEND=build $0"
echo "# ----------------------------------------------------------------------"

# not building dependencies image, following command will build this
if [ "${DEPEND}" == "build" ]; then
    ${DEBUG} docker build \
        --build-arg R_VERSION=${R_VERSION} \
        --tag pecan/depends:${IMAGE_VERSION} \
        docker/depends
else
    if [ "$( docker image ls -q pecan/depends:${IMAGE_VERSION} )" == "" ]; then
        if [ "${PECAN_GIT_BRANCH}" != "master" ]; then
            if [ "$( docker image ls -q pecan/depends:develop )" == "" ]; then
                ${DEBUG} docker pull pecan/depends:develop
            fi
            if [ "${IMAGE_VERSION}" != "develop" ]; then
                ${DEBUG} docker tag pecan/depends:develop pecan/depends:${IMAGE_VERSION}
            fi
        else
            if [ "$( docker image ls -q pecan/depends:latest )" == "" ]; then
                ${DEBUG} docker pull pecan/depends:latest
            fi
            if [ "${IMAGE_VERSION}" != "latest" ]; then
                ${DEBUG} docker tag pecan/depends:latest pecan/depends:${IMAGE_VERSION}
            fi
        fi
    fi
fi
echo ""

# require all of PEcAn to build
for x in base web docs; do
    ${DEBUG} docker build \
        --tag pecan/$x:${IMAGE_VERSION} \
        --build-arg FROM_IMAGE="${FROM_IMAGE:-depends}" \
        --build-arg IMAGE_VERSION="${IMAGE_VERSION}" \
        --build-arg PECAN_VERSION="${VERSION}" \
        --build-arg PECAN_GIT_BRANCH="${PECAN_GIT_BRANCH}" \
        --build-arg PECAN_GIT_CHECKSUM="${PECAN_GIT_CHECKSUM}" \
        --build-arg PECAN_GIT_DATE="${PECAN_GIT_DATE}" \
        --file docker/$x/Dockerfile \
        .
done

# all files in subfolder
for x in models executor data thredds monitor; do
    ${DEBUG} docker build \
        --tag pecan/$x:${IMAGE_VERSION} \
        --build-arg IMAGE_VERSION="${IMAGE_VERSION}" \
        docker/$x
done

# --------------------------------------------------------------------------------
# MODEL BUILD SECTION
# --------------------------------------------------------------------------------

# build sipnet
for version in 136; do
    ${DEBUG} docker build \
        --tag pecan/model-sipnet-${version}:${IMAGE_VERSION} \
        --build-arg MODEL_VERSION="${version}" \
        --build-arg IMAGE_VERSION="${IMAGE_VERSION}" \
        models/sipnet
done

# build ed2
for version in git; do
    ${DEBUG} docker build \
        --tag pecan/model-ed2-${version}:${IMAGE_VERSION} \
        --build-arg MODEL_VERSION="${version}" \
        --build-arg IMAGE_VERSION="${IMAGE_VERSION}" \
        models/ed
done

# build maespa
for version in git; do
    ${DEBUG} docker build \
        --tag pecan/model-maespa-${version}:${IMAGE_VERSION} \
        --build-arg MODEL_VERSION="${version}" \
        --build-arg IMAGE_VERSION="${IMAGE_VERSION}" \
        models/maespa
done
