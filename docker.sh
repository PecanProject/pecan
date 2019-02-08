#!/bin/bash

# exit script if error occurs (-e) in any command of pipeline (pipefail)
set -o pipefail
set -e

cd $(dirname $0)

DEBUG=${DEBUG:-""}
DEPEND=${DEPEND:-"nothing"}

# --------------------------------------------------------------------------------
# PECAN BUILD SECTION
# --------------------------------------------------------------------------------

# some git variables
PECAN_GIT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
PECAN_GIT_CHECKSUM="$(git log --pretty=format:%H -1)"
PECAN_GIT_DATE="$(git log --pretty=format:%ad -1)"

# get version number
VERSION=${VERSION:-"$(awk '/Version:/ { print $2 }' base/all/DESCRIPTION)"}

echo "Building PEcAn"
echo " PECAN_VERSION      : ${VERSION}"
echo " PECAN_GIT_BRANCH   : ${PECAN_GIT_BRANCH}"
echo " PECAN_GIT_DATE     : ${PECAN_GIT_DATE}"
echo " PECAN_GIT_CHECKSUM : ${PECAN_GIT_CHECKSUM}"
echo ""

# not building dependencies image, following command will build this
if [ "${DEPEND}" == "build" ]; then
    echo "# To just pull latest/develop version (default) run"
    echo "# DEPEND=pull $0"
    ${DEBUG} docker build --tag pecan/depends:latest docker/depends
elif [ "${DEPEND}" == "pull" ]; then
    echo "# docker image for dependencies is not build by default."
    echo "# this image takes a long time to build. To build this"
    echo "# image run DEPEND=build $0"
    if [ "${PECAN_GIT_BRANCH}" != "master" ]; then
      echo "# this will pull develop of base image and tag as latest"
      echo "# To disable run DEPEND=nothing $0"
      ${DEBUG} docker pull pecan/depends:develop
      ${DEBUG} docker tag pecan/depends:develop pecan/depends:latest
    else
      echo "# this will pull latest of base image"
      echo "# To disable run DEPEND=nothing $0"
      ${DEBUG} docker pull pecan/depends:latest
    fi
else
    echo "# docker image for dependencies is not build by default."
    echo "# this image takes a long time to build. To build this"
    echo "# image run DEPEND=build $0"
    echo "# To just pull latest/develop version run"
    echo "# DEPEND=pull $0"
fi
echo ""

# require all of PEcAn to build
for x in base web docs; do
    ${DEBUG} docker build --tag pecan/$x:latest --file docker/$x/Dockerfile \
        --build-arg FROM_IMAGE="${FROM_IMAGE:-depends}" \
        --build-arg IMAGE_VERSION="${IMAGE_VERSION:-latest}" \
        --build-arg PECAN_VERSION="${VERSION}" \
        --build-arg PECAN_GIT_BRANCH="${PECAN_GIT_BRANCH}" \
        --build-arg PECAN_GIT_CHECKSUM="${PECAN_GIT_CHECKSUM}" \
        --build-arg PECAN_GIT_DATE="${PECAN_GIT_DATE}" \
        .
done

# all files in subfolder
for x in models executor data thredds monitor; do
    ${DEBUG} docker build --tag pecan/$x:latest docker/$x
done

# --------------------------------------------------------------------------------
# MODEL BUILD SECTION
# --------------------------------------------------------------------------------

# build sipnet
for version in 136; do
    ${DEBUG} docker build \
        --build-arg MODEL_VERSION="${version}" \
        --tag pecan/model-sipnet-${version}:latest \
        --file models/sipnet/Dockerfile .
done

# build ed2
for version in git; do
    ${DEBUG} docker build \
        --build-arg MODEL_VERSION="${version}" \
        --tag pecan/model-ed2-${version}:latest \
        --file models/ed/Dockerfile .
done

# build maespa
for version in git; do
    ${DEBUG} docker build \
        --build-arg MODEL_VERSION="${version}" \
        --tag pecan/model-maespa-${version}:latest \
        --file models/maespa/Dockerfile .
done
