#!/bin/bash

set -e
cd $(dirname $0)

DEBUG=${DEBUG:-""}
DEPEND=${DEPEND:-""}

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
if [ "${DEPEND}" == "" ]; then
    echo "# docker image for dependencies is not build by default."
    echo "# this image takes a long time to build. To build this"
    echo "# image run DEPEND=1 $0"
else
    ${DEBUG} docker build \
        --tag pecan/depends:latest \
        --file docker/base/Dockerfile.depends . | tee docker/base/build.depends.log
fi
echo ""

# build images in this specific order. Images are tagged with latest so other
# docker images build in this script will use that specifc build.
for i in base executor web data docs; do
    rm -f build.${i}.log
    ${DEBUG} docker build \
        --build-arg PECAN_VERSION="${VERSION}" \
        --build-arg PECAN_GIT_BRANCH="${PECAN_GIT_BRANCH}" \
        --build-arg PECAN_GIT_CHECKSUM="${PECAN_GIT_CHECKSUM}" \
        --build-arg PECAN_GIT_DATE="${PECAN_GIT_DATE}" \
        --tag pecan/${i}:latest \
        --file docker/base/Dockerfile.${i} . | tee docker/base/build.${i}.log
done

# --------------------------------------------------------------------------------
# MODEL BUILD SECTION
# --------------------------------------------------------------------------------

# build sipnet 136
rm -f build.sipnet.log
${DEBUG} docker build \
    --build-arg MODEL_VERSION="136" \
    --tag pecan/model-sipnet-136:latest \
    --file docker/models/Dockerfile.sipnet . | tee docker/models/build.sipnet.log

# build ed2 latest
rm -f build.ed2.log
${DEBUG} docker build \
    --build-arg MODEL_VERSION="git" \
    --tag pecan/model-ed2-git:latest \
    --file docker/models/Dockerfile.ed2 . | tee docker/models/build.ed2.log
