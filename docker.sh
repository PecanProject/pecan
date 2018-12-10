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
    ${DEBUG} docker build \
        --tag pecan/depends:latest \
        --file docker/base/Dockerfile.depends . | tee docker/base/build.depends.log
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

# build images in this specific order. Images are tagged with latest so other
# docker images build in this script will use that specifc build.
for i in base executor web data thredds docs; do
    rm -f build.${i}.log
    ${DEBUG} docker build \
        --build-arg FROM_IMAGE="${FROM_IMAGE:-depends}" \
        --build-arg IMAGE_VERSION="${IMAGE_VERSION:-latest}" \
        --build-arg PECAN_VERSION="${VERSION}" \
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
