#!/bin/bash

# Use docker.sh -h for instructions on how to use this script.

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

# read command line options, overriding any environment variables
while getopts dfhi:r: opt; do
    case $opt in
    d)
        DEBUG="echo"
        ;;
    f)
        DEPEND="build"
        ;;
    h)
        cat << EOF
$0 [-dfh] [-i <IMAGE_VERSION>] [-r <R VERSION]

The following script can be used to create all docker images. Without any
options this will build all images and tag them based on the branch you
are on. The master branch will be tagged with latest, develop branch will
be tagged with develop and any other branch will be tagged with testing.
Most options can be set using either an environment variable or using
command line options. If both are set, the command line options will
override the environmnet variables.

You can change the docker image tag using the environment variable
IMAGE_VERSION or the option -i.

To run the script in debug mode without actually building any images you
can use the environment variable DEBUG or option -d.

By default the docker.sh process will try and use a prebuild dependency
image since this image takes a long time to build. To force this image
to be build use the DEPEND="build" environment flag, or use option -f.

To set the version used of R when building the dependency image use
the environment option R_VERSION (as well as DEPEND). You can also use
the -r option which will make sure the dependency image is build.

You can use the FROM_IMAGE environment variable to also specify what
image should be used when building the base image. You can for example
use the previous base image which will speed up the compile process of
PEcAn.
EOF
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
    esac
done

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
        --pull \
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
for x in models executor data thredds monitor rstudio-nginx check; do
    ${DEBUG} docker build \
        --tag pecan/$x:${IMAGE_VERSION} \
        --build-arg IMAGE_VERSION="${IMAGE_VERSION}" \
        docker/$x
done

# shiny apps
for x in dbsync; do
    ${DEBUG} docker build \
        --tag pecan/shiny-$x:${IMAGE_VERSION} \
        --build-arg IMAGE_VERSION="${IMAGE_VERSION}" \
        shiny/$x
done

# --------------------------------------------------------------------------------
# MODEL BUILD SECTION
# --------------------------------------------------------------------------------

# build basgra
for version in BASGRA_N_v1.0; do
    ${DEBUG} docker build \
        --tag pecan/model-basgra-$(echo $version | tr '[A-Z]' '[a-z]'):${IMAGE_VERSION} \
        --build-arg MODEL_VERSION="${version}" \
        --build-arg IMAGE_VERSION="${IMAGE_VERSION}" \
        models/basgra
done

# build biocro
for version in 0.95; do
    ${DEBUG} docker build \
        --tag pecan/model-biocro-${version}:${IMAGE_VERSION} \
        --build-arg MODEL_VERSION="${version}" \
        --build-arg IMAGE_VERSION="${IMAGE_VERSION}" \
        models/biocro
done

# build ed2
for version in git 2.2.0; do
    ${DEBUG} docker build \
        --tag pecan/model-ed2-${version}:${IMAGE_VERSION} \
        --build-arg MODEL_VERSION="${version}" \
        --build-arg IMAGE_VERSION="${IMAGE_VERSION}" \
        --build-arg BINARY_VERSION="2.2" \
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

# build sipnet
for version in git r136; do
    ${DEBUG} docker build \
        --tag pecan/model-sipnet-${version}:${IMAGE_VERSION} \
        --build-arg MODEL_VERSION="${version}" \
        --build-arg IMAGE_VERSION="${IMAGE_VERSION}" \
        models/sipnet
done
