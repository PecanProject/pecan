#!/bin/bash

# Use docker.sh -h for instructions on how to use this script.

# exit script if error occurs (-e) in any command of pipeline (pipefail)
set -o pipefail
set -e

cd "$(dirname "$0")"

# Set defaults
DEBUG=""
DEPEND=""
PLATFORM="linux/amd64"
REGISTRY="pecan"
PUSH=""

# Extract DEFAULT_R_VERSION from GitHub action
R_VERSION=$(awk '/DEFAULT_R_VERSION:/ {print $2; exit}' .github/workflows/docker.yml)
if [ -z "$R_VERSION" ]; then
    echo "Warning: Could not extract DEFAULT_R_VERSION from .github/workflows/docker.yml, using 4.4 as fallback" >&2
    R_VERSION="4.4"
fi

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
if [ "${PECAN_GIT_BRANCH}" == "main" ]; then
    IMAGE_VERSION=${IMAGE_VERSION:-"latest"}
elif [ "${PECAN_GIT_BRANCH}" == "develop" ]; then
    IMAGE_VERSION=${IMAGE_VERSION:-"develop"}
else
    IMAGE_VERSION=${IMAGE_VERSION:-"testing"}
fi

# Helper function to show help
show_help() {
    cat << EOF
$0 [OPTIONS]

The following script can be used to create all docker images. Without any
options this will build all images and tag them based on the branch you
are on. The main branch will be tagged with latest, develop branch will
be tagged with develop and any other branch will be tagged with testing.

Options:
  -d, --depend          Force a build of the depends image from scratch
  -h, --help            Show this help message
  -n, --dry-run         Print commands without executing them
  -p, --platform PLATFORM
                        Platform to build for (default: linux/amd64)
  -r, --r-version VERSION
                        R version to use (default: ${R_VERSION})
  -t, --tag TAG         Tag to use for the build images
  --push                Push images to registry after building
  --registry REGISTRY   Registry and organization prefix (default: pecan)
                        Examples: pecan, ghcr.io/pecanproject

Examples:
  $0                                    # Build all images with defaults
  $0 -d -t mytag                        # Build depends and all images with tag 'mytag'
  $0 --platform linux/arm64 --push      # Build for ARM64 and push to registry
  $0 --registry ghcr.io/pecanproject --push # Build and push to GitHub Container Registry
EOF
    exit 0
}

# Parse command line options, overriding any environment variables
while [ $# -gt 0 ]; do
    case "$1" in
    -d|--depend)
        DEPEND="build"
        shift
        ;;
    -h|--help)
        show_help
        ;;
    -n|--dry-run)
        DEBUG="echo"
        shift
        ;;
    -p|--platform)
        PLATFORM="$2"
        shift 2
        ;;
    --push)
        PUSH="true"
        shift
        ;;
    -r|--r-version)
        R_VERSION="$2"
        shift 2
        ;;
    --registry)
        REGISTRY="$2"
        shift 2
        ;;
    -t|--tag)
        IMAGE_VERSION="$2"
        shift 2
        ;;
    -*)
        echo "Unknown option: $1" >&2
        show_help
        ;;
    *)
        shift
        ;;
    esac
done

# pass github workflow
if [ -n "$GITHUB_WORKFLOW" ]; then
    GITHUB_WORKFLOW_ARG="--build-arg GITHUB_WORKFLOW=${GITHUB_WORKFLOW}"
fi

# information for user before we build things
echo "# ----------------------------------------------------------------------"
echo "# Building PEcAn"
echo "#  PECAN_VERSION      : ${VERSION}"
echo "#  PECAN_GIT_BRANCH   : ${PECAN_GIT_BRANCH}"
echo "#  PECAN_GIT_DATE     : ${PECAN_GIT_DATE}"
echo "#  PECAN_GIT_CHECKSUM : ${PECAN_GIT_CHECKSUM}"
echo "#  IMAGE_VERSION      : ${IMAGE_VERSION}"
echo "#  REGISTRY           : ${REGISTRY}"
echo "#  PLATFORM           : ${PLATFORM}"
echo "#  R_VERSION          : ${R_VERSION}"
echo "#  PUSH               : ${PUSH:-false}"
echo "#"
echo "# Created images will be tagged with '${REGISTRY}/image:${IMAGE_VERSION}'. If you want to"
echo "# test this build you can use:"
echo "# PECAN_VERSION='${IMAGE_VERSION}' docker-compose up"
echo "#"
echo "# The docker image for dependencies takes a long time to build. You"
echo "# can use a prebuilt version (default) or force a new version to be"
echo "# built locally using: -d or --depend"
echo "#"
echo "# EXPERIMENTAL: To attempt updating an existing dependency image"
echo "# instead of building from scratch, use UPDATE_DEPENDS_FROM_TAG=<tag>"
echo "# ----------------------------------------------------------------------"

# --------------------------------------------------------------------------------
# STEP 1: Build depends image (no dependency)
# --------------------------------------------------------------------------------
if [ "${DEPEND}" == "build" ]; then
    ${DEBUG} docker build \
        --pull \
        --platform ${PLATFORM} \
        --secret id=github_token,env=GITHUB_PAT \
        --build-arg R_VERSION=${R_VERSION} ${GITHUB_WORKFLOW_ARG} \
        --tag ${REGISTRY}/depends:${IMAGE_VERSION} \
        docker/depends
    if [ "${PUSH}" == "true" ]; then
        ${DEBUG} docker push ${REGISTRY}/depends:${IMAGE_VERSION}
    fi
elif [ "${UPDATE_DEPENDS_FROM_TAG}" != "" ]; then
    echo "# Attempting to update from existing ${REGISTRY}/depends:${UPDATE_DEPENDS_FROM_TAG}."
    echo "# This is experimental. if it fails, please instead use"
    echo "# '-d' or '--depend' to start from a known clean state."
    ${DEBUG} docker build \
        --pull \
        --platform ${PLATFORM} \
        --secret id=github_token,env=GITHUB_PAT \
        --build-arg PARENT_IMAGE="${REGISTRY}/depends" \
        --build-arg R_VERSION=${UPDATE_DEPENDS_FROM_TAG} ${GITHUB_WORKFLOW_ARG} \
        --tag ${REGISTRY}/depends:${IMAGE_VERSION} \
        docker/depends
    if [ "${PUSH}" == "true" ]; then
        ${DEBUG} docker push ${REGISTRY}/depends:${IMAGE_VERSION}
    fi
else
    if [ "$( docker image ls -q ${REGISTRY}/depends:${IMAGE_VERSION} )" == "" ]; then
        if [ "${PECAN_GIT_BRANCH}" != "main" ]; then
            ${DEBUG} docker pull ${REGISTRY}/depends:R${R_VERSION} || ${DEBUG} docker pull pecan/depends:R${R_VERSION}
            if [ "${IMAGE_VERSION}" != "develop" ]; then
                ${DEBUG} docker tag ${REGISTRY}/depends:R${R_VERSION} ${REGISTRY}/depends:${IMAGE_VERSION} 2>/dev/null || \
                ${DEBUG} docker tag pecan/depends:R${R_VERSION} ${REGISTRY}/depends:${IMAGE_VERSION}
            fi
        else
            if [ "$( docker image ls -q ${REGISTRY}/depends:latest )" == "" ]; then
                ${DEBUG} docker pull ${REGISTRY}/depends:latest || ${DEBUG} docker pull pecan/depends:latest
            fi
            if [ "${IMAGE_VERSION}" != "latest" ]; then
                ${DEBUG} docker tag ${REGISTRY}/depends:latest ${REGISTRY}/depends:${IMAGE_VERSION} 2>/dev/null || \
                ${DEBUG} docker tag pecan/depends:latest ${REGISTRY}/depends:${IMAGE_VERSION}
            fi
        fi
    fi
fi
echo ""

# --------------------------------------------------------------------------------
# STEP 2: Build base image (depends on depends)
# --------------------------------------------------------------------------------
${DEBUG} docker build \
    --platform ${PLATFORM} \
    --secret id=github_token,env=GITHUB_PAT \
    --tag ${REGISTRY}/base:${IMAGE_VERSION} \
    --build-arg PARENT_IMAGE="${PARENT_IMAGE:-${REGISTRY}/depends}" \
    --build-arg IMAGE_VERSION="${IMAGE_VERSION}" ${GITHUB_WORKFLOW_ARG} \
    --build-arg PECAN_VERSION="${VERSION}" \
    --build-arg PECAN_GIT_BRANCH="${PECAN_GIT_BRANCH}" \
    --build-arg PECAN_GIT_CHECKSUM="${PECAN_GIT_CHECKSUM}" \
    --build-arg PECAN_GIT_DATE="${PECAN_GIT_DATE}" \
    --file docker/base/Dockerfile \
    .
if [ "${PUSH}" == "true" ]; then
    ${DEBUG} docker push ${REGISTRY}/base:${IMAGE_VERSION}
fi
echo ""

# --------------------------------------------------------------------------------
# STEP 3: Build models image (depends on base)
# --------------------------------------------------------------------------------
${DEBUG} docker build \
    --platform ${PLATFORM} \
    --tag ${REGISTRY}/models:${IMAGE_VERSION} \
    --build-arg PARENT_IMAGE="${REGISTRY}/base" \
    --build-arg IMAGE_VERSION="${IMAGE_VERSION}" ${GITHUB_WORKFLOW_ARG} \
    docker/models
if [ "${PUSH}" == "true" ]; then
    ${DEBUG} docker push ${REGISTRY}/models:${IMAGE_VERSION}
fi
echo ""

# --------------------------------------------------------------------------------
# STEP 4: Build model-* images (depend on models)
# --------------------------------------------------------------------------------

# build basgra
${DEBUG} docker build \
    --platform ${PLATFORM} \
    --tag ${REGISTRY}/model-basgra-basgra_n_v1:${IMAGE_VERSION} \
    --build-arg PARENT_IMAGE="${REGISTRY}/models" \
    --build-arg IMAGE_VERSION="${IMAGE_VERSION}" ${GITHUB_WORKFLOW_ARG} \
    --build-arg MODEL_VERSION="BASGRA_N_v1" \
    models/basgra
if [ "${PUSH}" == "true" ]; then
    ${DEBUG} docker push ${REGISTRY}/model-basgra-basgra_n_v1:${IMAGE_VERSION}
fi

# build biocro
${DEBUG} docker build \
    --platform ${PLATFORM} \
    --tag ${REGISTRY}/model-biocro-0.95:${IMAGE_VERSION} \
    --build-arg PARENT_IMAGE="${REGISTRY}/models" \
    --build-arg IMAGE_VERSION="${IMAGE_VERSION}" ${GITHUB_WORKFLOW_ARG} \
    --build-arg MODEL_VERSION="0.95" \
    models/biocro
if [ "${PUSH}" == "true" ]; then
    ${DEBUG} docker push ${REGISTRY}/model-biocro-0.95:${IMAGE_VERSION}
fi

# build ed2 (only git version)
${DEBUG} docker build \
    --platform ${PLATFORM} \
    --tag ${REGISTRY}/model-ed2-git:${IMAGE_VERSION} \
    --build-arg PARENT_IMAGE="${REGISTRY}/models" \
    --build-arg IMAGE_VERSION="${IMAGE_VERSION}" ${GITHUB_WORKFLOW_ARG} \
    --build-arg MODEL_VERSION="git" \
    models/ed
if [ "${PUSH}" == "true" ]; then
    ${DEBUG} docker push ${REGISTRY}/model-ed2-git:${IMAGE_VERSION}
fi

# build maespa
${DEBUG} docker build \
    --platform ${PLATFORM} \
    --tag ${REGISTRY}/model-maespa-git:${IMAGE_VERSION} \
    --build-arg PARENT_IMAGE="${REGISTRY}/models" \
    --build-arg IMAGE_VERSION="${IMAGE_VERSION}" ${GITHUB_WORKFLOW_ARG} \
    --build-arg MODEL_VERSION="git" \
    models/maespa
if [ "${PUSH}" == "true" ]; then
    ${DEBUG} docker push ${REGISTRY}/model-maespa-git:${IMAGE_VERSION}
fi

# build sipnet
${DEBUG} docker build \
    --platform ${PLATFORM} \
    --tag ${REGISTRY}/model-sipnet-git:${IMAGE_VERSION} \
    --build-arg PARENT_IMAGE="${REGISTRY}/models" \
    --build-arg IMAGE_VERSION="${IMAGE_VERSION}" ${GITHUB_WORKFLOW_ARG} \
    --build-arg MODEL_VERSION="git" \
    models/sipnet
if [ "${PUSH}" == "true" ]; then
    ${DEBUG} docker push ${REGISTRY}/model-sipnet-git:${IMAGE_VERSION}
fi
echo ""

# --------------------------------------------------------------------------------
# STEP 5: Build baseplus images (depend on base) - can build in parallel with models
# --------------------------------------------------------------------------------

# build docs
${DEBUG} docker build \
    --platform ${PLATFORM} \
    --tag ${REGISTRY}/docs:${IMAGE_VERSION} \
    --build-arg PARENT_IMAGE="${REGISTRY}/base" \
    --build-arg IMAGE_VERSION="${IMAGE_VERSION}" ${GITHUB_WORKFLOW_ARG} \
    --build-arg PECAN_VERSION="${VERSION}" \
    --build-arg PECAN_GIT_BRANCH="${PECAN_GIT_BRANCH}" \
    --build-arg PECAN_GIT_CHECKSUM="${PECAN_GIT_CHECKSUM}" \
    --build-arg PECAN_GIT_DATE="${PECAN_GIT_DATE}" \
    --file docker/docs/Dockerfile \
    .
if [ "${PUSH}" == "true" ]; then
    ${DEBUG} docker push ${REGISTRY}/docs:${IMAGE_VERSION}
fi

# build executor
${DEBUG} docker build \
    --platform ${PLATFORM} \
    --tag ${REGISTRY}/executor:${IMAGE_VERSION} \
    --build-arg PARENT_IMAGE="${REGISTRY}/base" \
    --build-arg IMAGE_VERSION="${IMAGE_VERSION}" ${GITHUB_WORKFLOW_ARG} \
    docker/executor
if [ "${PUSH}" == "true" ]; then
    ${DEBUG} docker push ${REGISTRY}/executor:${IMAGE_VERSION}
fi

# build api
${DEBUG} docker build \
    --platform ${PLATFORM} \
    --secret id=github_token,env=GITHUB_PAT \
    --tag ${REGISTRY}/api:${IMAGE_VERSION} \
    --build-arg PARENT_IMAGE="${REGISTRY}/base" \
    --build-arg IMAGE_VERSION="${IMAGE_VERSION}" ${GITHUB_WORKFLOW_ARG} \
    --build-arg PECAN_VERSION="${VERSION}" \
    --build-arg PECAN_GIT_BRANCH="${PECAN_GIT_BRANCH}" \
    --build-arg PECAN_GIT_CHECKSUM="${PECAN_GIT_CHECKSUM}" \
    --build-arg PECAN_GIT_DATE="${PECAN_GIT_DATE}" \
    apps/api/
if [ "${PUSH}" == "true" ]; then
    ${DEBUG} docker push ${REGISTRY}/api:${IMAGE_VERSION}
fi
echo ""

# --------------------------------------------------------------------------------
# STEP 6: Build extras images (no dependency on base) - can build in parallel
# --------------------------------------------------------------------------------

# build web
${DEBUG} docker build \
    --platform ${PLATFORM} \
    --tag ${REGISTRY}/web:${IMAGE_VERSION} \
    --build-arg IMAGE_VERSION="${IMAGE_VERSION}" ${GITHUB_WORKFLOW_ARG} \
    --build-arg PECAN_VERSION="${VERSION}" \
    --build-arg PECAN_GIT_BRANCH="${PECAN_GIT_BRANCH}" \
    --build-arg PECAN_GIT_CHECKSUM="${PECAN_GIT_CHECKSUM}" \
    --build-arg PECAN_GIT_DATE="${PECAN_GIT_DATE}" \
    --file docker/web/Dockerfile \
    .
if [ "${PUSH}" == "true" ]; then
    ${DEBUG} docker push ${REGISTRY}/web:${IMAGE_VERSION}
fi

# build shiny-dbsync
${DEBUG} docker build \
    --platform ${PLATFORM} \
    --tag ${REGISTRY}/shiny-dbsync:${IMAGE_VERSION} \
    --build-arg IMAGE_VERSION="${IMAGE_VERSION}" ${GITHUB_WORKFLOW_ARG} \
    shiny/dbsync
if [ "${PUSH}" == "true" ]; then
    ${DEBUG} docker push ${REGISTRY}/shiny-dbsync:${IMAGE_VERSION}
fi

# build data
${DEBUG} docker build \
    --platform ${PLATFORM} \
    --tag ${REGISTRY}/data:${IMAGE_VERSION} \
    --build-arg IMAGE_VERSION="${IMAGE_VERSION}" ${GITHUB_WORKFLOW_ARG} \
    docker/data
if [ "${PUSH}" == "true" ]; then
    ${DEBUG} docker push ${REGISTRY}/data:${IMAGE_VERSION}
fi

# build monitor
${DEBUG} docker build \
    --platform ${PLATFORM} \
    --tag ${REGISTRY}/monitor:${IMAGE_VERSION} \
    --build-arg IMAGE_VERSION="${IMAGE_VERSION}" ${GITHUB_WORKFLOW_ARG} \
    docker/monitor
if [ "${PUSH}" == "true" ]; then
    ${DEBUG} docker push ${REGISTRY}/monitor:${IMAGE_VERSION}
fi

# build rstudio-nginx
${DEBUG} docker build \
    --platform ${PLATFORM} \
    --tag ${REGISTRY}/rstudio-nginx:${IMAGE_VERSION} \
    --build-arg IMAGE_VERSION="${IMAGE_VERSION}" ${GITHUB_WORKFLOW_ARG} \
    docker/rstudio-nginx
if [ "${PUSH}" == "true" ]; then
    ${DEBUG} docker push ${REGISTRY}/rstudio-nginx:${IMAGE_VERSION}
fi
echo ""

