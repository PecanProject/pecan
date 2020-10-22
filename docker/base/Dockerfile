# this needs to be at the top, what version are we building
ARG IMAGE_VERSION="latest"
ARG FROM_IMAGE="depends"
FROM pecan/${FROM_IMAGE}:${IMAGE_VERSION}
MAINTAINER Rob Kooper <kooper@illinois.edu>

# ----------------------------------------------------------------------
# PEcAn version information
# ----------------------------------------------------------------------
ARG PECAN_VERSION="develop"
ARG PECAN_GIT_BRANCH="unknown"
ARG PECAN_GIT_CHECKSUM="unknown"
ARG PECAN_GIT_DATE="unknown"

# ----------------------------------------------------------------------
# PEcAn installation from local source
# ----------------------------------------------------------------------
# copy folders
COPY Makefile Makefile.depends /pecan/
COPY scripts/time.sh /pecan/scripts/time.sh
COPY scripts/travis /pecan/scripts/travis/
COPY base     /pecan/base/
COPY modules  /pecan/modules/
COPY models   /pecan/models/

RUN cd /pecan && make && rm -rf /tmp/downloaded_packages

# COPY WORKFLOW
WORKDIR /work
COPY web/workflow.R docker/base/rstudio.sh /work/

# COMMAND TO RUN
CMD Rscript --vanilla workflow.R | tee workflow.Rout

# variables to store in docker image
ENV PECAN_VERSION=${PECAN_VERSION} \
    PECAN_GIT_BRANCH=${PECAN_GIT_BRANCH} \
    PECAN_GIT_CHECKSUM=${PECAN_GIT_CHECKSUM} \
    PECAN_GIT_DATE=${PECAN_GIT_DATE}
