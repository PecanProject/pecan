# this needs to be at the top, what version are we building
ARG IMAGE_VERSION="latest"
ARG FROM_IMAGE="depends"
FROM pecan/${FROM_IMAGE}:${IMAGE_VERSION}

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
COPY scripts/confirm_deps.R /pecan/scripts/confirm_deps.R
COPY base     /pecan/base/
COPY modules  /pecan/modules/
COPY models   /pecan/models/

# install all PEcAn packages
# `make clean` is to remove artifacts copied in from host system
#   (e.g. basgra.so)
RUN cd /pecan \
    && make clean \
    && make \
    && rm -rf /tmp/downloaded_packages

# COPY WORKFLOW
WORKDIR /work
COPY web/workflow.R docker/base/rstudio.sh /work/

# COMMAND TO RUN
CMD ["bash", "-c", "Rscript --vanilla workflow.R | tee workflow.Rout"]

# variables to store in docker image
ENV PECAN_VERSION=${PECAN_VERSION} \
    PECAN_GIT_BRANCH=${PECAN_GIT_BRANCH} \
    PECAN_GIT_CHECKSUM=${PECAN_GIT_CHECKSUM} \
    PECAN_GIT_DATE=${PECAN_GIT_DATE}
