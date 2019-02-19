# this needs to be at the top, what version are we building
ARG IMAGE_VERSION="latest"

# ----------------------------------------------------------------------
# compile bookdown to html
# ----------------------------------------------------------------------
FROM pecan/base:${IMAGE_VERSION} AS pecandocs
MAINTAINER Rob Kooper <kooper@illinois.edu>

RUN apt-get update \
    && apt-get install -y --no-install-recommends pandoc \
    && install2.r -e -s -n -1 bookdown \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src/book_source/
COPY book_source /src/book_source/
COPY documentation /src/documentation/
COPY docker-compose.yml /src/
COPY docker/env.example /src/docker/
RUN make build

# ----------------------------------------------------------------------
# copy html pages to container
# ----------------------------------------------------------------------
FROM httpd
MAINTAINER Rob Kooper <kooper@illinois.edu>

COPY docker/docs/index.html /usr/local/apache2/htdocs/
COPY --from=pecandocs /src/book_source/_book/ /usr/local/apache2/htdocs/docs/pecan/

# ----------------------------------------------------------------------
# PEcAn version information
# ----------------------------------------------------------------------
ARG PECAN_VERSION="develop"
ARG PECAN_GIT_BRANCH="unknown"
ARG PECAN_GIT_CHECKSUM="unknown"
ARG PECAN_GIT_DATE="unknown"

# variables to store in docker image
ENV PECAN_VERSION=${PECAN_VERSION} \
    PECAN_GIT_BRANCH=${PECAN_GIT_BRANCH} \
    PECAN_GIT_CHECKSUM=${PECAN_GIT_CHECKSUM} \
    PECAN_GIT_DATE=${PECAN_GIT_DATE}
