# this needs to be at the top, what version are we building
ARG IMAGE_VERSION="latest"
ARG PARENT_IMAGE="pecan/base"

# ----------------------------------------------------------------------
# compile bookdown to html
# ----------------------------------------------------------------------
FROM ${PARENT_IMAGE}:${IMAGE_VERSION} AS pecandocs

RUN apt-get update \
    && apt-get install -y --no-install-recommends pandoc \
    && Rscript \
        -e 'repos <- c(getOption("repos"),' \
        -e '           sub(r"(\d{4}-\d{2}-\d{2})", "latest", getOption("repos")))' \
        -e 'remotes::install_version("rmarkdown", ">= 2.19", dependencies = TRUE, upgrade = FALSE, repos = repos)' \
        -e 'remotes::install_version("knitr", ">= 1.42", dependencies = TRUE, upgrade = FALSE, repos = repos)' \
        -e 'remotes::install_version("bookdown", ">= 0.31", dependencies = TRUE, upgrade = FALSE, repos = repos)' \
        -e 'install.packages("pkgdown", repos = repos)' \
    && rm -rf /var/lib/apt/lists/*

# ----------------------------------------------------------------------
# Compile bookdown documentation
# ----------------------------------------------------------------------    
WORKDIR /src/book_source/
COPY book_source /src/book_source/
COPY documentation /src/documentation/
COPY docker-compose.yml /src/
COPY docker/env.example /src/docker/
RUN make build

# ----------------------------------------------------------------------
# Build pkgdown documentation
# ----------------------------------------------------------------------
WORKDIR /pkgdoc/
COPY Makefile Makefile.depends /pkgdoc/
COPY scripts/build_pkgdown.R /pkgdoc/scripts/build_pkgdown.R
COPY base /pkgdoc/base/
COPY modules /pkgdoc/modules/
COPY models /pkgdoc/models/
RUN make clean && make pkgdocs

# ----------------------------------------------------------------------
# copy html pages to container
# ----------------------------------------------------------------------
FROM httpd

# need curl for health checks
RUN apt-get update \
    && apt-get install -y curl \
    && rm -rf /var/lib/apt/lists/*

COPY docker/docs/index.html /usr/local/apache2/htdocs/
COPY --from=pecandocs /src/book_source/_book/ /usr/local/apache2/htdocs/docs/pecan/
COPY --from=pecandocs /pkgdoc/_pkgdown_docs/ /usr/local/apache2/htdocs/pkgdocs/

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
