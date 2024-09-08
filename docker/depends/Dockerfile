ARG R_VERSION="4.1"
ARG FROM_IMAGE="rocker/tidyverse"

# ----------------------------------------------------------------------
# PECAN FOR MODEL BASE IMAGE
# ----------------------------------------------------------------------
FROM ${FROM_IMAGE}:${R_VERSION}

# ----------------------------------------------------------------------
# INSTALL BINARY/LIBRARY DEPENDENCIES
# ----------------------------------------------------------------------
RUN apt-get update \
    && apt-get -y --no-install-recommends install \
       cmake \
       curl \
       jags \
       time \
       openssh-client \
       patch \
       rsync \
       libgdal-dev \
       libglpk-dev \
       librdf0-dev \
       libnetcdf-dev \
       libudunits2-dev \
       libgl1-mesa-dev \
       libglu1-mesa-dev \
    && rm -rf /var/lib/apt/lists/*

# ----------------------------------------------------------------------
# INSTALL DEPENDENCIES
# ----------------------------------------------------------------------
COPY pecan.depends.R pecan_package_dependencies.csv pecan_deps_from_github.txt /
RUN Rscript -e "install.packages(c('desc', 'remotes'))" \
  && R_LIBS_USER='/usr/local/lib/R/site-library' Rscript /pecan.depends.R \
  && rm -rf /tmp/*

