# ----------------------------------------------------------------------
# PECAN FOR MODEL BASE IMAGE
# ----------------------------------------------------------------------
FROM rocker/tidyverse:3.5.1
MAINTAINER Rob Kooper <kooper@illinois.edu>

# ----------------------------------------------------------------------
# INSTALL BINARY/LIBRARY DEPENDENCIES
# ----------------------------------------------------------------------
RUN apt-get update \
    && apt-get -y --no-install-recommends install \
       jags \
       time \
       libgdal-dev \
       librdf0-dev \
       libnetcdf-dev \
       libudunits2-dev \
       libx11-dev \
       libgl1-mesa-dev \
       libglu1-mesa-dev \
       libfreetype6-dev \
    && rm -rf /var/lib/apt/lists/*

# ----------------------------------------------------------------------
# INSTALL DEPENDENCIES
# ----------------------------------------------------------------------
COPY pecan.depends /
RUN bash /pecan.depends \
  && rm -rf /tmp/downloaded_packages

