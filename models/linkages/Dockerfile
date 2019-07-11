# this needs to be at the top, what version are we building
ARG IMAGE_VERSION="latest"

# ----------------------------------------------------------------------
# LINKAGES
# ----------------------------------------------------------------------
FROM pecan/models:${IMAGE_VERSION}

# ----------------------------------------------------------------------
# SETUP FOR SPECIFIC LINKAGES VERSION
# Right now only the latest GIT version is supported, otherwise
# you might have to install it from GitHub.
# ----------------------------------------------------------------------

# Some variables that can be used to set control the docker build
ARG MODEL_VERSION=R_version

# Setup model_info file
COPY models/linkages/model_info.json /work/model.json

RUN sed -i -e "s/@VERSION@/${MODEL_VERSION}/g" \
           -e "s#@BINARY@#/usr/local/bin/linkages.R#g" /work/model.json
COPY models/linkages/linkages.R /usr/local/bin
