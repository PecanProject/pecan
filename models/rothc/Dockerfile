# this needs to be at the top, what version are we building
ARG IMAGE_VERSION="latest"


# ----------------------------------------------------------------------
# BUILD PECAN FOR MODEL
# ----------------------------------------------------------------------
FROM pecan/models:${IMAGE_VERSION}

# ----------------------------------------------------------------------
# INSTALL MODEL SPECIFIC PIECES
# ----------------------------------------------------------------------

#RUN apt-get update \
#    && apt-get install -y --no-install-recommends \
#       python3.9 \
#    && rm -rf /var/lib/apt/lists/*

# ----------------------------------------------------------------------
# SETUP FOR SPECIFIC MODEL
# ----------------------------------------------------------------------

# Some variables that can be used to set control the docker build
ARG MODEL_VERSION=2.1.0

RUN mkdir -p /tmp/rothc \
    && curl -sSL https://github.com/Rothamsted-Models/RothC_Code/archive/refs/tags/v${MODEL_VERSION}.tar.gz \
        | tar xzf - -C /tmp/rothc \
    && cd /tmp/rothc/RothC_Code-${MODEL_VERSION} \
    && gfortran RothC.for Shell.for -o rothc_bin \
    && mv rothc_bin /usr/local/bin/RothC_v${MODEL_VERSION} \
    && rm -rf /tmp/rothc

# TODO hard-coding this path is probably wrong
# Setup model_info file
# @VERSION@ is replaced with model version in the model_info.json file
# @BINARY@ is replaced with model binary in the model_info.json file
COPY model_info.json /work/model.json
RUN sed -i -e "s/@VERSION@/${MODEL_VERSION}/g" \
           -e "s#@BINARY@#/usr/local/bin/RothC_v${MODEL_VERSION}#g" /work/model.json
