########################################################################
# First we build the actual model, this has everything that is needed
# for the model to compile. Next we create the final image that has the
# PEcAn code as well as only the model binary.
########################################################################

# this needs to be at the top, what version are we building
ARG IMAGE_VERSION="latest"

# ----------------------------------------------------------------------
# BUILD MODEL BINARY
# ----------------------------------------------------------------------
FROM debian:stretch as model-binary

# Some variables that can be used to set control the docker build
ARG MODEL_VERSION=git

# install dependencies
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       build-essential \
       curl \
       gfortran \
       git \
       libhdf5-dev \
       libopenmpi-dev \
    && rm -rf /var/lib/apt/lists/*

# download, unzip and build ed2
WORKDIR /src
RUN git clone https://github.com/model/repo.git \
    && cd repo \
    && if [ "${MODEL_VERSION}" != "git" ]; then git checkout "v.${MODEL_VERSION}"; fi \
    && ./configure \
    && make

########################################################################

# ----------------------------------------------------------------------
# BUILD PECAN FOR MODEL
# ----------------------------------------------------------------------
FROM pecan/models:${IMAGE_VERSION}

# ----------------------------------------------------------------------
# INSTALL MODEL SPECIFIC PIECES
# ----------------------------------------------------------------------

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       libgfortran3 \
       libopenmpi2 \
    && rm -rf /var/lib/apt/lists/*

# ----------------------------------------------------------------------
# SETUP FOR SPECIFIC MODEL
# ----------------------------------------------------------------------

# Some variables that can be used to set control the docker build
ARG MODEL_VERSION=git

# Setup model_info file
# @VERSION@ is replaced with model version in the model_info.json file
# @BINARY@ is replaced with model binary in the model_info.json file
COPY model_info.json /work/model.json
RUN sed -i -e "s/@VERSION@/${MODEL_VERSION}/g" \
           -e "s#@BINARY@#/usr/local/bin/model#g" /work/model.json

# COPY model binary
COPY --from=model-binary /src/repo/src/model /usr/local/bin/model
