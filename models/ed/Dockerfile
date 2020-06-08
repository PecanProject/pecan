# this needs to be at the top, what version are we building
ARG IMAGE_VERSION="latest"

# ----------------------------------------------------------------------
# BUILD MODEL BINARY
# ----------------------------------------------------------------------
FROM debian:stretch as model-binary

# Some variables that can be used to set control the docker build
ARG MODEL_VERSION="2.2.0"
ARG BINARY_VERSION="2.2"

# specify fortran compiler
ENV FC_TYPE=GNU

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
RUN git -c http.sslVerify=false clone https://github.com/EDmodel/ED2.git \
    && cd ED2/ED/build \
    && curl -o make/include.mk.VM http://isda.ncsa.illinois.edu/~kooper/EBI/include.mk.opt.`uname -s` \
    && if [ "${MODEL_VERSION}" != "git" ]; then git checkout "v.${MODEL_VERSION}"; fi \
    && ./install.sh -g -p VM \
    && mv /src/ED2/ED/build/ed_${BINARY_VERSION}-opt /src/ED2/ED/build/ed

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

# INSTALL PEcAn.ED2
#COPY models/ed2 /src/ed/
#RUN Rscript --vanilla -e  "devtools::install_local('/src/ed'); q(status=as.integer(!'PEcAn.ED2' %in% installed.packages()))" 

# ----------------------------------------------------------------------
# SETUP FOR SPECIFIC MODEL
# ----------------------------------------------------------------------

# Some variables that can be used to set control the docker build
ARG MODEL_VERSION="2.2.0"

# Setup model_info file
COPY model_info.json /work/model.json
RUN sed -i -e "s/@VERSION@/${MODEL_VERSION}/g" \
           -e "s#@BINARY@#/usr/local/bin/ed.${MODEL_VERSION}#g" /work/model.json

# COPY model binary
COPY --from=model-binary /src/ED2/ED/build/ed /usr/local/bin/ed.${MODEL_VERSION}
