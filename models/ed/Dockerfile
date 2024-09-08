# this needs to be at the top, what version are we building
ARG IMAGE_VERSION="latest"

# ----------------------------------------------------------------------
# BUILD MODEL BINARY
# ----------------------------------------------------------------------
FROM pecan/models:${IMAGE_VERSION} AS model-binary

# Some variables that can be used to set control the docker build
ARG MODEL_VERSION="2.2.0"
ARG BINARY_VERSION="2.2"

# install dependencies
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       build-essential \
       curl \
       gfortran \
       git \
       libhdf5-openmpi-dev  \
    && rm -rf /var/lib/apt/lists/*

# download, unzip and build ed2
WORKDIR /src
RUN git -c http.sslVerify=false clone https://github.com/EDmodel/ED2.git
COPY data-raw/include.mk.opt /src/ED2/ED/build/make/include.mk.pecan
RUN cd ED2/ED/build \
    && if [ "${MODEL_VERSION}" != "git" ]; then git checkout "v.${MODEL_VERSION}"; fi \
    && ./install.sh -g -p pecan \
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
       libhdf5-openmpi-103 \
       libgomp1 \
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
