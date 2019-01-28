# ----------------------------------------------------------------------
# BUILD SIPNET BINARY
# ----------------------------------------------------------------------
FROM debian:stretch as sipnet-binary

# Some variables that can be used to set control the docker build
ARG MODEL_VERSION=136

# install dependencies
RUN apt-get update \
    && apt-get install -y \
       curl \
       gcc \
       make \
    && rm -rf /var/lib/apt/lists/*

# download, unzip and build sipnet
WORKDIR /src
RUN curl -o sipnet_r${MODEL_VERSION}.tar.gz http://isda.ncsa.illinois.edu/~kooper/PEcAn/sipnet/sipnet_r${MODEL_VERSION}.tar.gz \
    && tar zxf sipnet_r${MODEL_VERSION}.tar.gz \
    && cd sipnet_r${MODEL_VERSION} \
    && make \
    && mv sipnet /src

########################################################################

# ----------------------------------------------------------------------
# BUILD PECAN FOR SIPNET
# ----------------------------------------------------------------------
FROM pecan/models:latest

# ----------------------------------------------------------------------
# INSTALL SIPNET SPECIFIC PIECES
# ----------------------------------------------------------------------

# INSTALL PEcAn.data.atmosphere
#COPY modules/data.atmosphere /src/data.atmosphere/
#RUN Rscript --vanilla -e  "devtools::install_local('/src/data.atmosphere'); q(status=as.integer(!'PEcAn.data.atmosphere' %in% installed.packages()))" 

# INSTALL PEcAn.SIPNET
#COPY models/sipnet /src/sipnet/
#RUN Rscript --vanilla -e  "devtools::install_local('/src/sipnet'); q(status=as.integer(!'PEcAn.SIPNET' %in% installed.packages()))" 

# ----------------------------------------------------------------------
# SETUP FOR SPECIFIC SIPNET VERSION
# ----------------------------------------------------------------------

# Some variables that can be used to set control the docker build
ARG MODEL_VERSION=136

# Setup model_info file
COPY models/sipnet/model_info.json /work/model.json
RUN sed -i -e "s/@VERSION@/${MODEL_VERSION}/g" \
           -e "s#@BINARY@#/usr/local/bin/sipnet.${MODEL_VERSION}#g" /work/model.json

# COPY sipnet binary
COPY --from=sipnet-binary /src/sipnet /usr/local/bin/sipnet.${MODEL_VERSION}
