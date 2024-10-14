# this needs to be at the top, what version are we building
ARG IMAGE_VERSION="latest"

# ----------------------------------------------------------------------
# BUILD SIPNET BINARY
# ----------------------------------------------------------------------
FROM pecan/models:${IMAGE_VERSION} as model-binary 

# Some variables that can be used to set control the docker build
ARG MODEL_VERSION=git

# download, unzip and build sipnet
WORKDIR /src
RUN git clone https://github.com/PecanProject/fates.git \
    && cd sipnet \
    && if [ "${MODEL_VERSION}" != "git" ]; then git checkout ${MODEL_VERSION}; fi \
    && make

########################################################################

# ----------------------------------------------------------------------
# BUILD PECAN FOR FATES
# ----------------------------------------------------------------------
FROM pecan/models:${IMAGE_VERSION} 


# ----------------------------------------------------------------------
# SETUP FOR SPECIFIC SIPNET VERSION
# ----------------------------------------------------------------------

# Some variables that can be used to set control the docker build
ARG MODEL_VERSION=git

# Setup model_info file
COPY model_info.json /work/model.json
RUN sed -i -e "s/@VERSION@/${MODEL_VERSION}/g" \
           -e "s#@BINARY@#/usr/local/bin/fates.${MODEL_VERSION}#g" /work/model.json

# COPY sipnet binary
COPY --from=model-binary /src/CESM/fates /usr/local/bin/fates.${MODEL_VERSION} 

# /src/CESM/fates?
