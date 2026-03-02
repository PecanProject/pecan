# this needs to be at the top, what version are we building
ARG IMAGE_VERSION="latest"

# ----------------------------------------------------------------------
# BUILD MODEL
# ----------------------------------------------------------------------
FROM pecan/models:${IMAGE_VERSION}

# load packages before adding CTSM
RUN apt update && apt upgrade -y
RUN apt install -y \
    sudo \
    build-essential \
    python3-dev \
    cmake \
    gfortran-9 \
    zlib1g-dev \
    byacc \
    libblas-dev \
    liblapack-dev \
    libxml2-utils \
    libxml-libxml-perl \
    libgdal-dev \
    xmlstarlet \
    csh \
    diffutils \
    rsync

RUN update-alternatives --install /usr/bin/gfortran gfortran /usr/bin/gfortran-9 100

RUN echo '/usr/local/lib' > /etc/ld.so.conf.d/local.conf && ldconfig

WORKDIR /tmp/sources

RUN wget -q http://www.mpich.org/static/downloads/3.3.2/mpich-3.3.2.tar.gz && \
    tar zxf mpich-3.3.2.tar.gz && \
    cd mpich-3.3.2 && \
    ./configure --prefix=/usr/local && \
    make -j 2 install

RUN  wget -q https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.12/hdf5-1.12.0/src/hdf5-1.12.0.tar.gz && \
     tar zxf hdf5-1.12.0.tar.gz && \
     cd hdf5-1.12.0 && \
     ./configure --prefix=/usr/local && \
     make -j 2 install

RUN  wget -q https://github.com/Unidata/netcdf-c/archive/refs/tags/v4.7.4.tar.gz && \
     tar zxf v4.7.4.tar.gz && \
     cd netcdf-c-4.7.4 && \
     ./configure --prefix=/usr/local && \
     make -j 2 install && \
     ldconfig

RUN  wget -q https://github.com/Unidata/netcdf-fortran/archive/refs/tags/v4.5.3.tar.gz && \
     tar zxf v4.5.3.tar.gz && \
     cd netcdf-fortran-4.5.3 && \
     ## original netcdf(version=4.7.3): /usr,  ; fates netcdf(version>=4.7.4): /usr/local; --enable-fortran to ensure netcdf.mod
     ./configure --prefix=/usr/local LDFLAGS="-L/usr/local/lib" CPPFLAGS="-I/usr/local/include" --enable-fortran && \ 
     make -j 2 install && \
     ldconfig

RUN  wget -q https://parallel-netcdf.github.io/Release/pnetcdf-1.12.1.tar.gz && \
     tar zxf pnetcdf-1.12.1.tar.gz && \
     cd pnetcdf-1.12.1 && \
     ./configure MPIF77=/usr/local/bin/mpif77 MPIF90=/usr/local/bin/mpif90 MPICXX=/usr/local/bin/mpicxx MPICC=/usr/local/bin/mpicc && \
     make -j 2 install && \
     ldconfig

RUN cd  /tmp/sources \
    wget -q https://github.com/esmf-org/esmf/archive/refs/tags/ESMF_8_2_0.tar.gz && \
    tar zxf ESMF_8_2_0.tar.gz && \
    cd esmf-ESMF_8_2_0 && \
    export ESMF_DIR='/tmp/sources/esmf-ESMF_8_2_0' && \
    export ESMF_INSTALL_PREFIX='/usr/local' && \
    export ESMF_LAPACK="netlib" && \
    export ESMF_LAPACK_LIBS="-llapack -lblas" && \
    export ESMF_LAPACK_LIBPATH="/usr/lib/x86_64-linux-gnu" && \
    export ESMF_NETCDF="nc-config" && \
    make && \
    make install && \
    ldconfig

RUN rm -rf /tmp/sources
RUN pip install --upgrade pip setuptools

## Above has been tested in the pecan/model container

########################################################################

## Below needs further discussion.

## Load CTSM from https://github.com/NorESMhub/noresm-land-sites-platform/blob/main/docker/api/entrypoint_api.sh
## Following the structure of ED, download CTSM
WORKDIR /src
RUN git clone https://github.com/ESCOMP/CTSM.git && \
    ## go to model folder
    cd CTSM && \
    ## Checkout specific CTSM version, which has been tested in the container, more recent version than this needs to be tested in the future.
    git checkout ctsm5.1.dev108 && \ 
    ## Download fates (tag = sci.1.58.1_api.24.1.0), the version can be modified in Externals_CLM.cfg & CISM should be manually cloned
    ./manage_externals/checkout_externals

RUN set -e && \
    ## set home folder for running CTSM-fates.
    USER=root && \
    HOME=/root && \
    ## create .bashrc file for setting basic compiling parameters for CTSM-fates
    export USER="$USER" && \
    ## 1 /usr/bin/python3
    export PYTHONPATH='/usr/bin/python3' && \ 
    export CIME_MACHINE=container && \
    export MPICC=mpicc && \
    export MPIFC=mpif90 && \
    export MPIF90=mpif90 && \
    export MPIF77=mpif77 && \
    ## if CESMDATAROOT is changed, also change in config.py and other relevant places.
    ## 2 /src/data
    export CESMDATAROOT=${CESMDATAROOT:-/src/data} && \ 
    ## Copy dotcime ((https://github.com/NorESMhub/ctsm-api/tree/main/docker/dotcime)) to pecan/models/fates folder
    ## Dotcime is a key file for setting the compiling environment for CTSM-fates, SHOULD be kept in the home directory of the fates model conatiner as '.cime'
    cd .. && \
    git clone https://github.com/NorESMhub/ctsm-api.git && \
    cp -r ./ctsm-api/docker/dotcime "$HOME"/.cime && \
    rm -rf /src/ctsm-api


########################################################################

# Some variables that can be used to set control the docker build
ARG MODEL_VERSION=git
ENV MODEL_VERSION=${MODEL_VERSION}

# Setup model_info file
## model.json 
COPY model_info.json /work/model.json
RUN sed -i -e "s/@VERSION@/${MODEL_VERSION}/g" \
           -e "s#@BINARY@#/usr/local/bin/fates.${MODEL_VERSION}#g" /work/model.json