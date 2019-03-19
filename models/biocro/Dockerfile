FROM rocker/geospatial

RUN mkdir /data &&\
    mkdir /pecan &&\
    cd /data &&\
    mkdir rundir &&\
    mkdir outdir &&\
    cd /pecan &&\
    mkdir base &&\
    mkdir modules &&\
    mkdir models &&\
    cd /pecan/base &&\
    mkdir logger &&\
    mkdir remote &&\
    mkdir utils &&\
    mkdir db &&\
    mkdir settings &&\
    cd /pecan/modules &&\
    mkdir data.atmosphere &&\
    mkdir data.land &&\
    mkdir nneo &&\
    cd /pecan/models &&\
    mkdir biocro 

COPY ./base/logger /pecan/base/logger

COPY ./base/remote /pecan/base/remote

COPY ./base/utils /pecan/base/utils

COPY ./base/db /pecan/base/db

COPY ./base/settings /pecan/base/settings

COPY ./modules/data.atmosphere /pecan/modules/data.atmosphere

COPY ./modules/data.land /pecan/modules/data.land

COPY ./modules/nneo /pecan/modules/nneo 
 
COPY ./models/biocro /pecan/models/biocro

RUN install2.r --error DBI \
    dbplyr \
    dplyr \
    tibble \
    purrr \
    tidyr \
    glue \
    lubridate \
    magrittr \
    rlang \
    udunits2 \
    XML \
    abind \
    RCurl \
    stringi \
    car \
    data.table \
    geonames \
    ggplot2 \
    httr \
    jsonlite \
    MASS \
    mgcv \
    REddyProc \
    reshape2 \
    rnoaa \
    stringr \
    testthat \
    truncnorm \
    zoo \
    crul \ 
    fauxpas \
    datapack \
    dataone \
    redland \
    sirt &&\ 
    cd /pecan/modules &&\
    R CMD build nneo &&\
    R CMD INSTALL nneo_0.1.0.tar.gz &&\
    cd /pecan/base &&\
    R CMD build logger &&\
    R CMD INSTALL PEcAn.logger_1.7.0.tar.gz &&\
    R CMD build remote &&\
    R CMD INSTALL PEcAn.remote_1.7.0.tar.gz &&\
    R CMD build utils &&\
    R CMD INSTALL PEcAn.utils_1.7.0.tar.gz &&\
    R CMD build db &&\
    R CMD INSTALL PEcAn.DB_1.7.0.tar.gz &&\
    R CMD build settings &&\
    R CMD INSTALL PEcAn.settings_1.7.0.tar.gz &&\
    cd /pecan/modules &&\
    R CMD build data.atmosphere &&\
    R CMD INSTALL PEcAn.data.atmosphere_1.7.0.tar.gz &&\
    R CMD build data.land &&\
    R CMD INSTALL PEcAn.data.land_1.7.0.tar.gz &&\
    cd /pecan/models &&\
    R CMD build biocro &&\
    R CMD INSTALL PEcAn.BIOCRO_1.7.0.tar.gz 

WORKDIR /data

ENTRYPOINT ["Rscript", "/pecan/models/biocro/inst/biocro.Rscript"]
