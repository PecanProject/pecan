FROM rocker/shiny

ENV PGHOST=postgres \
    PGDATABASE=bety \
    PGUSER=bety \
    PGPASSWORD=bety

RUN apt-get -y install libpq-dev libssl-dev \
    && install2.r -e -s -n -1 dbplyr DT leaflet rgeolocate RPostgreSQL \
    && rm -rf /srv/shiny-server/*
ADD . /srv/shiny-server/
