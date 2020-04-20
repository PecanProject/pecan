FROM rocker/shiny

ENV PGHOST=postgres \
    PGDATABASE=bety \
    PGUSER=bety \
    PGPASSWORD=bety \
    GEOCACHE=/srv/shiny-server/geoip.json

RUN apt-get -y install libpq-dev libssl-dev \
    && install2.r -e -s -n -1 curl dbplyr DT leaflet RPostgreSQL \
    && rm -rf /srv/shiny-server/*
ADD . /srv/shiny-server/

# special script to start shiny server and preserve env variable
CMD /srv/shiny-server/save-env-shiny.sh
