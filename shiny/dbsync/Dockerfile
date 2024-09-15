FROM rocker/shiny

ENV PGHOST=postgres \
    PGDATABASE=bety \
    PGUSER=bety \
    PGPASSWORD=bety \
    GEOCACHE=/srv/shiny-server/geoip.json

RUN apt-get update \
    && apt-get -y install curl libpq-dev libssl-dev \
    && install2.r -e -s -n -1 curl dbplyr DT leaflet RPostgreSQL \
    && rm -rf /srv/shiny-server/* \
    && rm -rf /var/lib/apt/lists/*
ADD . /srv/shiny-server/

ADD https://raw.githubusercontent.com/rocker-org/shiny/master/shiny-server.sh /usr/bin/

RUN chmod +x /usr/bin/shiny-server.sh

# special script to start shiny server and preserve env variable
CMD ["/srv/shiny-server/save-env-shiny.sh"]
