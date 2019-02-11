#!/bin/bash

set -e
. $( dirname $0 )/func.sh

# INSTALLING SIPNET
(
    travis_time_start "load_database" "Loading minimal BETY database"

    sudo service postgresql stop
    docker run --detach --rm --name postgresql --publish 5432:5432 mdillon/postgis:9.6-alpine
    echo -n "Waiting for Postgres to start...";
    until psql -U postgres -c 'select 1' >/dev/null 2>&1;
      do echo -n ".";
      sleep 1;
    done;
    echo " OK"
    psql -q -o /dev/null -U postgres -c "CREATE ROLE BETY WITH LOGIN CREATEDB SUPERUSER CREATEROLE UNENCRYPTED PASSWORD 'bety'";
    psql -q -o /dev/null -U postgres -c "CREATE DATABASE bety OWNER bety;"
    curl -o bety.sql http://isda.ncsa.illinois.edu/~kooper/PEcAn/data/bety.sql
    psql -q -o /dev/null -U postgres < bety.sql
    rm bety.sql
    ./scripts/add.models.sh
    chmod +x book_source/deploy.sh
    chmod +x documentation/tutorials/deploy.sh

    travis_time_end
)
