#!/bin/bash

# Downloads and instantiates a skeleton version of the PEcAn database.

# Needs Postgres to be running where psql can find it -- if not localhost,
# you may need to configure by e.g. setting PGHOST

# N.B. The default URL loads a static file that is not updated often and
# contains only a subset of data, selected to be useful for CI tests and to
# avoid any nonpublic values. If you need a more recent or more complete
# BeTY dump, consider using load_bety.sh instead of this script.

set -e

SOURCE_URL=${1:-'http://isda.ncsa.illinois.edu/~kooper/PEcAn/data/bety.sql'}

psql -q -o /dev/null -U postgres -c \
	"CREATE ROLE bety WITH LOGIN CREATEDB SUPERUSER CREATEROLE UNENCRYPTED PASSWORD 'bety'"
psql -q -o /dev/null -U postgres -c "CREATE DATABASE bety OWNER bety;"
curl -o bety.sql ${SOURCE_URL}
psql -q -o /dev/null -U postgres < bety.sql
rm bety.sql
./scripts/add.models.sh
