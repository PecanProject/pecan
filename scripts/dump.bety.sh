#!/bin/bash

# ----------------------------------------------------------------------
# START CONFIGURATION SECTION
# ----------------------------------------------------------------------

# name of the dabase to dump
# this script assumes the user running it has access to the database
DATABASE=${DATABASE:-"bety"}

# psql options
# this allows you to add the user to use as well as any other options
PG_OPT=${PG_OPT-"-U bety"}

# ID's used in database
# These ID's need to be unique for the sharing to work. If you want
# to share your data, send email to kooper@illinois.edu to claim
# your ID range. The master list is maintained at
# https://github.com/PecanProject/bety/wiki/Distributed-BETYdb
#
#  0 - EBI           - David LeBauer
#  1 - BU            - Mike Dietze
#  2 - Brookhaven    - Shawn Serbin
#  3 - Purdue        - Jeanne Osnas
#  4 - Virginia Tech - Quinn Thomas
# 99 - VM
MYSITE=${MYSITE:-99}

# access level requirement
# 0 - private
# 4 - public
LEVEL=${LEVEL:-3}

# dump unchecked traits and yields
# set this to "YES" to dump all unchecked traits/yields as well
UNCHECKED=${UNCHECKED:-"NO"}

# keep users
# set this to YES to dump all user information, otherwise it will
# be anonymized
KEEPUSERS=${KEEPUSERS:-"NO"}

# location where to write the results, this will be a tar file
OUTPUT=${OUTPUT:-"$PWD/dump"}

# Should the process be quiet
QUIET=${QUIET:-"NO"}

# Should all the data be dumped
ALLDATA=${ALLDATA:-"NO"}

# ----------------------------------------------------------------------
# END CONFIGURATION SECTION
# ----------------------------------------------------------------------

# parse command line options
while getopts ad:hkl:m:o:p:qu opt; do
  case $opt in
  a)
    ALLDATA="YES"
    ;;
  d)
    DATABASE=$OPTARG
    ;;
  h)
    echo "$0 [-a] [-d database] [-h] [-k] [-l 0,1,2,3,4] [-m my siteid] [-o folder] [-p psql options] [-u]"
    echo " -a dump all records"
    echo " -d database, default is bety"
    echo " -h this help page"
    echo " -k keep users, default is to be anonymized"
    echo " -l level of data that can be dumped, default is 3"
    echo " -m site id, default is 99 (VM)"
    echo " -o output folder where dumped data is written, default is dump"
    echo " -p additional psql command line options, default is -U bety"
    echo " -q should the export be quiet"
    echo " -u should unchecked data be dumped, default is NO"
    exit 0
    ;;
  k)
    KEEPUSERS="YES"
    ;;
  l)
    LEVEL=$OPTARG
    ;;
  m)
    MYSITE=$OPTARG
    ;;
  o)
    OUTPUT=$OPTARG
    ;;
  p)
    PG_OPT=$OPTARG
    ;;
  q)
    QUIET="YES"
    ;;
  u)
    UNCHECKED="YES"
    ;;
  esac
done

# Table that contains the users, this table will be anonymized
USER_TABLES="users"

# list of all tables, schema_migrations is ignored since that
# will be imported during creaton
CLEAN_TABLES="benchmark_sets benchmarks"
CLEAN_TABLES="${CLEAN_TABLES} citations covariates cultivars dbfiles"
CLEAN_TABLES="${CLEAN_TABLES} ensembles entities experiments formats inputs"
CLEAN_TABLES="${CLEAN_TABLES} likelihoods machines managements metrics"
CLEAN_TABLES="${CLEAN_TABLES} methods mimetypes models modeltypes"
CLEAN_TABLES="${CLEAN_TABLES} pfts posteriors priors reference_runs"
CLEAN_TABLES="${CLEAN_TABLES} runs sites species treatments"
CLEAN_TABLES="${CLEAN_TABLES} variables workflows"
CLEAN_TABLES="${CLEAN_TABLES} projects sitegroups"

# tables that have checks that need to be looked at.
CHECK_TABLES="traits yields"

# tables that have many to many relationships
MANY_TABLES="benchmarks_benchmarks_reference_runs benchmarks_ensembles"
MANY_TABLES="${MANY_TABLES} benchmarks_ensembles_scores benchmarks_metrics benchmark_sets_benchmark_reference_runs"
MANY_TABLES="${MANY_TABLES} citations_sites citations_treatments"
MANY_TABLES="${MANY_TABLES} cultivars_pfts current_posteriors"
MANY_TABLES="${MANY_TABLES} experiments_sites experiments_treatments"
MANY_TABLES="${MANY_TABLES} formats_variables inputs_runs"
MANY_TABLES="${MANY_TABLES} managements_treatments modeltypes_formats"
MANY_TABLES="${MANY_TABLES} pfts_priors pfts_species"
MANY_TABLES="${MANY_TABLES} posterior_samples posteriors_ensembles"
MANY_TABLES="${MANY_TABLES} sitegroups_sites sites_cultivars trait_covariate_associations"

# tables that should NOT be dumped
IGNORE_TABLES="sessions"
SYSTEM_TABLES="schema_migrations spatial_ref_sys"

# be quiet if not interactive
if ! tty -s ; then
    exec 1>/dev/null
fi

# this value should be constant, do not change
ID_RANGE=1000000000

# make output folder
mkdir -p "${OUTPUT}"
DUMPDIR="/tmp/$$"
mkdir -p "${DUMPDIR}"
chmod 777 "${DUMPDIR}"

# compute range based on MYSITE
if [ "${ALLDATA}" != "YES" ]; then
  START_ID=$(( MYSITE * ID_RANGE + 1 ))
  LAST_ID=$(( START_ID + ID_RANGE - 1 ))
  if [ "${QUIET}" != "YES" ]; then
    echo "Dumping all items that have id    : [${START_ID} - ${LAST_ID}]"
  fi
  LIMIT="(id >= ${START_ID} AND id <= ${LAST_ID})"
else
  LIMIT="TRUE"
  if [ "${QUIET}" != "YES" ]; then
    echo "Dumping all items that have id    : ALL ITEMS"
  fi
fi

# find current schema version
# following returns a triple:
# - number of migrations
# - largest migration
# - hash of all migrations
MIGRATIONS=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c 'SELECT COUNT(version) FROM schema_migrations' | tr -d ' ' )
VERSION=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c 'SELECT md5(array_agg(version)::text) FROM (SELECT version FROM schema_migrations ORDER BY version) as v;' | tr -d ' ' )
LATEST=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c 'SELECT version FROM schema_migrations ORDER BY version DESC LIMIT 1' | tr -d ' ' )
NOW=$( date -u +"%Y-%m-%dT%H:%M:%SZ" )
echo "${MIGRATIONS}	${VERSION}	${LATEST}	${NOW}" > "${OUTPUT}/version.txt"

# dump schema
if [ "${QUIET}" != "YES" ]; then
  printf "Dumping %-25s : " "schema"
fi
pg_dump ${PG_OPT} -s "${DATABASE}" -O -x > "${DUMPDIR}/${VERSION}.schema"
if [ "${QUIET}" != "YES" ]; then
  echo "DUMPED version ${VERSION} with ${MIGRATIONS}, latest migration is ${LATEST}"
fi

# dump ruby special table
if [ "${QUIET}" != "YES" ]; then
  printf "Dumping %-25s : " "schema_migrations"
fi
ADD=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "SELECT count(*) FROM schema_migrations;" | tr -d ' ' )
psql ${PG_OPT} -t -q -d "${DATABASE}" -c "\COPY schema_migrations TO '${DUMPDIR}/schema_migrations.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV, ENCODING 'UTF-8')"
if [ "${QUIET}" != "YES" ]; then
  echo "DUMPED ${ADD}"
fi

# skip following tables
# - inputs_runs (PEcAn, site specific)
# - posteriors_runs (PEcAn, site specific, is this used?)
# - runs (PEcAn, site specific)
# - workflows (PEcAn, site specific)

# dump users
if [ "${QUIET}" != "YES" ]; then
  printf "Dumping %-25s : " "users"
fi
if [ "${KEEPUSERS}" == "YES" ]; then
    psql ${PG_OPT} -t -q -d "${DATABASE}" -c "\COPY (SELECT * FROM ${USER_TABLES} WHERE ${LIMIT})  TO '${DUMPDIR}/users.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV, ENCODING 'UTF-8')"
else
    psql ${PG_OPT} -t -q -d "${DATABASE}" -c "\COPY (SELECT id, CONCAT('user', id) AS login, CONCAT('user ' , id) AS name, CONCAT('betydb+', id, '@gmail.com') as email, 'Urbana' AS city,  'USA' AS country, '' AS area, '1234567890abcdef' AS crypted_password, 'BU' AS salt, NOW() AS created_at, NOW() AS updated_at, NULL as remember_token, NULL AS remember_token_expires_at, 3 AS access_level, 4 AS page_access_level, NULL AS apikey, 'IL' AS state_prov, '61801' AS postal_code FROM ${USER_TABLES} WHERE ${LIMIT}) TO '${DUMPDIR}/users.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV, ENCODING 'UTF-8')"
fi
ADD=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "SELECT count(*) FROM ${USER_TABLES} WHERE ${LIMIT};" | tr -d ' ' )
if [ "${QUIET}" != "YES" ]; then
  echo "DUMPED ${ADD}"
fi

# unrestricted tables
for T in ${CLEAN_TABLES} ${MANY_TABLES}; do
    if [ "${QUIET}" != "YES" ]; then
      printf "Dumping %-25s : " "${T}"
    fi
    psql ${PG_OPT} -t -q -d "${DATABASE}" -c "\COPY (SELECT * FROM ${T} WHERE ${LIMIT}) TO '${DUMPDIR}/${T}.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV, ENCODING 'UTF-8')"
    ADD=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "SELECT count(*) FROM ${T} WHERE ${LIMIT}" | tr -d ' ' )
    if [ "${QUIET}" != "YES" ]; then
      echo "DUMPED ${ADD}"
    fi
done

# restricted and unchecked tables
for T in ${CHECK_TABLES}; do
    if [ "${QUIET}" != "YES" ]; then
      printf "Dumping %-25s : " "${T}"
    fi
    if [ "${UNCHECKED}" == "YES" ]; then
        UNCHECKED_QUERY=""
    else
        UNCHECKED_QUERY="AND checked != -1"
    fi
    psql ${PG_OPT} -t -q -d "${DATABASE}" -c "\COPY (SELECT * FROM ${T} WHERE ${LIMIT} AND access_level >= ${LEVEL} ${UNCHECKED_QUERY}) TO '${DUMPDIR}/${T}.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV, ENCODING 'UTF-8');"
    ADD=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "SELECT count(*) FROM ${T} WHERE ${LIMIT}" | tr -d ' ' )
    if [ "${QUIET}" != "YES" ]; then
      echo "DUMPED ${ADD}"
    fi
done

# all done dumping database
tar zcf "${OUTPUT}/bety.tar.gz" -C "${DUMPDIR}" .
rm -rf "${DUMPDIR}"
