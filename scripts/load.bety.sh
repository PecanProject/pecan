#!/bin/bash

# ----------------------------------------------------------------------
# START CONFIGURATION SECTION
# ----------------------------------------------------------------------

# name of the dabase to load
# this script assumes the user running it has access to the database
DATABASE=${DATABASE:-"bety"}

# owner of the database
# also used to connect to the database for most operations
OWNER=${OWNER:-"bety"}

# psql options
# this allows you to add any other options
PG_OPT=${PG_OPT:-""}

# ID's used in database
# These ID's need to be unique for the sharing to work. If you want
# to share your data, send email to kooper@illinois.edu to claim
# your ID range. The master list is maintained at 
# https://github.com/PecanProject/bety/wiki/Distributed-BETYdb
#
#  0 - EBI           - David LeBauer
#  1 - BU            - Mike Dietze
#  2 - Brookhaven    - Shawn Serbin
#  3 - Purdue        -
#  4 - Virginia Tech - Quinn Thomas
# 99 - VM
MYSITE=${MYSITE:-99}
REMOTESITE=${REMOTESITE:-0}

# Create the database from scratch
# Set this to YES to create the database, this will remove all existing
# data!
CREATE=${CREATE:-"NO"}

# Keep the tmp folder even if the sync failed?
# Set this to YES to keep the tmp folder, this is helpful for
# debugging the script. The default value is NO and the tmp folder will
# be removed
KEEPTMP=${KEEPTMP:-"NO"}

# Convert user account 1 to carya for use on VM
# Set this to YES to convert user 1 to carya with password. This will
# give this user admin priviliges. It will also create 16 more users
# that have specific abilities.
if [ -z "${USERS}" ]; then
	if [ "${MYSITE}" -eq "99" ]; then
		USERS="YES"
	else
		USERS="NO"
	fi
fi

# ----------------------------------------------------------------------
# END CONFIGURATION SECTION
# ----------------------------------------------------------------------

# list of all tables, schema_migrations is ignored since that
# will be imported during creaton

# order is important to check constraints in database
CLEAN_TABLES="citations counties covariates cultivars"
CLEAN_TABLES="${CLEAN_TABLES} dbfiles ensembles entities formats"
CLEAN_TABLES="${CLEAN_TABLES} inputs likelihoods location_yields"
CLEAN_TABLES="${CLEAN_TABLES} machines managements methods"
CLEAN_TABLES="${CLEAN_TABLES} mimetypes models pfts "
CLEAN_TABLES="${CLEAN_TABLES} posterior_samples posteriors"
CLEAN_TABLES="${CLEAN_TABLES} priors runs sessions sites"
CLEAN_TABLES="${CLEAN_TABLES} species traits treatments"
CLEAN_TABLES="${CLEAN_TABLES} workflows yields"
CLEAN_TABLES="${CLEAN_TABLES} modeltypes modeltypes_formats"

# eventually these 2 should be loaded first to check constraints
CLEAN_TABLES="${CLEAN_TABLES} users variables"

MANY_TABLES="${MANY_TABLES} citations_sites citations_treatments"
MANY_TABLES="${MANY_TABLES} formats_variables inputs_runs"
MANY_TABLES="${MANY_TABLES} inputs_variables"
MANY_TABLES="${MANY_TABLES} managements_treatments pfts_priors"
MANY_TABLES="${MANY_TABLES} pfts_species posteriors_ensembles"

# list where to download data from. This data should come
# from the database. Same as mysite which should come from
# the database as well.
if [ -z "${DUMPURL}" ]; then
	if [ "${REMOTESITE}" == "0" ]; then
		DUMPURL="https://ebi-forecast.igb.illinois.edu/pecan/dump/bety.tar.gz"
	elif [ "${REMOTESITE}" == "1" ]; then
		DUMPURL="http://psql-pecan.bu.edu/sync/dump/bety.tar.gz"
	else
		echo "Don't know where to get data for site ${REMOTESITE}"
		exit
	fi
fi

# this value should be constant, do not change
ID_RANGE=1000000000

# before anything is done, check to make sure database exists
if ! psql -lqt | cut -d \| -f 1 | grep -w "${DATABASE}"; then
  echo "Database ${DATABASE} does not exist, please create it:"
  echo "(see https://github.com/PecanProject/pecan/wiki/Installing-PEcAn#installing-bety)"
  echo "  sudo -u postgres createuser -d -l -P -R -S bety"
  echo "  sudo -u postgres createdb -O bety ${DATABASE}"
  exit
fi

# make output folder
DUMPDIR="/tmp/$$"
mkdir "${DUMPDIR}"

# download dump file and unpack
curl -o "${DUMPDIR}/dump.tar.gz" "${DUMPURL}"
tar zxf "${DUMPDIR}/dump.tar.gz" -C "${DUMPDIR}"

# create database if need be, otherwise check version of schema
if [ "${CREATE}" == "YES" ]; then
	printf "Loading %-25s : " "schema"
  # create empty public schema
	psql -q -d "${DATABASE}" -c "DROP SCHEMA public CASCADE"
	psql -U ${OWNER} -q -d "${DATABASE}" -c "CREATE SCHEMA public"

  # following commands require superuser abilities
  psql -d "${DATABASE}" -c 'CREATE EXTENSION Postgis;'
  psql -d "${DATABASE}" -c "GRANT ALL ON ALL TABLES IN SCHEMA public TO ${OWNER};"

  # create rest of database
	psql ${PG_OPT} -U ${OWNER} -q -d "${DATABASE}" < "${DUMPDIR}"/*.schema
	echo "CREATED SCHEMA"

	printf "Loading  %-25s : " "schema_migrations"
	ADD=$( psql ${PG_OPT} -U ${OWNER} -t -q -d "${DATABASE}" -c "\COPY schema_migrations FROM '${DUMPDIR}/schema_migrations.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV, ENCODING 'UTF-8'); SELECT COUNT(*) FROM schema_migrations;" | tr -d ' ' )
	echo "ADDED ${ADD}"
else
	printf "Checking %-25s : " "schema"

	# find current schema version
	VERSION=$( psql ${PG_OPT} -U ${OWNER} -t -q -d "${DATABASE}" -c 'SELECT version FROM schema_migrations ORDER BY version DESC limit 1' | tr -d ' ' )

	if [ ! -e "${DUMPDIR}/${VERSION}.schema" ]; then
		echo "EXPECTED SCHEMA version ${VERSION}"
		echo "Dump is from a different schema, please fix schema in database."
    if [ "$KEEPTMP" == "YES" ]; then
		  echo "Files are in ${DUMPDIR}"
    else 
      rm -rf "${DUMPDIR}"
    fi
		exit
	fi

	echo "MATCHED SCHEMA version ${VERSION}"
fi

# compute range based on {MY,REMOTE}SITE
MY_START_ID=$(( MYSITE * ID_RANGE + 1 ))
MY_LAST_ID=$(( MY_START_ID + ID_RANGE - 1 ))
REM_START_ID=$(( REMOTESITE * ID_RANGE + 1 ))
REM_LAST_ID=$(( REM_START_ID + ID_RANGE - 1 ))

# clean tables
for T in ${CLEAN_TABLES}; do
	printf "Cleaning %-25s : " "${T}"
  WHERE="WHERE (id >= ${REM_START_ID} AND id <= ${REM_LAST_ID})"
	DEL=$( psql ${PG_OPT} -U ${OWNER} -t -q -d "${DATABASE}" -c "SELECT count(*) FROM ${T} ${WHERE}" | tr -d ' ' )
	psql ${PG_OPT} -U ${OWNER} -t -q -d "${DATABASE}" -c "DELETE FROM ${T} ${WHERE}"
	echo "DEL ${DEL}"
	printf "Loading  %-25s : " "${T}"
  START=$( psql ${PG_OPT} -U ${OWNER} -t -q -d "${DATABASE}" -c "SELECT COUNT(*) FROM ${T}" | tr -d ' ' )
	if [ -f "${DUMPDIR}/${T}.csv" ]; then
		psql ${PG_OPT} -U ${OWNER} -t -q -d "${DATABASE}" -c "\COPY ${T} FROM '${DUMPDIR}/${T}.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV, ENCODING 'UTF-8')"
	fi
  END=$( psql ${PG_OPT} -U ${OWNER} -t -q -d "${DATABASE}" -c "SELECT COUNT(*) FROM ${T}" | tr -d ' ' )
  ADD=$(( END - START ))
	echo "ADD ${ADD}"
	printf "Fixing   %-25s : " "${T}"
	NEXT=$( psql ${PG_OPT} -U ${OWNER} -t -q -d "${DATABASE}" -c "SELECT setval('${T}_id_seq', ${MY_START_ID}, false); SELECT setval('${T}_id_seq', (SELECT MAX(id) FROM ${T} WHERE id >= ${MY_START_ID} AND id < ${MY_LAST_ID}), true); SELECT last_value from ${T}_id_seq;" | tr -d ' ' )
	echo "SEQ ${NEXT}"
done

# hasmany relation ships
for T in ${MANY_TABLES}; do
	Z=(${T//_/ })
	X=${Z[0]}
	X=${X%s}
	Y=${Z[1]}
	Y=${Y%s}
	printf "Cleaning %-25s : " "${T}"
  WHERE="WHERE (${X}_id >= ${REM_START_ID} AND ${X}_id <= ${REM_LAST_ID} AND ${Y}_id >= ${REM_START_ID} AND ${Y}_id <= ${REM_LAST_ID})"
	DEL=$( psql ${PG_OPT} -U ${OWNER} -t -q -d "${DATABASE}" -c "SELECT count(*) FROM ${T} ${WHERE}" | tr -d ' ' )
	psql ${PG_OPT} -U ${OWNER} -t -q -d "${DATABASE}" -c "DELETE FROM ${T} ${WHERE}"
	echo "DEL ${DEL}"
	printf "Loading  %-25s : " "${T}"
  START=$( psql ${PG_OPT} -U ${OWNER} -t -q -d "${DATABASE}" -c "SELECT COUNT(*) FROM ${T}" | tr -d ' ' )
	if [ -f "${DUMPDIR}/${T}.csv" ]; then
		psql ${PG_OPT} -U ${OWNER} -t -q -d "${DATABASE}" -c "\COPY ${T} FROM '${DUMPDIR}/${T}.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV, ENCODING 'UTF-8')"
	fi
	END=$( psql ${PG_OPT} -U ${OWNER} -t -q -d "${DATABASE}" -c "SELECT COUNT(*) FROM ${T}" | tr -d ' ' )
  ADD=$(( END - START ))
	echo "ADD ${ADD}"
done

# convert user 1 if needed
if [ "${USERS}" == "YES" ]; then
  ID=2

  RESULT=$( psql ${PG_OPT} -U ${OWNER} -t -d "${DATABASE}" -c "SELECT count(id) FROM users WHERE login='carya';" )
  if [ ${RESULT} -eq 0 ]; then
    RESULT='UPDATE 0'
    while [ "${RESULT}" = "UPDATE 0" ]; do
      RESULT=$( psql ${PG_OPT} -U ${OWNER} -t -d "${DATABASE}" -c "UPDATE users SET login='carya', name='carya', crypted_password='df8428063fb28d75841d719e3447c3f416860bb7', salt='carya', access_level=1, page_access_level=1 WHERE id=${ID};" )
      ((ID++))
    done
  fi
  echo "Added carya with admin privileges"

  # set all users
  for f in 1 2 3 4; do
    for g in 1 2 3 4; do
      RESULT=$( psql ${PG_OPT} -U ${OWNER} -t -d "${DATABASE}" -c "SELECT count(id) FROM users WHERE login='carya${f}${g}';" )
      if [ ${RESULT} -eq 0 ]; then
        RESULT='UPDATE 0'
        while [ "${RESULT}" = "UPDATE 0" ]; do
          RESULT=$( psql ${PG_OPT} -U ${OWNER} -t -d "${DATABASE}" -c "UPDATE users SET login='carya${f}${g}', name='carya a-${f} p-${g}', crypted_password='df8428063fb28d75841d719e3447c3f416860bb7', salt='carya', access_level=${f}, page_access_level=${g} WHERE id=${ID};" )
          ((ID++))
        done
      fi
    done
  done
  echo "Updated users to have login='caryaXY' with appropriate privileges"
  echo "  (X=access_level, Y=page_access_level)."

  # add guest user
  RESULT=$( psql ${PG_OPT} -U ${OWNER} -t -d "${DATABASE}" -c "SELECT count(id) FROM users WHERE login='guestuser';" )
  if [ ${RESULT} -eq 0 ]; then
    RESULT='UPDATE 0'
    while [ "${RESULT}" = "UPDATE 0" ]; do
      RESULT=$( psql ${PG_OPT} -U ${OWNER} -t -d "${DATABASE}" -c "UPDATE users SET login='guestuser', name='guestuser', crypted_password='994363a949b6486fc7ea54bf40335127f5413318', salt='bety', access_level=4, page_access_level=4 WHERE id=${ID};" )
      ((ID++))
    done
  fi
  echo "Added guestuser with access_level=4 and page_access_level=4"
fi

# all done, cleanup
rm -rf "${DUMPDIR}"
