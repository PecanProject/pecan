#!/bin/bash

# ----------------------------------------------------------------------
# START CONFIGURATION SECTION
# ----------------------------------------------------------------------

# name of the dabase to load
# this script assumes the user running it has access to the database
DATABASE=${DATABASE:-"bety"}

# owner of the database
# also use to connect to the database
OWNER="bety"

# psql options
# this allows you to add the user to use as well as any other options
PG_OPT=${PG_OPT-"-U $OWNER"}

# ID's used in database
# These ID's need to be unique for the sharing to work. If you want
# to share your data, send email to kooper@illinois.edu to claim
# your ID range.
#
#  0 - EBI master database
#  1 - BU
#  2 - Brookhaven
# 99 - VM
MYSITE=${MYSITE:-99}
REMOTESITE=${REMOTESITE:-0}

# Create the database from scratch
# Set this to YES to create the database, this will remove all existing
# data!
CREATE=${CREATE:-"NO"}

# Convert user account 1 to carya for use on VM
# Set this to YES to conver user 1 to carya with password. This will
# give this user admin priviliges
if [ -z "${ADMIN}" ]; then
	if [ "${MYSITE}" -eq "99" ]; then
		ADMIN="YES"
	else
		ADMIN="NO"
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
		DUMPURL="http://psql-pecan.bu.edu/sync/bety.tar.gz"
	else
		echo "Don't know where to get data for site ${REMOTESITE}"
		exit
	fi
fi

# this value should be constant, do not change
ID_RANGE=1000000000

# make output folder
DUMPDIR="/tmp/$$"
mkdir "${DUMPDIR}"

# download dump file and unpack
curl -o "${DUMPDIR}/dump.tar.gz" "${DUMPURL}"
tar zxf "${DUMPDIR}/dump.tar.gz" -C "${DUMPDIR}"

# create database if need be, otherwise check version of schema
if [ "${CREATE}" == "YES" ]; then
	printf "Loading %-25s : " "schema"
	psql ${PG_OPT} -q -d "${DATABASE}" -c "DROP SCHEMA public CASCADE"
	psql ${PG_OPT} -q -d "${DATABASE}" -c "CREATE SCHEMA public"
	psql ${PG_OPT} -q -d "${DATABASE}" < "${DUMPDIR}"/*.schema
	echo "CREATED SCHEMA"

	printf "Loading  %-25s : " "schema_migrations"
	ADD=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "\COPY schema_migrations FROM '${DUMPDIR}/schema_migrations.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV, ENCODING 'UTF-8'); SELECT COUNT(*) FROM schema_migrations;" | tr -d ' ' )
	echo "ADDED ${ADD}"
else
	printf "Checking %-25s : " "schema"

	# find current schema version
	VERSION=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c 'SELECT version FROM schema_migrations ORDER BY version DESC limit 1' | tr -d ' ' )

	if [ ! -e "${DUMPDIR}/${VERSION}.schema" ]; then
		echo "EXPECTED SCHEMA version ${VERSION}"
		echo "Dump is from a different schema, please fix schema in database."
		echo "Files are in ${DUMPDIR}"
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
	DEL=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "SELECT count(*) FROM ${T} WHERE (id >= ${REM_START_ID} AND id <= ${REM_LAST_ID})" | tr -d ' ' )
	psql ${PG_OPT} -t -q -d "${DATABASE}" -c "DELETE FROM ${T} WHERE (id >= ${REM_START_ID} AND id <= ${REM_LAST_ID})"
	echo "DEL ${DEL}"
	printf "Loading  %-25s : " "${T}"
	if [ -f "${DUMPDIR}/${T}.csv" ]; then
		psql ${PG_OPT} -t -q -d "${DATABASE}" -c "\COPY ${T} FROM '${DUMPDIR}/${T}.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV, ENCODING 'UTF-8')"
	fi
	ADD=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "SELECT COUNT(*) FROM ${T} WHERE (id >= ${REM_START_ID} AND id <= ${REM_LAST_ID});" | tr -d ' ' )
	echo "ADD ${ADD}"
	printf "Fixing   %-25s : " "${T}"
	NEXT=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "SELECT setval('${T}_id_seq', ${MY_START_ID}, false); SELECT setval('${T}_id_seq', (SELECT MAX(id) FROM ${T} WHERE id >= ${MY_START_ID} AND id < ${MY_LAST_ID}), true); SELECT last_value from ${T}_id_seq;" | tr -d ' ' )
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
	DEL=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "SELECT count(*) FROM ${T} WHERE (${X}_id >= ${REM_START_ID} AND ${X}_id <= ${REM_LAST_ID} AND ${Y}_id >= ${REM_START_ID} AND ${Y}_id <= ${REM_LAST_ID})" | tr -d ' ' )
	psql ${PG_OPT} -t -q -d "${DATABASE}" -c "DELETE FROM ${T} WHERE (${X}_id >= ${REM_START_ID} AND ${X}_id <= ${REM_LAST_ID} AND ${Y}_id >= ${REM_START_ID} AND ${Y}_id <= ${REM_LAST_ID})"
	echo "DEL ${DEL}"
	printf "Loading  %-25s : " "${T}"
	if [ -f "${DUMPDIR}/${T}.csv" ]; then
		psql ${PG_OPT} -t -q -d "${DATABASE}" -c "\COPY ${T} FROM '${DUMPDIR}/${T}.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV, ENCODING 'UTF-8')"
	fi
	ADD=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "SELECT COUNT(*) FROM ${T} WHERE (${X}_id >= ${REM_START_ID} AND ${X}_id <= ${REM_LAST_ID} AND ${Y}_id >= ${REM_START_ID} AND ${Y}_id <= ${REM_LAST_ID})" | tr -d ' ' )
	echo "ADD ${ADD}"
done

# convert user 1 if needed
if [ "${ADMIN}" == "YES" ]; then
	psql ${PG_OPT} -t -q -d "${DATABASE}" -c "UPDATE users SET login='carya', crypted_password='df8428063fb28d75841d719e3447c3f416860bb7', salt='carya', access_level=1, page_access_level=1 WHERE id=1;"
	echo "User 1 now has admin priviliges"
fi

# all done, cleanup
rm -rf "${DUMPDIR}"
