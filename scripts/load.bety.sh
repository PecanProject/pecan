#!/bin/bash

# ----------------------------------------------------------------------
# START CONFIGURATION SECTION
# ----------------------------------------------------------------------

# name of the dabase to load
# this script assumes the user running it has access to the database
DATABASE=bety

# psql options
# this allows you to add the user to use as well as any other options
PG_OPT="-U bety"

# ID's used in database
# These ID's need to be unique for the sharing to work. If you want
# to share your data, send email to kooper@illinois.edu to claim
# your ID range.
#
#  0 - EBI master database
#  1 - BU
# 99 - VM
MYSITE=99
REMOTESITE=0

# url to get data from
DUMPURL="https://ebi-forecast.igb.illinois.edu/pecan/pecanweb.0.tar.gz"
DUMPURL="file:///home/carya/bety.0.tar.gz"

# Create the database from scratch
# Set this to YES to create the database, this will remove all existing
# data!
CREATE="YES"

# ----------------------------------------------------------------------
# END CONFIGURATION SECTION
# ----------------------------------------------------------------------

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
	sudo -u postgres dropdb "${DATABASE}"
	sudo -u postgres createdb "${DATABASE}" -O bety
	psql ${PG_OPT} -q -d "${DATABASE}" < "${DUMPDIR}"/*.schema
	echo "CREATED SCHEMA"

	printf "Loading  %-25s : " "schema_migrations"
	ADD=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "\COPY schema_migrations FROM '${DUMPDIR}/schema_migrations.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV); SELECT COUNT(*) FROM schema_migrations;" | tr -d ' ' )
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
MY_LAST_ID=$(( START_ID + ID_RANGE - 1 ))
REM_START_ID=$(( REMOTESITE * ID_RANGE + 1 ))
REM_LAST_ID=$(( START_ID + ID_RANGE - 1 ))

# clean tables
for T in users citations counties covariates cultivars dbfiles ensembles entities formats likelihoods location_yields machines managements methods mimetypes models pfts posteriors priors sessions sites species treatments variables inputs traits yields; do
	printf "Cleaning %-25s : " "${T}"
	DEL=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "SELECT count(*) FROM ${T} WHERE (id >= ${REM_START_ID} AND id <= ${REM_LAST_ID})" | tr -d ' ' )
	psql ${PG_OPT} -t -q -d "${DATABASE}" -c "DELETE FROM ${T} WHERE (id >= ${REM_START_ID} AND id <= ${REM_LAST_ID})"
	echo "DEL ${DEL}"
	printf "Loading  %-25s : " "${T}"
	psql ${PG_OPT} -t -q -d "${DATABASE}" -c "\COPY ${T} FROM '${DUMPDIR}/${T}.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV)"
	ADD=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "SELECT COUNT(*) FROM ${T} WHERE (id >= ${REM_START_ID} AND id <= ${REM_LAST_ID});" | tr -d ' ' )
	echo "ADD ${ADD}"
	printf "Fixing   %-25s : " "${T}"
	NEXT=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "SELECT setval('${T}_id_seq', ${MY_START_ID}, false); SELECT setval('${T}_id_seq', (SELECT MAX(id) FROM ${T} WHERE id >= ${MY_START_ID} AND id < ${MY_LAST_ID}), true); SELECT last_value from ${T}_id_seq;" | tr -d ' ' )
	echo "SEQ ${NEXT}"
done

# hasmany relation ships
for T in citations_sites citations_treatments formats_variables inputs_variables managements_treatments pfts_priors pfts_species; do
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
	psql ${PG_OPT} -t -q -d "${DATABASE}" -c "\COPY ${T} FROM '${DUMPDIR}/${T}.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV)"
	ADD=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "SELECT COUNT(*) FROM ${T} WHERE (${X}_id >= ${REM_START_ID} AND ${X}_id <= ${REM_LAST_ID} AND ${Y}_id >= ${REM_START_ID} AND ${Y}_id <= ${REM_LAST_ID})" | tr -d ' ' )
	echo "ADD ${ADD}"
done

# all done, cleanup
if [ "${CREATE}" == "YES" ]; then
	echo "Don't forget to run rake db:migrate"
fi
rm -rf "${DUMPDIR}"