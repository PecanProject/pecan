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
# your ID range.
#
#  0 - EBI master database
#  1 - BU
#  2 - Brookhaven
# 99 - VM
MYSITE=${MYSITE:-99}

# access level requirement
# 0 - private
# 4 - public
LEVEL=${LEVEL:-3}

# dump unchecked traits and yields
# set this to "YES" to dump all unchecked traits/yields as well
UNCHECKED=${UNCHECKED:-"NO"}

# anonymous users
# set this to NO to dump all user information
ANONYMOUS=${ANONYMOUS:-"YES"}

# location where to write the results, this will be a tar file
OUTPUT=${OUTPUT:-"$PWD/dump"}

# ----------------------------------------------------------------------
# END CONFIGURATION SECTION
# ----------------------------------------------------------------------

# list of all tables, schema_migrations is ignored since that
# will be imported during creaton
CLEAN_TABLES="citations counties covariates cultivars dbfiles"
CLEAN_TABLES="${CLEAN_TABLES} ensembles entities formats inputs"
CLEAN_TABLES="${CLEAN_TABLES} likelihoods location_yields"
CLEAN_TABLES="${CLEAN_TABLES} machines managements methods"
CLEAN_TABLES="${CLEAN_TABLES} mimetypes models pfts "
CLEAN_TABLES="${CLEAN_TABLES} posterior_samples posteriors"
CLEAN_TABLES="${CLEAN_TABLES} priors runs sessions sites"
CLEAN_TABLES="${CLEAN_TABLES} species treatments"
CLEAN_TABLES="${CLEAN_TABLES} variables workflows"
CLEAN_TABLES="${CLEAN_TABLES} modeltypes modeltypes_formats"

USER_TABLES="users"
CHECK_TABLES="traits yields"

MANY_TABLES="${MANY_TABLES} citations_sites citations_treatments"
MANY_TABLES="${MANY_TABLES} formats_variables inputs_runs"
MANY_TABLES="${MANY_TABLES} inputs_variables"
MANY_TABLES="${MANY_TABLES} managements_treatments pfts_priors"
MANY_TABLES="${MANY_TABLES} pfts_species posteriors_ensembles"

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
START_ID=$(( MYSITE * ID_RANGE + 1 ))
LAST_ID=$(( START_ID + ID_RANGE - 1 ))
echo "Dumping all items that have id : [${START_ID} - ${LAST_ID}]"

# find current schema version
VERSION=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c 'SELECT version FROM schema_migrations ORDER BY version DESC limit 1' | tr -d ' ' )

# dump schema
printf "Dumping %-25s : " "schema"
pg_dump ${PG_OPT} -s "${DATABASE}" -O -x > "${DUMPDIR}/${VERSION}.schema"
echo "DUMPED version ${VERSION}"
echo "${VERSION}" > "${OUTPUT}/version.txt"

# dump ruby special table
printf "Dumping %-25s : " "schema_migrations"
ADD=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "SELECT count(*) FROM schema_migrations;" | tr -d ' ' )
psql ${PG_OPT} -t -q -d "${DATABASE}" -c "\COPY schema_migrations TO '${DUMPDIR}/schema_migrations.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV, ENCODING 'UTF-8')"
echo "DUMPED ${ADD}"

# skip following tables
# - inputs_runs (PEcAn, site specific)
# - posteriors_runs (PEcAn, site specific, is this used?)
# - runs (PEcAn, site specific)
# - workflows (PEcAn, site specific)

# dump users
printf "Dumping %-25s : " "users"
if [ "${ANONYMOUS}" == "NO" ]; then
	psql ${PG_OPT} -t -q -d "${DATABASE}" -c "\COPY (SELECT * FROM ${USER_TABLES} WHERE (id >= ${START_ID} AND id <= ${LAST_ID}))  TO '${DUMPDIR}/users.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV, ENCODING 'UTF-8')"
else
	psql ${PG_OPT} -t -q -d "${DATABASE}" -c "\COPY (SELECT id, CONCAT('user', id) AS login, CONCAT('user ' , id) AS name, CONCAT('betydb+', id, '@gmail.com') as email, 'Urbana' AS city,  'USA' AS country, NULL AS area, '*' AS crypted_password, 'BU' AS salt, NOW() AS created_at, NOW() AS updated_at, NULL as remember_token, NULL AS remember_token_expires_at, 3 AS access_level, 4 AS page_access_level, NULL AS apikey, 'IL' AS state_prov, '61801' AS postal_code FROM ${USER_TABLES} WHERE (id >= ${START_ID} AND id <= ${LAST_ID})) TO '${DUMPDIR}/users.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV, ENCODING 'UTF-8')"
fi
ADD=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "SELECT count(*) FROM ${USER_TABLES} WHERE (id >= ${START_ID} AND id <= ${LAST_ID});" | tr -d ' ' )
echo "DUMPED ${ADD}"

# unrestricted tables
for T in ${CLEAN_TABLES}; do
	printf "Dumping %-25s : " "${T}"
	psql ${PG_OPT} -t -q -d "${DATABASE}" -c "\COPY (SELECT * FROM ${T} WHERE (id >= ${START_ID} AND id <= ${LAST_ID})) TO '${DUMPDIR}/${T}.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV, ENCODING 'UTF-8')"
	ADD=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "SELECT count(*) FROM ${T} WHERE (id >= ${START_ID} AND id <= ${LAST_ID})" | tr -d ' ' )
	echo "DUMPED ${ADD}"
done

# restricted and unchecked tables
for T in ${CHECK_TABLES}; do
	printf "Dumping %-25s : " "${T}"
	if [ "${UNCHECKED}" == "YES" ]; then
		UNCHECKED_QUERY=""
	else
		UNCHECKED_QUERY="AND checked != -1"
	fi
	psql ${PG_OPT} -t -q -d "${DATABASE}" -c "\COPY (SELECT * FROM ${T} WHERE (id >= ${START_ID} AND id <= ${LAST_ID}) AND access_level >= ${LEVEL} ${UNCHECKED_QUERY}) TO '${DUMPDIR}/${T}.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV, ENCODING 'UTF-8');"
	ADD=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "SELECT count(*) FROM ${T} WHERE (id >= ${START_ID} AND id <= ${LAST_ID})" | tr -d ' ' )
	echo "DUMPED ${ADD}"
done

# hasmany relation ships
for T in ${MANY_TABLES}; do
	Z=(${T//_/ })
	X=${Z[0]}
	X=${X%s}
	Y=${Z[1]}
	Y=${Y%s}
	printf "Dumping %-25s : " "${T}"
	psql ${PG_OPT} -t -q -d "${DATABASE}" -c "\COPY (SELECT * FROM ${T} WHERE (${X}_id >= ${START_ID} AND ${X}_id <= ${LAST_ID} AND ${Y}_id >= ${START_ID} AND ${Y}_id <= ${LAST_ID})) TO '${DUMPDIR}/${T}.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV, ENCODING 'UTF-8');"
	ADD=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "SELECT count(*) FROM ${T} WHERE (${X}_id >= ${START_ID} AND ${X}_id <= ${LAST_ID} AND ${Y}_id >= ${START_ID} AND ${Y}_id <= ${LAST_ID})" | tr -d ' ' )
	echo "DUMPED ${ADD}"
done

# all done dumping database
tar zcf "${OUTPUT}/bety.tar.gz" -C "${DUMPDIR}" .
rm -rf "${DUMPDIR}"
