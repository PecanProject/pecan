#!/bin/bash

# exit on error
set -e

# ----------------------------------------------------------------------
# START CONFIGURATION SECTION
# ----------------------------------------------------------------------

# name of the dabase to reindexing
# this script assumes the user running it has access to the database
DATABASE=${DATABASE:-"bety"}

# The database catalog to search for mactching table names
DBCATALOG=${DBCATALOG:-"bety"}

# psql options
# this allows you to add the user to use as well as any other options
PG_OPT=${PG_OPT-"-U bety"}

# Should the process be quiet
QUIET=${QUIET:-"NO"}

# Should all the tables be indexed
TABLENAME=${TABLENAME:-"ALL"}

# Non-database-system tables that should always be ignored on a reindex
IGNORETABLES=${IGNORETABLES:-""}

# Skip reindexing the entire database after reindexing the tables
SKIPDATABASE=${SKIPDATABASE:-"YES"}

# ----------------------------------------------------------------------
# END CONFIGURATION SECTION
# ----------------------------------------------------------------------

# parse command line options
while getopts c:d:hi:p:qst: opt; do
  case $opt in
  c)
    DBCATALOG=$OPTARG
    ;;
  d)
    DATABASE=$OPTARG
    ;;
  h)
    echo "$0 [-c datalog] [-d database] [-h] [-i table names] [-p psql options] [-q] [-s] [-t tablename]"
    echo " -c catalog, database catalog name used to search for tables, default is bety"
    echo " -d database, default is bety"
    echo " -h this help page"
    echo " -i table names, list of space-separated table names to skip over when reindexing"
    echo " -p additional psql command line options, default is -U bety"
    echo " -q the reindexing should be quiet"
    echo " -s reindex the database after reindexing the tables (this should be done sparingly)"
    echo " -t tablename, the name of the one table to reindex"
    exit 0
    ;;
  i)
    # We add spaces to assist in exact table name maching
    IGNORETABLES=" ${OPTARG} "
    ;;
  p)
    PG_OPT=$OPTARG
    ;;
  q)
    QUIET="YES"
    ;;
  s)
    SKIPDATABASE="NO"
    ;;
  t)
    TABLENAME=$OPTARG
    SKIPDATABASE="YES"
    ;;
  esac
done

# be quiet if not interactive
if ! tty -s ; then
    exec 1>/dev/null
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
if [ "${QUIET}" != "YES" ]; then
  echo "Version: ${MIGRATIONS}  ${VERSION}  ${LATEST}"
  echo "Starting reindexing: ${NOW}"
fi

# Inform the caller of what we're doing
if [ "${QUIET}" != "YES" ]; then
  RUNINFO=""
  if [ "${TABLENAME}" == "ALL" ]; then
    RUNINFO="Reindexing all tables in catalog ${DBCATALOG}"
  else
    RUNINFO="Reindexing ${TABLENAME}"
  fi
  if [ "${SKIPDATABASE}" == "YES" ]; then
    RUNINFO="${RUNINFO}, skipping entire database"
  else
    RUNINFO="${RUNINFO}, reindexing entire database"
  fi
  echo "${RUNINFO}"
fi

# If we are reindexing all the tables, get the list of tables from the database
if [ "${TABLENAME}" == "ALL" ]; then
  TABLENAME=$(psql ${PG_OPT} -t -q -d "${DATABASE}" -c "with curuser as (select user) select t.table_name from information_schema.tables t join pg_catalog.pg_class c on (t.table_name = c.relname) join pg_catalog.pg_user u on (c.relowner = u.usesysid) join curuser cu on (u.usename = cu.current_user) where t.table_catalog='${DBCATALOG}' and t.table_schema='public' and t.table_type like '%TABLE%' order by t.table_name asc" | tr -d ' ')  
  if [ "${QUIET}" != "YES" ]; then
    printf "Reindexing all tables\n"
  fi
else
  if [ "${QUIET}" != "YES" ]; then
    printf "Reindexing %-40s\n" "${TABLENAME}"
  fi
fi

# Reindex the tables
for T in ${TABLENAME}; do
  if echo "${IGNORETABLES}" | grep -qi " ${T} "; then
    if [ "${QUIET}" != "YES" ]; then
      printf "Ignoring %-40s\n" "${T}"
    fi
  else
    if [ "${QUIET}" != "YES" ]; then
      printf "Reindex %-40s\n" "${T}"
    fi
    psql ${PG_OPT} -t -q -d "${DATABASE}" -c "REINDEX TABLE ${T}"
  fi
done

# Reindexing the overall database, should be fast(er) with all the tables already reindexed
if [ "${SKIPDATABASE}" == "NO" ]; then
  if [ "${QUIET}" != "YES" ]; then
    printf "Reindex entire database\n"
  fi
  psql  ${PG_OPT} -t -q -d "${DATABASE}" -c "REINDEX DATABASE ${DATABASE}"
fi

if [ "${QUIET}" != "YES" ]; then
  NOW=$( date -u +"%Y-%m-%dT%H:%M:%SZ" )
  echo "Completed reindexing: ${NOW}"
fi
