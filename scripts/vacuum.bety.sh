#!/bin/bash

# exit on error
set -e

# ----------------------------------------------------------------------
# START CONFIGURATION SECTION
# ----------------------------------------------------------------------

# name of the dabase to vacuum
# this script assumes the user running it has access to the database
DATABASE=${DATABASE:-"bety"}

# The database catalog to search for mactching table names
DBCATALOG=${DBCATALOG:-"bety"}

# Perform a full vacuum which returns resources to the system
FULLVACUUM=${FULLVACUUM:-"NO"}

# psql options
# this allows you to add the user to use as well as any other options
PG_OPT=${PG_OPT-"-U bety"}

# Should the process be quiet
QUIET=${QUIET:-"NO"}

# Should all the tables be vacuumed
TABLENAME=${TABLENAME:-"ALL"}

# Whether the tables should be analyzed
ANALYZETABLES=${ANALYZETABLES:-"YES"}

# Only analyze the tables, don't perform a regular vacuum
ANALYZEONLY=${ANALYZEONLY:-"NO"}

# Non-database-system tables that should always be ignored on a vacuum
IGNORETABLES=${IGNORETABLES:-""}

# Skip vacuuming the entire database after vacuuming the tables
SKIPDATABASE=${SKIPDATABASE:-"NO"}

# ----------------------------------------------------------------------
# END CONFIGURATION SECTION
# ----------------------------------------------------------------------

# parse command line options
while getopts c:d:fhi:np:qst:z opt; do
  case $opt in
  c)
    DBCATALOG=$OPTARG
    ;;
  d)
    DATABASE=$OPTARG
    ;;
  f)
    if [ "${ANALYZEONLY}" == "NO" ]; then
      FULLVACUUM="YES"
    fi
    ;;
  h)
    echo "$0 [-c datalog] [-d database] [-f] [-h] [-i table names] [-n] [-p psql options] [-q] [-s] [-t tablename] [-z]"
    echo " -c catalog, database catalog name used to search for tables, default is bety"
    echo " -d database, default is bety"
    echo " -f perform a full vacuum to return resources to the system. Specify rarely, if ever"
    echo " -h this help page"
    echo " -i table names, list of space-separated table names to skip over when vacuuming"
    echo " -n only vacuum the tables and do not analyze, default is to first vacuum and then analyze"
    echo " -p additional psql command line options, default is -U bety"
    echo " -q the vacuum should be quiet"
    echo " -s skip vacuuming the database after vacuuming the tables"
    echo " -t tablename, the name of the one table to vacuum"
    echo " -z only perform analyze, do not perform a regular vacuum, overrides -n and -f, sets -s"
    exit 0
    ;;
  i)
    # We add spaces to assist in exact table name maching
    IGNORETABLES=" ${OPTARG} "
    ;;
  n)
    if [ "${ANALYZEONLY}" == "NO" ]; then
      ANALYZETABLES="NO"
    fi
    ;;
  p)
    PG_OPT=$OPTARG
    ;;
  q)
    QUIET="YES"
    ;;
  s)
    SKIPDATABASE="YES"
    ;;
  t)
    TABLENAME=$OPTARG
    SKIPDATABASE="YES"
    ;;
  z)
    ANALYZEONLY="YES"
    ANALYZETABLES="YES"
    SKIPDATABASE="YES"
    FULLVACUUM="NO"
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
  echo "Starting vacuum: ${NOW}"
fi

# Inform the caller of what we're doing
if [ "${QUIET}" != "YES" ]; then
  RUNINFO=""
  if [ "${TABLENAME}" == "ALL" ]; then
    RUNINFO="Vacuuming all tables in catalog ${DBCATALOG}"
  else
    RUNINFO="Vacuuming ${TABLENAME}"
  fi
  if [ "${ANALYZETABLES}" == "YES" ]; then
    RUNINFO="${RUNINFO}, with analysis"
  else
    RUNINFO="${RUNINFO}, skipping analysis"
  fi
  if [ "${SKIPDATABASE}" == "YES" ]; then
    RUNINFO="${RUNINFO}, skipping entire database"
  fi
  if [ "${FULLVACUUM}" == "YES" ]; then
    RUNINFO="${RUNINFO}, with the FULL option set (this may take a long time)"
  fi
  echo "${RUNINFO}"
fi

# If we are vacuuming all the tables, get the list of tables from the database
if [ "${TABLENAME}" == "ALL" ]; then
  TABLENAME=$(psql ${PG_OPT} -t -q -d "${DATABASE}" -c "select table_name from information_schema.tables where table_catalog='${DBCATALOG}' and table_schema='public' and table_type like '%TABLE%' order by table_name asc" | tr -d ' ')  
  if [ "${QUIET}" != "YES" ]; then
    printf "Vacuuming all tables\n"
  fi
else
  if [ "${QUIET}" != "YES" ]; then
    printf "Vacuuming %-40s\n" "${TABLENAME}"
  fi
fi

# Vacuum the tables
FULLOPTION=""
if [ "${FULLVACUUM}" == "YES" ]; then
  FULLOPTION="FULL"
fi
for T in ${TABLENAME}; do
  if echo "${IGNORETABLES}" | grep -qi " ${T} "; then
    if [ "${QUIET}" != "YES" ]; then
      printf "Ignoring %-40s\n" "${T}"
    fi
  else
    if [ "${ANALYZEONLY}" == "NO" ]; then
      if [ "${QUIET}" != "YES" ]; then
        printf "Vacuum %s%-40s\n" "${FULLOPTION} " "${T}"
      fi
      psql ${PG_OPT} -t -q -d "${DATABASE}" -c "VACUUM ${FULLOPTION} ${T}"
    fi
    if [ "${ANALYZETABLES}" == "YES" ]; then
      if [ "${QUIET}" != "YES" ]; then
        printf "Vacuum analyze %-40s\n" "${T}"
      fi
      psql ${PG_OPT} -t -q -d "${DATABASE}" -c "VACUUM ANALYZE ${T}"
    fi
  fi
done

# Vacuum the overall database, should be fast(er) with all the tables already vacuumed
if [ "${SKIPDATABASE}" == "NO" ]; then
  if [ "${ANALYZEONLY}" == "NO" ]; then
    if [ "${QUIET}" != "YES" ]; then
      printf "Vacuum %sdatabase\n" "${FULLOPTION} "
    fi
    psql  ${PG_OPT} -t -q -d "${DATABASE}" -c "VACUUM ${FULLOPTION}"
  fi
  if [ "${ANALYZETABLES}" == "YES" ]; then
    if [ "${QUIET}" != "YES" ]; then
      printf "Vacuum analyze database\n"
    fi
    psql ${PG_OPT} -t -q -d "${DATABASE}" -c "VACUUM ANALYZE"
  fi
fi

if [ "${QUIET}" != "YES" ]; then
  NOW=$( date -u +"%Y-%m-%dT%H:%M:%SZ" )
  echo "Completed vacuum: ${NOW}"
fi
