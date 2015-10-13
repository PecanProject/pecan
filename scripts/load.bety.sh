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
#  3 - Purdue        - Jeanne Osnas
#  4 - Virginia Tech - Quinn Thomas
#  5 - Wisconsin     - Ankur Desai
# 99 - VM
MYSITE=${MYSITE:-99}
REMOTESITE=${REMOTESITE:-0}

# Create the database from scratch
# Set this to YES to create the database, this will remove all existing
# data!
CREATE=${CREATE:-"NO"}

# Fix the sequence numbers, this should only be need when creating a
# new database. Set this to YES to initialize the sequence numbers.
FIXSEQUENCE=${FIXSEQUENCE:-"NO"}

# Keep the tmp folder even if the sync failed?
# Set this to YES to keep the tmp folder, this is helpful for
# debugging the script. The default value is NO and the tmp folder will
# be removed
KEEPTMP=${KEEPTMP:-"NO"}

# Should the process be quiet
QUIET=${QUIET:-"NO"}

# Convert user account 1 to carya
# Set this to YES to convert user 1 to carya with password. This will
# give this user admin priviliges. It will also create 16 more users
# that have specific abilities.
USERS=${USERS:-"NO"}

# Log file
LOG=${LOG:-"$PWD/dump/sync.log"}

# ----------------------------------------------------------------------
# END CONFIGURATION SECTION
# ----------------------------------------------------------------------

# parse command line options
while getopts c:d:f:hl:m:o:p:qr:t:u: opt; do
  case $opt in
  c)
    CREATE=$OPTARG
    ;;
  d)
    DATABASE=$OPTARG
    ;;
  f)
    FIXSEQUENCE=$OPTARG
    ;;
  h)
    echo "$0 [-c YES|NO] [-d database] [-f YES|NO] [-h] [-m my siteid] [-o owner] [-p psql options] [-r remote siteid] [-t YES|NO] [-u YES|NO]"
    echo " -c create database, THIS WILL ERASE THE CURRENT DATABASE, default is NO"
    echo " -d database, default is bety"
    echo " -f fix sequence numbers, this should not be needed, default is NO"
    echo " -h this help page"
    echo " -l location of log file (place this with the dump files)"
    echo " -m site id, default is 99 (VM)"
    echo " -o owner of the database, default is bety"
    echo " -p additional psql command line options, default is empty"
    echo " -q should the import be quiet"
    echo " -r remote site id, default is 0 (EBI)"
    echo " -t keep temp folder, default is NO"
    echo " -u create carya users, this will create some default users"
    exit 0
    ;;
  l)
    LOG=$OPTARG
    ;;
  m)
    MYSITE=$OPTARG
    ;;
  o)
    OWNER=$OPTARG
    ;;
  p)
    PG_OPT=$OPTARG
    ;;
  q)
    QUIET=YES
    ;;
  r)
    REMOTESITE=$OPTARG
    ;;
  t)
    KEEPTMP=$OPTARG
    ;;
  u)
    USERS=$OPTARG
    ;;
  esac
done

# simple sanity check
if [ "${CREATE}" == "YES" -a "${OWNER}" == "" ]; then
  echo "Can not create database without owner"
  exit 1
fi
if [ "${MYSITE}" == "${REMOTESITE}" ]; then
  echo "Can not have same remotesite as mysite"
  exit 1
fi
if [ "${CREATE}" == "YES" ]; then
  FIXSEQUENCE="YES"
fi

# list of all tables, schema_migrations is ignored since that
# will be imported during creaton

# list of tables that are one to many relationships
CLEAN_TABLES="citations covariates cultivars"
CLEAN_TABLES="${CLEAN_TABLES} ensembles entities formats"
CLEAN_TABLES="${CLEAN_TABLES} inputs likelihoods"
CLEAN_TABLES="${CLEAN_TABLES} machines managements methods"
CLEAN_TABLES="${CLEAN_TABLES} mimetypes models"
CLEAN_TABLES="${CLEAN_TABLES} modeltypes modeltypes_formats"
CLEAN_TABLES="${CLEAN_TABLES} pfts posterior_samples posteriors"
CLEAN_TABLES="${CLEAN_TABLES} priors runs sessions sites"
CLEAN_TABLES="${CLEAN_TABLES} species treatments"
CLEAN_TABLES="${CLEAN_TABLES} variables workflows"
CLEAN_TABLES="${CLEAN_TABLES} traits yields"
CLEAN_TABLES="${CLEAN_TABLES} dbfiles users"

# list of tables that are many to many relationships
MANY_TABLES="${MANY_TABLES} citations_sites citations_treatments"
MANY_TABLES="${MANY_TABLES} formats_variables inputs_runs"
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
  elif [ "${REMOTESITE}" == "2" ]; then
    DUMPURL="https://www.dropbox.com/s/wr8sldv080wa9y8/bety.tar.gz?dl=0"
  elif [ "${REMOTESITE}" == "5" ]; then  
    DUMPURL="http://tree.aos.wisc.edu:6480/sync/dump/bety.tar.gz"
  else
    echo "Don't know where to get data for site ${REMOTESITE}"
    exit 1
  fi
fi

# this value should be constant, do not change
ID_RANGE=1000000000

# before anything is done, check to make sure database exists
if ! psql -lqt | cut -d \| -f 1 | grep -w "${DATABASE}" > /dev/null ; then
  echo "Database ${DATABASE} does not exist, please create it:"
  echo "(see https://github.com/PecanProject/pecan/wiki/Installing-PEcAn#installing-bety)"
  echo "  sudo -u postgres createuser -d -l -P -R -S bety"
  echo "  sudo -u postgres createdb -O bety ${DATABASE}"
  exit 1
fi

# make output folder
DUMPDIR="/tmp/$$"
mkdir "${DUMPDIR}"

# download dump file and unpack
curl -s -L -o "${DUMPDIR}/dump.tar.gz" "${DUMPURL}"
tar zxf "${DUMPDIR}/dump.tar.gz" -C "${DUMPDIR}"

# create database if need be, otherwise check version of schema
if [ "${CREATE}" == "YES" ]; then
  if [ "${QUIET}" != "YES" ]; then
     printf "Loading %-25s : " "schema"
  fi
  # create empty public schema
  psql -q -d "${DATABASE}" -c "DROP SCHEMA public CASCADE"
  psql -U ${OWNER} -q -d "${DATABASE}" -c "CREATE SCHEMA public"

  # following commands require superuser abilities
  psql -d "${DATABASE}" -c 'CREATE EXTENSION Postgis;'
  psql -d "${DATABASE}" -c "GRANT ALL ON ALL TABLES IN SCHEMA public TO ${OWNER};"

  # create rest of database
  psql ${PG_OPT} -U ${OWNER} -q -d "${DATABASE}" < "${DUMPDIR}"/*.schema
  if [ "${QUIET}" != "YES" ]; then
    echo "CREATED SCHEMA"
  fi

  if [ "${QUIET}" != "YES" ]; then
    printf "Loading  %-25s : " "schema_migrations"
  fi
  ADD=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c "\COPY schema_migrations FROM '${DUMPDIR}/schema_migrations.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV, ENCODING 'UTF-8'); SELECT COUNT(*) FROM schema_migrations;" | tr -d ' ' )
  if [ "${QUIET}" != "YES" ]; then
    echo "ADDED ${ADD}"
  fi
else
  if [ "${QUIET}" != "YES" ]; then
    printf "Checking %-25s : " "schema"
  fi

  # find current schema version
  VERSION=$( psql ${PG_OPT} -t -q -d "${DATABASE}" -c 'SELECT md5(array_agg(version)::text) FROM (SELECT version FROM schema_migrations ORDER BY version) as v;' | tr -d ' ' )

  if [ ! -e "${DUMPDIR}/${VERSION}.schema" ]; then
    echo "EXPECTED SCHEMA version ${VERSION}"
    echo "Dump is from a different schema, please fix schema in database."
    if [ "$KEEPTMP" == "YES" ]; then
      echo "Files are in ${DUMPDIR}"
    else
      rm -rf "${DUMPDIR}"
    fi
    if [ -e ${LOG} ]; then
      echo `date -u` $REMOTESITE 1 >> $LOG
    fi
    exit 1
  fi

  if [ "${QUIET}" != "YES" ]; then
    echo "MATCHED SCHEMA version ${VERSION}"
  fi
fi

# compute range based on {MY,REMOTE}SITE
MY_START_ID=$(( MYSITE * ID_RANGE + 1 ))
MY_LAST_ID=$(( MY_START_ID + ID_RANGE - 1 ))
REM_START_ID=$(( REMOTESITE * ID_RANGE + 1 ))
REM_LAST_ID=$(( REM_START_ID + ID_RANGE - 1 ))

# common statement pieces used
REM_WHERE="WHERE (id >= ${REM_START_ID} AND id <= ${REM_LAST_ID})"
MY_WHERE="WHERE (id >= ${MY_START_ID} AND id <= ${MY_LAST_ID})"

# create psql process that will be used for all code
PSQL_PIPE_INP=/tmp/psql_inp.$$
PSQL_PIPE_OUT=/tmp/psql_out.$$
mkfifo -m 600 $PSQL_PIPE_INP
mkfifo -m 600 $PSQL_PIPE_OUT
psql ${PG_OPT} --quiet --no-align --no-readline --tuples-only -P footer=off --dbname ${DATABASE} <$PSQL_PIPE_INP >$PSQL_PIPE_OUT &
exec 3>$PSQL_PIPE_INP
exec 4<$PSQL_PIPE_OUT
PSQL_PID=$!
if [ "${QUIET}" != "YES" ]; then
  echo "Started psql (pid=$PSQL_PID)"
fi

# capture EXIT so we can rollback if needed, as well as cleanup
trap '
  if ps -p $PSQL_PID > /dev/null ; then
    echo "Process killed, no changes are made to the database."
    echo "ROLLBACK;" >&3
    kill $PSQL_PID
    cat <&4
    if [ -e ${LOG} ]; then
      echo `date -u` $REMOTESITE 2 >> $LOG
    fi
  fi
  rm -f $PSQL_PIPE_INP $PSQL_PIPE_OUT
' EXIT

# start transaction
echo "BEGIN;" >&3

# for all tables
# 1) disable constraints on this table
# 2) remove all rows that have id in range of remote site
# 3) load new data
# 4) set last inserted item in my range
# 5) enable constraints on this table
for T in ${CLEAN_TABLES} ${MANY_TABLES}; do
  echo "ALTER TABLE ${T} DISABLE TRIGGER ALL;" >&3
  echo "SELECT count(*) FROM ${T} ${REM_WHERE};" >&3 && read DEL <&4
  # TODO what is last index in range we are adding, this will give a better
  #      indication if rows are added.
  echo "DELETE FROM ${T} ${REM_WHERE};" >&3
  echo "SELECT COUNT(*) FROM ${T};" >&3 && read START <&4
  if [ -f "${DUMPDIR}/${T}.csv" ]; then
    echo "\COPY ${T} FROM '${DUMPDIR}/${T}.csv' WITH (DELIMITER '	',  NULL '\\N', ESCAPE '\\', FORMAT CSV, ENCODING 'UTF-8')" >&3
  fi
  echo "SELECT COUNT(*) FROM ${T};" >&3 && read END <&4
  ADD=$(( END - START ))
  DIFF=$(( ADD - DEL ))
  if [ "${QUIET}" != "YES" ]; then
    if [ "$DEL" != "0" -o "$ADD" != "0" ]; then
      if [ "$DIFF" != "0" ]; then
        printf "Updated  %-25s : %6d (%+d)\n" "${T}" ${ADD} ${DIFF}
      else
        printf "Updated  %-25s : %6d\n" "${T}" ${ADD}
      fi
    fi
  fi
  echo "ALTER TABLE ${T} ENABLE TRIGGER ALL;" >&3
done

# convert user 1 if needed
if [ "${USERS}" == "YES" -a "${REMOTESITE}" == "0" ]; then
  ID=2

  echo "SELECT count(id) FROM users WHERE login='carya';" >&3 && read RESULT <&4
  while [ ${RESULT} -eq 0 ]; do
    echo "UPDATE users SET login='carya', name='carya', crypted_password='df8428063fb28d75841d719e3447c3f416860bb7', salt='carya', access_level=1, page_access_level=1 WHERE id=${ID};" >&3
    ((ID++))
    echo "SELECT count(id) FROM users WHERE login='carya';" >&3 && read RESULT <&4
  done
  if [ "${QUIET}" != "YES" ]; then
    echo "Added carya with admin privileges"
  fi

  # set all users
  for f in 1 2 3 4; do
    for g in 1 2 3 4; do
      echo "SELECT count(id) FROM users WHERE login='carya${f}${g}';" >&3 && read RESULT <&4
      while [ ${RESULT} -eq 0 ]; do
        echo "UPDATE users SET login='carya${f}${g}', name='carya a-${f} p-${g}', crypted_password='df8428063fb28d75841d719e3447c3f416860bb7', salt='carya', access_level=${f}, page_access_level=${g} WHERE id=${ID};" >&3
        ((ID++))
        echo "SELECT count(id) FROM users WHERE login='carya${f}${g}';" >&3 && read RESULT <&4
      done
    done
  done
  if [ "${QUIET}" != "YES" ]; then
    echo "Updated users to have login='caryaXY' with appropriate privileges"
    echo "  (X=access_level, Y=page_access_level)."
  fi

  # add guest user
  echo "SELECT count(id) FROM users WHERE login='guestuser';" >&3 && read RESULT <&4
  while [ ${RESULT} -eq 0 ]; do
    echo "UPDATE users SET login='guestuser', name='guestuser', crypted_password='994363a949b6486fc7ea54bf40335127f5413318', salt='bety', access_level=4, page_access_level=4 WHERE id=${ID};" >&3
    ((ID++))
    echo "SELECT count(id) FROM users WHERE login='guestuser';" >&3 && read RESULT <&4
  done
  if [ "${QUIET}" != "YES" ]; then
    echo "Added guestuser with access_level=4 and page_access_level=4"
  fi
fi

# fix sequence numbers if needed
if [ "${FIXSEQUENCE}" == "YES" ]; then
  for T in ${CLEAN_TABLES} ${MANY_TABLES}; do
    echo "SELECT last_value from ${T}_id_seq;" >&3 && read OLD <&4
    echo "SELECT setval('${T}_id_seq', ${MY_START_ID}, false);" >&3 && read IGN <&4
    echo "SELECT setval('${T}_id_seq', (SELECT MAX(id) FROM ${T} ${MY_WHERE}), true);" >&3 && read IGN <&4
    echo "SELECT last_value from ${T}_id_seq;" >&3 && read NEXT <&4
    if [ "${QUIET}" != "YES" ]; then
      if [ "$OLD" != "$NEXT" ]; then
        printf "Fixed    %-25s : %s\n" "${T}" "${NEXT}"
      fi
    fi
  done
fi

# close transaction
if [ -e ${LOG} ]; then
  echo `date -u` $REMOTESITE 0 >> $LOG
fi
echo "END;" >&3
echo "\quit" >&3
wait $PSQL_PID

# all done, cleanup
rm -rf "${DUMPDIR}"
