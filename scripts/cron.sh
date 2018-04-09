#!/bin/bash

# ----------------------------------------------------------------------
# START CONFIGURATION SECTION
# ----------------------------------------------------------------------

# name of the dabase to load
# this script assumes the user running it has access to the database
DATABASE=${DATABASE:-"bety"}

# hostname
FQDN=${FQDN:-"$(hostname -f)"}

# psql options
# this allows you to add any other options
PG_OPT=${PG_OPT:-""}

# ID's used in database, see load.bety.sh
# These ID's need to be unique for the sharing to work. If you want
MYSITE=${MYSITE:-""}

# location where to write the results, this will be a tar file
OUTPUT=${OUTPUT:-"$PWD/dump"}

# ----------------------------------------------------------------------
# END CONFIGURATION SECTION
# ----------------------------------------------------------------------

SCRIPTS_DIR="$( dirname $0 )"

# parse command line options
while getopts d:f:hm:o:p: opt; do
  case $opt in
  d)
    DATABASE="$OPTARG"
    ;;
  f)
    FQDN="$OPTARG"
    ;;
  h)
    echo "$0 [d database] [-f fqdn] [-h] [-m my siteid] [-o folder] [-p psql options]"
    echo " -d database, default is bety"
    echo " -f hostname, default is '$(hostname -f)'"
    echo " -h this help page"
    echo " -m site id, default is not set and will be guessed based on hostname"
    echo " -o output folder where dumped data is written, default is dump"
    echo " -p additional psql command line options, default is empty"
    exit 0
    ;;
  m)
    MYSITE="$OPTARG"
    ;;
  o)
    OUTPUT="$OPTARG"
    ;;
  p)
    PG_OPT="$OPTARG"
    ;;
  esac
done

# guess site
if [ "${MYSITE}" == "" ]; then
    MYSITE=$(sudo -u postgres psql ${PG_OPT} -d ${DATABASE} -t -c "SELECT sync_host_id FROM machines WHERE hostname='${FQDN}';")
fi
MYSITE=${MYSITE//[[:space:]]/}
if [ "${MYSITE}" == "" ]; then
    MYSITE="99"
fi

MY_START_ID=$(sudo -u postgres psql ${PG_OPT} -d ${DATABASE} -t -c "SELECT sync_start FROM machines WHERE sync_host_id=${MYSITE};")
MY_START_ID="${MY_START_ID//[[:space:]]/}"
MY_LAST_ID=$(sudo -u postgres psql -d bety -t -c "SELECT sync_end FROM machines WHERE sync_host_id=${MYSITE};")
MY_LAST_ID="${MY_LAST_ID//[[:space:]]/}"

if [ "${MY_START_ID}" == "" -o "${MY_LAST_ID}" == "" ]; then
    # this value should be constant, do not change
    ID_RANGE=1000000000
    MY_START_ID=$(( MYSITE * ID_RANGE + 1 ))
    MY_LAST_ID=$(( MY_START_ID + ID_RANGE - 1 ))
fi

# export variables for other scripts
export DATABASE FQDN MYSITE MY_START_ID MY_LAST_ID PG_OPT

# setup thredds catalog
"${SCRIPTS_DIR}/thredds.sh" > /home/carya/thredds/catalog.xml
service tomcat8 restart

# setup folder/files for database sync
mkdir -p ${OUTPUT}
chmod 777 ${OUTPUT}
touch ${OUTPUT}/sync.log
chmod 666 ${OUTPUT}/sync.log

# dump database
sudo -u postgres "${SCRIPTS_DIR}/dump.bety.sh" -q -m ${MYSITE} -o "${OUTPUT}"
if [ $? != 0 ]; then
  echo "Failed to run dump.bety.sh"
fi

# sync databases
for y in 0 1 2 5; do
    if [ $y -ne ${MYSITE} ]; then
        sudo -u postgres "${SCRIPTS_DIR}/load.bety.sh" -q -m ${MYSITE} -r $y -l "${OUTPUT}/sync.log"
        if [ $? != 0 ]; then
            echo "Failed to run load.bety.sh for site ${x}"
        fi
    fi
done
