#!/bin/bash

# simple script to creat backups of the bety database. This script will
# create a copy of the datbase daily, weekly, monthly and yearly. The
# files will be called:
# - bety-h-X, hourly backup, where X is the hour of the day (24 hour time).
# - bety-d-X, daily backup, where X is the day of the month.
# - bety-w-X, weekly backup, where X is the week number in the year
# - bety-m-X, montly backup, where X is the month of the year
# - bety-y-X, yearly backup, where X is the actual year.

# Variables to use for the database dump
DATABASE=bety
BETYUSER=bety

# Location where backup should be written
BACKUPDIR=$( dirname $0 )/backup

# Set path if needed
#export PATH=<location to postgresql>/bin;${PATH}

# Some handy variables
HOUR=$( date +"%H" )
TODAY=$( date +"%d" )
YESTERDAY=$( date -d "yesterday" +"%d" )
TOMORROW=$( date -d "tomorrow" +"%d" )
DOW=$( date +"%u" )
WEEK=$( date +"%W" )
MONTH=$( date +"%m" )
YEAR=$( date +"%Y" )

# Psql options
# This allows you to add any other options
PG_OPT=${PG_OPT:-""}
HOURLY=${HOURLY:-""}

# Parse command line options
while getopts ho:p:H opt; do
  case $opt in
  h)
    echo "$0 [-h] [-p psql options]"
    echo " -h this help page"
    echo " -o output folder where dumped data is written, default folder is 'backup'"
    echo " -p additional psql command line options, default is empty"
    echo " -H perform an hourly backup, defaults to daily backup"
    exit 0
    ;;
  o)
    BACKUPDIR=$OPTARG
    ;;
  p)
    PG_OPT="$OPTARG"
    ;;
  H)
    HOURLY="true"
    ;;
  esac
done

# Ensure the backup folder exists
mkdir -p ${BACKUPDIR}

# The backup
if [ "${HOURLY}" != "" ]; then
  DUMP_PATH=${BACKUPDIR}/bety-h-${HOUR}.sql.gz
else
  DUMP_PATH=${BACKUPDIR}/bety-d-${TODAY}.sql.gz
fi
pg_dump -U ${BETYUSER} -d ${DATABASE} ${PG_OPT} | gzip -9 > ${DUMP_PATH}

# Handling daily for when we have hourly backups
if [ "${HOURLY}" != "" -a "${HOUR}" == "00" ]; then
  cp -fp ${DUMP_PATH} ${BACKUPDIR}/bety-d-${YESTERDAY}.sql.gz
fi

# Weekly backup
if [ "${DOW}" == "7" ]; then
  cp -fp ${DUMP_PATH} ${BACKUPDIR}/bety-w-${WEEK}.sql.gz
fi

# Monthly backup
if [ "${TOMORROW}" == "01" ]; then
  cp -fp ${DUMP_PATH} ${BACKUPDIR}/bety-m-${MONTH}.sql.gz
fi

# Yearly backup
if [ "${TOMORROW}" == "01"  -a "${MONTH}" == "12" ]; then
  cp -fp ${DUMP_PATH} ${BACKUPDIR}/bety-y-${YEAR}.sql.gz
fi
