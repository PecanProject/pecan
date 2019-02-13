#!/bin/bash

# simple script to creat backups of the bety database. This script will
# create a copy of the datbase daily, weekly, monthly and yearly. The
# files will be called:
# - bety-h-X, hourly backup, where X is the hour of the day (24 hour time).
# - bety-d-X, daily backup, where X is the day of the month.
# - bety-w-X, weekly backup, where X is the week number in the year
# - bety-m-X, montly backup, where X is the month of the year
# - bety-y-X, yearly backup, where X is the actual year.

# variables to use for the database dump
DATABASE=bety
BETYUSER=bety

# location where backup should be written
BACKUPDIR=$( dirname $0 )/backup
mkdir -p ${BACKUPDIR}

# set path if needed
#export PATH=<location to postgresql>/bin;${PATH}

# some handy variables
HOUR=$( date +"%H" )
TODAY=$( date +"%d" )
TOMORROW=$( date -d "tomorrow" +"%d" )
DOW=$( date +"%u" )
WEEK=$( date +"%W" )
MONTH=$( date +"%m" )
YEAR=$( date +"%Y" )

# psql options
# this allows you to add any other options
PG_OPT=${PG_OPT:-""}

# parse command line options
while getopts hp: opt; do
  case $opt in
  h)
    echo "$0 [-h] [-p psql options]"
    echo " -h this help page"
    echo " -p additional psql command line options, default is empty"
    exit 0
    ;;
  p)
    PG_OPT="$OPTARG"
    ;;
  esac
done

# THE BACKUP
if [ "${HOURLY}" != "" ]; then
  DUMP_PATH=${BACKUPDIR}/bety-h-${HOUR}.sql.gz
else
  DUMP_PATH=${BACKUPDIR}/bety-d-${TODAY}.sql.gz
fi
pg_dump -U ${BETYUSER} -d ${DATABASE} ${PG_OPT} | gzip -9 > ${DUMP_PATH}

# HANDLING DAILY FOR WHEN WE HAVE HOURLY BACKUPS
if [ "${HOURLY}" != "" -a "${HOUR}" == "00" ]; then
  cp -fp ${DUMP_PATH} ${BACKUPDIR}/bety-d-${TODAY}.sql.gz
fi

# WEEKLY BACKUP
if [ "${DOW}" == "7" ]; then
  cp -fp ${DUMP_PATH} ${BACKUPDIR}/bety-w-${WEEK}.sql.gz
fi

# MONTHLY BACKUP
if [ "${TOMORROW}" == "01" ]; then
  cp -fp ${DUMP_PATH} ${BACKUPDIR}/bety-m-${MONTH}.sql.gz
fi

# YEARLY BACKUP
if [ "${TOMORROW}" == "01"  -a "${MONTH}" == "12" ]; then
  cp -fp ${DUMP_PATH} ${BACKUPDIR}/bety-y-${YEAR}.sql.gz
fi
