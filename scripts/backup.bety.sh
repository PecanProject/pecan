#!/bin/bash

# simple script to creat backups of the bety database. This script will
# create a copy of the datbase daily, weekly, monthly and yearly. The
# files will be called:
# - bety-d-X, aily backup, where X is the day of the month.
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
TODAY=$( date +"%u" )
TOMORROW=$( date -d "tomorrow" +"%d" )
DOW=$( date +"%d" )
WEEK=$( date +"%W" )
MONTH=$( date +"%m" )
YEAR=$( date +"%Y" )

# DAILY BACKUP
pg_dump -U ${BETYUSER} -d ${DATABASE} | gzip -9 > ${BACKUPDIR}/bety-d-${TODAY}.sql.gz

# WEEKLY BACKUP
if [ "${DOW}" == "7" ]; then
  pg_dump -U ${BETYUSER} -d ${DATABASE} | gzip -9 > ${BACKUPDIR}/bety-w-${WEEK}.sql.gz
fi

# MONTHLY BACKUP
if [ "${TOMORROW}" == "01" ]; then
  pg_dump -U ${BETYUSER} -d ${DATABASE} | gzip -9 > ${BACKUPDIR}/bety-m-${MONTH}.sql.gz
fi

# YEARLY BACKUP
if [ "${TOMORROW}" == "01"  -a "${MONTH}" == "12" ]; then
  pg_dump -U ${BETYUSER} -d ${DATABASE} | gzip -9 > ${BACKUPDIR}/bety-y-${YEAR}.sql.gz
fi
