#!/bin/bash

if [ ! -e /var/www/html/pecan/dump/dump.pgsql ]; then
  exit 0
fi
rm /var/www/html/pecan/dump/dump.pgsql

PATH=/usr/local/ruby-1.9.3/bin:$PATH 

BETYDUMP_DB="betydump"
QUIET="-q"

pg_dump ebi_production > ${HOME}/dump/betydump.psql

psql ${QUIET} -d "${BETYDUMP_DB}" -c "DROP SCHEMA public CASCADE" 2> /dev/null
psql ${QUIET} -d "${BETYDUMP_DB}" -c "CREATE SCHEMA public"

psql ${QUIET} ${BETYDUMP_DB} < ${HOME}/dump/betydump.psql > /dev/null

# anonymize all accounts
psql ${QUIET} ${BETYDUMP_DB} -c "update users set login=CONCAT('user', id), name=CONCAT('user ', id), email=CONCAT('betydb+', id, '@gmail.com'), city='Urbana, IL', country='USA', area=NULL, created_at=NOW(), updated_at=NOW(), crypted_password='563d44743a37d62a3cbf124fd27f32ab', salt='nosecret', remember_token=NULL, remember_token_expires_at=NULL, access_level=3, page_access_level=4, apikey=NULL, state_prov=NULL, postal_code=NULL;"

# remove all non checked data
psql ${QUIET} ${BETYDUMP_DB} -c "delete from traits where checked = -1;"
psql ${QUIET} ${BETYDUMP_DB} -c "delete from yields where checked = -1;"

# remove all secret data
psql ${QUIET} ${BETYDUMP_DB} -c "delete from traits where access_level < 3;"
psql ${QUIET} ${BETYDUMP_DB} -c "delete from yields where access_level < 3;"

# update bety
#cd ${HOME}/bety 
#git pull ${QUIET}
#rake db:migrate RAILS_ENV="dump"

pg_dump ${BETYDUMP_DB} | gzip > /var/www/html/pecan/dump/betydump.psql.gz
