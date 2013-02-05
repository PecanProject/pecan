#!/bin/bash

cd $(dirname $0)/..
set -x

# copy database and load locally
ssh kooper@ebi-forecast.igb.illinois.edu "mysqldump --lock-tables=false ZZZZ -u YYYY -pXXXX" > betydump.sql
mysql -u bety -pbety -e 'DROP DATABASE IF EXISTS betydump; CREATE DATABASE betydump'
grep -v "DEFINER" betydump.sql | mysql -f -u bety -pbety betydump

# remove all users to anonomize
mysql -u bety -pbety betydump -e 'update users set login=CONCAT("user", id), name=CONCAT("user ", id), email=CONCAT("betydb+", id, "@gmail.com"), city="Urbana, IL", country="USA", field=NULL, created_at=NOW(), updated_at=NOW(), crypted_password="!", salt="!", remember_token=NULL, remember_token_expires_at=NULL, access_level=3, page_access_level=3, apikey=NULL, state_prov=NULL, postal_code=NULL;'
mysql -u bety -pbety betydump -e 'update users set access_level=1, page_access_level=1 where id=1;'

# remove all non checked data
mysql -u bety -pbety betydump -e 'delete from traits where checked = -1;'
mysql -u bety -pbety betydump -e 'delete from yields where checked = -1;'

# remove all secret data
mysql -u bety -pbety betydump -e 'delete from traits where access_level < 3;'
mysql -u bety -pbety betydump -e 'delete from yields where access_level < 3;'

# dump database and copy to isda
mysqldump -u bety -pbety betydump > betydump.sql
cp betydump.sql /mnt/isda/kooper/public_html/EBI/betydump.sql
