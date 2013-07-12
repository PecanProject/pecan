#!/bin/bash

cd $(dirname $0)/..
set -x

# copy database and load locally
ssh ebi-forecast.igb.illinois.edu "mysqldump --lock-tables=false ZZZZ -u YYYY -pXXXX" > betydump.sql
mysql -u bety -pbety -e 'DROP DATABASE IF EXISTS betydump; CREATE DATABASE betydump'
grep -v "DEFINER" betydump.sql | mysql -f -u bety -pbety betydump

# anonymize all accounts, set default password to illinois
mysql -u bety -pbety betydump -e 'update users set login=CONCAT("user", id), name=CONCAT("user ", id), email=CONCAT("betydb+", id, "@gmail.com"), city="Urbana, IL", country="USA", field=NULL, created_at=NOW(), updated_at=NOW(), crypted_password="df8428063fb28d75841d719e3447c3f416860bb7", salt="carya", remember_token=NULL, remember_token_expires_at=NULL, access_level=3, page_access_level=3, apikey=NULL, state_prov=NULL, postal_code=NULL;'
mysql -u bety -pbety betydump -e 'update users set login="carya", access_level=1, page_access_level=1 where id=1;'

# remove all non checked data
mysql -u bety -pbety betydump -e 'delete from traits where checked = -1;'
mysql -u bety -pbety betydump -e 'delete from yields where checked = -1;'

# remove all secret data
mysql -u bety -pbety betydump -e 'delete from traits where access_level < 3;'
mysql -u bety -pbety betydump -e 'delete from yields where access_level < 3;'

# update bety
# this assumes there is an environment called dump in the database.yml file
if [ -e ../bety ]; then
  (cd ../bety && rake db:migrate RAILS_ENV="dump")
elif [ -e /usr/local/bety ]; then
  (cd /usr/local/bety && rake db:migrate RAILS_ENV="dump")
fi

# dump database and copy to isda
mysqldump -u bety -pbety betydump | gzip > betydump.mysql.gz
cp betydump.mysql.gz /mnt/isda/kooper/public_html/EBI/

# create postgres version
mysql -u bety -pbety betydump -e 'drop view mgmtview, yieldsview;'

echo "DROP DATABASE betydump; CREATE DATABASE betydump" | sudo -u postgres psql
taps server mysql://bety:bety@localhost/betydump?encoding=latin1 bety bety &
SERVER=$!

sleep 10
taps pull postgres://bety:bety@localhost/betydump http://bety:bety@localhost:5000 
kill -9 $SERVER

sudo -u postgres pg_dump betydump | gzip > betydump.psql.gz
cp betydump.psql.gz /mnt/isda/kooper/public_html/EBI
