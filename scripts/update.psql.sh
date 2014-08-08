#!/bin/bash

# goto home
cd $(dirname $0)/../..
set -x

# command to connect to database
if [ "`uname -s`" != "Darwin" ]; then
  export POSTGRES="sudo -u postgres"
fi
export CMD="${POSTGRES} psql -U bety"

# Fully qualified hostname
FQDN=$( hostname -f )

# load latest dump of the database
curl -o betydump.gz https://ebi-forecast.igb.illinois.edu/pecan/dump/betydump.psql.gz

${POSTGRES} dropdb bety
${POSTGRES} createdb -O bety bety
${POSTGRES} psql -d bety -c 'CREATE EXTENSION Postgis;'
${POSTGRES} psql -d bety -c 'GRANT ALL ON ALL TABLES IN SCHEMA public TO bety;'

gunzip -c betydump.gz | ${CMD} bety
rm betydump.gz

# set all users
${POSTGRES} psql -t -d bety -c "UPDATE users SET login='carya', crypted_password='df8428063fb28d75841d719e3447c3f416860bb7', salt='carya', access_level=1, page_access_level=1 WHERE id=1;"
ID=2
for f in 1 2 3 4; do
  for g in 1 2 3 4; do
    RESULT='UPDATE 0'
    while [ "${RESULT}" = "UPDATE 0" ]; do
      RESULT=$( ${POSTGRES} psql -t -d bety -c "UPDATE users SET login='carya${f}${g}', crypted_password='df8428063fb28d75841d719e3447c3f416860bb7', salt='carya', access_level=${f}, page_access_level=${g} WHERE id=${ID};" )
      ((ID++))
    done
  done
done
echo "Updated users to have login='caryaXY' with appropriate privileges"
echo "  (X=access_level, Y=page_access_level)."

# remove old runs
sudo rm -rf output
mkdir output
chmod 777 output

# add sites
if [ -e sites/addsites.sh ]; then
  (cd sites && ./addsites.sh)
fi

# add models
$(dirname $0)/add.models.sh
