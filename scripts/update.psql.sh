#!/bin/bash

# goto home
cd $(dirname $0)/../..
#set -x

# command to connect to database
if [ "`uname -s`" != "Darwin" ]; then
  export POSTGRES="sudo -u postgres"
fi
export CMD="${POSTGRES} psql -U bety"

# load latest dump of the database
curl -o betydump.gz https://ebi-forecast.igb.illinois.edu/pecan/dump/betydump.psql.gz

${POSTGRES} dropdb bety
${POSTGRES} createdb -O bety bety
${POSTGRES} psql -d bety -c 'CREATE EXTENSION Postgis;'
${POSTGRES} psql -d bety -c 'GRANT ALL ON ALL TABLES IN SCHEMA public TO bety;'

gunzip -c betydump.gz | ${CMD} -d bety
rm betydump.gz

# set all users
ID=2

RESULT=$( ${POSTGRES} psql -t -d bety -c "SELECT count(id) FROM users WHERE login='carya';" )
if [ ${RESULT} -eq 0 ]; then
  RESULT='UPDATE 0'
  while [ "${RESULT}" = "UPDATE 0" ]; do
    RESULT=$( ${POSTGRES} psql -t -d bety -c "UPDATE users SET login='carya', name='carya', crypted_password='df8428063fb28d75841d719e3447c3f416860bb7', salt='carya', access_level=1, page_access_level=1 WHERE id=${ID};" )
    ((ID++))
  done
fi
echo "Added carya with admin privileges"

# set all users
for f in 1 2 3 4; do
  for g in 1 2 3 4; do
    RESULT=$( ${POSTGRES} psql -t -d bety -c "SELECT count(id) FROM users WHERE login='carya${f}${g}';" )
    if [ ${RESULT} -eq 0 ]; then
      RESULT='UPDATE 0'
      while [ "${RESULT}" = "UPDATE 0" ]; do
        RESULT=$( ${POSTGRES} psql -t -d bety -c "UPDATE users SET login='carya${f}${g}', name='carya a-${f} p-${g}', crypted_password='df8428063fb28d75841d719e3447c3f416860bb7', salt='carya', access_level=${f}, page_access_level=${g} WHERE id=${ID};" )
        ((ID++))
      done
    fi
  done
done
echo "Updated users to have login='caryaXY' with appropriate privileges"
echo "  (X=access_level, Y=page_access_level)."

# add guest user
RESULT=$( ${POSTGRES} psql -t -d bety -c "SELECT count(id) FROM users WHERE login='guestuser';" )
if [ ${RESULT} -eq 0 ]; then
  RESULT='UPDATE 0'
  while [ "${RESULT}" = "UPDATE 0" ]; do
    RESULT=$( ${POSTGRES} psql -t -d bety -c "UPDATE users SET login='guestuser', name='guestuser', crypted_password='994363a949b6486fc7ea54bf40335127f5413318', salt='bety', access_level=4, page_access_level=4 WHERE id=${ID};" )
    ((ID++))
  done
fi
echo "Added guestuser with access_level=4 and page_access_level=4"
