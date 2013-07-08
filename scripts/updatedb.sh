#!/bin/bash

# goto home
cd $(dirname $0)/../..
set -x

export CMD="mysql -u bety -pbety bety"
export DB="mysql"
#export CMD="psql -h localhost -U bety bety"
#export DB="psql"

# load latest dump of the database
wget -q -O betydump.${DB}.gz http://isda.ncsa.illinois.edu/~kooper/EBI/betydump.${DB}.gz
gunzip betydump.${DB}.gz

echo "drop database if exists bety; create database bety;" | ${CMD}
${CMD} < betydump.${DB}
rm betydump.${DB}

# remove old runs
sudo rm -rf output
mkdir output
chmod 777 output

# add sites
if [ -e sites/addsites.sh ]; then
  (cd sites && ./addsites.sh)
fi

# add models
echo "INSERT INTO models (model_name, model_type, model_path, revision, created_at, updated_at) VALUES
    ('ED2.2', 'ED2', '${HOSTNAME}:/usr/local/bin/ed2.r46', 46, NOW(), NOW()),
    ('ED2.2', 'ED2', '${HOSTNAME}:/usr/local/bin/ed2.r82', 82, NOW(), NOW()),
    ('SIPNET', 'SIPNET', '${HOSTNAME}:/usr/local/bin/sipnet.runk', 0, NOW(), NOW()),
    ('BIOCRO', 'BIOCRO', '${HOSTNAME}:/bin/true', 1, NOW(), NOW());" | ${CMD}
