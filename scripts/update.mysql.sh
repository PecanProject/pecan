#!/bin/bash

# goto home
cd $(dirname $0)/../..
set -x

# command to connect to database
export CMD="mysql -u bety -pbety"

# load latest dump of the database
curl -o betydump.gz https://ebi-forecast.igb.illinois.edu/pecan/dump/betydump.mysql.gz

echo "drop database if exists bety; create database bety;" | ${CMD}

gunzip -c betydump.gz | grep -v 'DEFINER' | ${CMD} bety
rm betydump.gz

# remove old runs
sudo rm -rf output
mkdir output
chmod 777 output

# add sites
if [ -e sites/addsites.sh ]; then
  (cd sites && ./addsites.sh)
fi

# add models
if [ -e addmodels.sh ]; then
	./addmodels.sh
else
	echo "INSERT INTO models (model_name, model_type, model_path, revision, created_at, updated_at) VALUES
	    ('ED2.2', 'ED2', '${HOSTNAME}:/usr/local/bin/ed2.r46', '46', NOW(), NOW()),
	    ('ED2.2', 'ED2', '${HOSTNAME}:/usr/local/bin/ed2.r82', '82', NOW(), NOW()),
	    ('SIPNET', 'SIPNET', '${HOSTNAME}:/usr/local/bin/sipnet.runk', 'unk', NOW(), NOW()),
	    ('BIOCRO', 'BIOCRO', '${HOSTNAME}:/bin/true', '0.0.1', NOW(), NOW());" | ${CMD} bety
fi
