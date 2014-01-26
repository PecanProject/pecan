#!/bin/bash

# goto home
cd $(dirname $0)/../..
set -x

# command to connect to database
export CMD="psql -U bety"

# load latest dump of the database
curl -o betydump.gz http://isda.ncsa.illinois.edu/~kooper/EBI/betydump.psql.gz

sudo -u postgres dropdb bety
sudo -u postgres createdb -O bety bety

gunzip -c betydump.gz | ${CMD} bety
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
