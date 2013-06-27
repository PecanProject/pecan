#!/bin/bash
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

export DB="fia5data"
export DB_USER="fia5data"
export DB_PASS="fia5data"
export VER=5_1

# set this to yes to force a reload always
reload="no"

FILES="FIADB_REFERENCE AK AL AR AZ CA CO CT DE FL GA IA ID IL IN KS KY LA MA MD ME MI MN MO MS MT NC ND NE NH NJ NM NV NY OH OK OR PA PR RI SC SD TN TX UT VA VI VT WA WI WV WY"

# mirror(filename, [extra path])
# will append http://apps.fs.fed.us/fiadb-downloads/ to remote file
function mirror {
  filename="$1" 
  url="http://apps.fs.fed.us/fiadb-downloads/$2/${filename}"

  echo "Checking ${filename}"

  # ONLY WORKS IF NOT ALL FILES ARE CHANGED! OTHERWISE NEED A CMP
  if [ -e data/${filename} ]; then
    if [ -e tmp/${filename} ]; then
      rm -f tmp/${filename}
    fi
    ln data/${filename} tmp
    before=$( cd tmp ; ls -l ${filename} )
  else
    before=""
  fi
  wget -q -P tmp -N ${url}
  after=$( cd tmp ; ls -l ${filename} )

  result=1
  if [ "${before}" != "${after}" ]; then
    cmp -s tmp/${filename} data/${filename}
    if [ $? != 0 ]; then
      result=0
    fi
    mv tmp/${filename} data/${filename}
  fi
  if [ -e tmp/${filename} ]; then
    rm -f tmp/${filename}
  fi
  return ${result}
}

# ----------------------------------------------------------------------
# CREATE DATA AND TMP DIR
# ----------------------------------------------------------------------
if [ ! -d data ]; then mkdir data; fi
if [ ! -d tmp ]; then mkdir tmp; fi
  
# ----------------------------------------------------------------------
# FETCH SCHEMA
# ----------------------------------------------------------------------
echo "----------------------------------------------------------------------"
echo "CHECKING FOR SCHEMA FILE"
echo ""
if mirror "FIADB_version${VER}.accdb" "images" ; then
  echo "NEW SCHEMA DETECTED!"
  echo "Please convert schema to fiadb${VER}.sql"
  exit
fi

# ----------------------------------------------------------------------
# FETCH CHANGES
# ----------------------------------------------------------------------
echo "----------------------------------------------------------------------"
echo "CHECKING FOR NEW CHANGES FILES"
echo ""
if mirror "recent_load_history.html" "images" ; then
  mv data/changes.txt data/oldchanges.txt
  ./html2text.py data/recent_load_history.html | egrep -v "^[ ]*$" | head --lines=-1 | tail --lines=+2 > data/changes.txt
  diff data/oldchanges.txt data/changes.txt
  rm data/oldchanges.txt
fi

# ----------------------------------------------------------------------
# FETCH DATA
# ----------------------------------------------------------------------
echo "----------------------------------------------------------------------"
echo "CHECKING FOR NEW DATA FILES"
echo ""
for f in ${FILES}; do
  if mirror "$f.ZIP"; then
    echo "FILE data/$f.ZIP CHANGED"
    reload="yes"
  fi
done

# ----------------------------------------------------------------------
# RELOAD DATABASE
# ----------------------------------------------------------------------
if [ "${reload}" == "yes" ]; then
  echo "----------------------------------------------------------------------"
  echo "LOADING DATABASE"
  mysql -u ${DB_USER} -p${DB_PASS} ${DB} < fiadb${VER}.sql
  if [ -e fiadb${VER}_index.sql ]; then
    mysql -u ${DB_USER} -p${DB_PASS} ${DB} < fiadb${VER}_index.sql
  fi

  for f in ${FILES}; do
    unzip -q -d data/$f data/$f.ZIP
    if [ -e data/$f.log ]; then
      rm data/$f.log
    fi
    for g in data/$f/*.CSV; do
      table=$(basename $g .CSV)
      if [[ "${table}" == ${f}_* ]]; then
        table=${table:3}
      fi
      echo "LOADING ${g} INTO TABLE fiadb4.${table}" >> data/$f.log
      mysql --local-infile=1 --user=${DB_USER}  --password=${DB_PASS} --database=${DB} --execute="load data local infile '${g}' INTO TABLE ${DB}.${table} FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '\"' LINES TERMINATED BY '\r\n' IGNORE 1 LINES; SHOW WARNINGS" >> data/$f.log
    done
    rm -rf data/$f
  done
fi

echo "----------------------------------------------------------------------"
