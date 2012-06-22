#!/bin/bash

# To run this from cron add the following line to your crontab (crontab -e)
#
# # m h  dom mon dow   command
# */15 * * * * cd ${HOME}/autobuild/trunk && ./build.sh

# packages that are to be compiled
PACKAGES="common utils db modules/meta.analysis modules/uncertainty models/ed"

# people to notify of the build
TO="kooper@illinois.edu,sserbin@illinois.edu,dlebauer@illinois.edu,mdietze@illinois.edu"

# are we still running
if [ -e running ]; then
  exit
fi

# when did the job run the last time?
touch running
touch lastrun

# check to see if there are any updates
bzr missing >/dev/null
if [ $? -eq 1 ]; then
  START=`date +'%s.%N'`
  STATUS="OK"

  # update repository
  bzr pull -q > changes.log

  # get changes
  echo "----------------------------------------------------------------------" >> changes.log
  echo "CHANGES" >> changes.log
  echo "----------------------------------------------------------------------" >> changes.log
  bzr log > newlog
  diff bzr.log newlog | grep '^> ' | sed 's/^> //' > changes.log
  mv newlog bzr.log

  # get committer names and emails
  # TODO all commiters are smushed together
  #IFS_BAK=$IFS
  #IFS=`echo -e '\n'`
  #NAMES=""
  #EMAILS=""
  #for c in `grep 'committer: ' changes.log | sort -u`; do
  #  EMAILS="${EMAILS},`echo $c | sed -e 's/.*<\(.*\)>/\1/'`"
  #done
  #IFS=$IFS_BAK
  #NAMES=$( grep 'committer: ' changes.log | uniq | sed -e 's/committer: //' )
  #EMAILS=$( grep 'committer: ' changes.log | uniq | sed -e 's/.*<\(.*\)>/\1/' )
  #TO="${TO} ${EMAILS}"

  # get version number
  REVNO=$( grep 'revno: ' bzr.log | head -1 | sed -e 's/revno: //' )

  # location where packages are installed
  if [ ! -e autobuild ]; then mkdir autobuild; fi
  export R_LIBS_USER="autobuild"

  # check/install packages
  for p in ${PACKAGES}; do
    PACKAGE="OK"
    R CMD check --library=${R_LIBS_USER} $p &> out.log
    if [ $? -ne 0 ]; then
      STATUS="BROKEN"
      PACKAGE="BROKEN"
      echo "----------------------------------------------------------------------" >> changes.log
      echo "CHECK $p BROKEN" >> changes.log
      echo "----------------------------------------------------------------------" >> changes.log
      cat out.log >> changes.log
    fi
    R CMD INSTALL --build --library=${R_LIBS_USER} $p &> out.log
    if [ $? -ne 0 ]; then
      STATUS="BROKEN"
      PACKAGE="BROKEN"
      echo "----------------------------------------------------------------------" >> changes.log
      echo "INSTALL $p BROKEN" >> changes.log
      echo "----------------------------------------------------------------------" >> changes.log
      cat out.log >> changes.log
    fi
    if [ "$PACKAGE" == "OK" ]; then
      echo "----------------------------------------------------------------------" >> changes.log
      echo "CHECK/INSTALL $p OK" >> changes.log
      echo "----------------------------------------------------------------------" >> changes.log
    fi
  done

  # all done
  TIME=$(echo "`date +'%s.%N'` - $START" |bc -l)
  echo "----------------------------------------------------------------------" >> changes.log
  echo "build took ${TIME} seconds." >> changes.log
  echo "----------------------------------------------------------------------" >> changes.log
  cat changes.log | mail -s "PEcAn BUILD ${REVNO} is ${STATUS}" ${TO}

  # cleanup
  rm -rf changes.log out.log autobuild *.Rcheck PEcAn.*.tar.gz
fi

rm running
