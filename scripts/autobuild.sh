#!/bin/bash
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

# To run this from cron add the following line to your crontab (crontab -e)
#
# # m h  dom mon dow   command
# */15 * * * * cd ${HOME}/autobuild/trunk && ./scripts/autobuild.sh

# packages that are to be compiled
PACKAGES="utils db"
PACKAGES="${PACKAGES} modules/meta.analysis modules/uncertainty"
PACKAGES="${PACKAGES} modules/data.land modules/data.atmosphere"
PACKAGES="${PACKAGES} modules/assim.batch modules/assim.sequential modules/priors"
PACKAGES="${PACKAGES} models/ed models/sipnet models/biocro"
PACKAGES="${PACKAGES} all"

# people to notify of the build, leave blank to not send email
TO=""

# Should a pull be done before building
PULL="yes"

# Should PEcAn be always build, or only if a change was made
BUILD="no"

# run check before install
CHECK="yes"

# location where to install packages
if [ $UID -eq 0 ]; then
  unset R_LIBS_USER
elif [ -z $R_LIBS_USER ]; then
  export R_LIBS_USER="${HOME}/lib/R"
fi
if [ ! -z $R_LIBS_USER ]; then
  if [ ! -e ${R_LIBS_USER} ]; then mkdir -p ${R_LIBS_USER}; fi
  rm -rf ${R_LIBS_USER}/PEcAn.*
  R_LIB_INC="--library=${R_LIBS_USER}"
fi

# are we still running
if [ -e running ]; then
  exit
fi

# when did the job run the last time?
touch running
touch lastrun

# pull any changes
if [ "$PULL" == "yes" ]; then
  git pull > changes.log
  if `grep 'Already' changes.log > /dev/null`; then
    BUILD="yes"
  fi
fi

if [ "$BUILD" == "yes" ]; then
  START=`date +'%s.%N'`
  STATUS="OK"

  # get changes
  echo "----------------------------------------------------------------------" >> changes.log
  echo "CHANGES" >> changes.log
  echo "----------------------------------------------------------------------" >> changes.log
  git log > newlog
  diff git.log newlog | grep '^> ' | sed 's/^> //' > changes.log
  mv newlog git.log

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
  REVNO=$( git show -s --pretty=format:%T master )

  # check/install packages
  for p in ${PACKAGES}; do
    PACKAGE="OK"

    if [ "$CHECK" == "yes" ]; then
	    R CMD check ${R_LIB_INC} $p &> out.log
	    if [ $? -ne 0 ]; then
	      STATUS="BROKEN"
	      PACKAGE="BROKEN"
	      echo "----------------------------------------------------------------------" >> changes.log
	      echo "CHECK $p BROKEN" >> changes.log
	      echo "----------------------------------------------------------------------" >> changes.log
	      cat out.log >> changes.log
	      if [ "$TO" == "" ]; then
	        cat changes.log
	        rm changes.log
	      fi
	    fi
	  fi

    R CMD INSTALL --build ${R_LIB_INC} $p &> out.log
    if [ $? -ne 0 ]; then
      STATUS="BROKEN"
      PACKAGE="BROKEN"
      echo "----------------------------------------------------------------------" >> changes.log
      echo "INSTALL $p BROKEN" >> changes.log
      echo "----------------------------------------------------------------------" >> changes.log
      cat out.log >> changes.log
      if [ "$TO" == "" ]; then
        cat changes.log
        rm changes.log
      fi
    fi
    
    if [ "$PACKAGE" == "OK" ]; then
      if [ "$CHECK" == "yes" ]; then
        echo "----------------------------------------------------------------------" >> changes.log
        echo "CHECK/INSTALL $p OK" >> changes.log
        echo "----------------------------------------------------------------------" >> changes.log
      else
        echo "----------------------------------------------------------------------" >> changes.log
        echo "INSTALL $p OK" >> changes.log
        echo "----------------------------------------------------------------------" >> changes.log
      fi
      if [ "$TO" == "" ]; then
        cat changes.log
        rm changes.log
      fi
    fi
  done

  # all done
  TIME=$(echo "`date +'%s.%N'` - $START" |bc -l)
  echo "----------------------------------------------------------------------" >> changes.log
  echo "build took ${TIME} seconds." >> changes.log
  echo "----------------------------------------------------------------------" >> changes.log
  if [ "$TO" == "" ]; then
    cat changes.log
  else
    cat changes.log | mail -s "PEcAn BUILD ${REVNO} is ${STATUS}" ${TO}
  fi

  # cleanup
  rm -rf changes.log out.log *.Rcheck PEcAn.*.tar.gz
fi

rm running
