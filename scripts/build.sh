#!/bin/bash
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

# change to right folder
cd $(dirname $0)/..

# Don't fail on missing suggests
export _R_CHECK_FORCE_SUGGESTS_="FALSE"

# these variables are set using the command line arguments below
CHECK="no"
DOCUMENTATION="no"
FORCE="no"
GIT="no"
INSTALL="yes"
TESTS="no"
NAME="$HOSTNAME"
MANUAL=""

# no arguments means build
if [ $# == 0 ]; then
  FORCE="yes"
fi

# find all variables
while true; do
  if [ "$1" == "" ]; then
    break
  fi
  case "$1" in
    --help|-h)
      echo "$0 <options>"
      echo " --check         : check the R packages before install"
      echo " --documentation : (re)generates all Rd files"
      echo " --force         : force a build"
      echo " --git           : do a git pull"
      echo " --help          : this help text"
      echo " --install       : install all R packages (default)"
      echo " --manual        : generate PDF manual during check, requires tex installed"
      echo " --name X        : use X as hostname"
      echo " --tests         : run tests"
      echo ""
      echo "You can prefix any option with --no- to set it to ignore that option,"
      echo "for example --no-git will not do a git pull. Except for name"
      echo ""
      echo "If you do not pass any arguments it will assume that you want to build"
      echo "even if there are no changes pulled from git. You can change this by"
      echo "using --no-force"
      exit
    ;;

    --check)
      CHECK="yes"
      ;;

    --no-check)
      CHECK="no"
      ;;

    --documentation)
      DOCUMENTATION="yes"
      ;;

    --no-documentation)
      DOCUMENTATION="no"
      ;;

    --force)
      FORCE="yes"
      ;;

    --no-force)
      FORCE="no"
      ;;

    --git)
      GIT="yes"
      ;;

    --no-git)
      GIT="no"
      ;;

    --install)
      INSTALL="yes"
      ;;

    --no-install)
      INSTALL="no"
      ;;

    --manual)
      MANUAL=""
      ;;
      
    --no-manual)
      MANUAL="--no-manual"
      ;;
      
    --name)
      shift
      NAME="$1"
      ;;

    --tests)
      TESTS="yes"
      ;;

    --no-tests)
      TESTS="no"
      ;;


    *)
      echo "unknown argument $1"
      exit 1
      ;;
  esac
  shift
done

# location where to install packages
if [ -z $R_LIBS_USER ]; then
  echo "R_LIBS_USER not set, this could prevent the script from running correctly."
  echo "see https://github.com/PecanProject/pecan/wiki/Installing-PEcAn#set_r_libs_user"
fi

# are we still running
if [ -e running -a "$FORCE" != "yes" ]; then
  echo "Old build still running (file running exists). Use --force to build"
  exit
fi

# remove lock files
if [ "$FORCE" == "yes" ]; then
  rm -rf ${R_LIBS_USER}/00LOCK-*
fi
  
# when did the job run the last time?
touch running
touch lastrun

# pull any changes
if [ "$GIT" == "yes" ]; then
  echo "----------------------------------------------------------------------"
  echo "GIT"
  echo "----------------------------------------------------------------------"
  git pull
  if [ $? != 0 ]; then
    echo Error pulling
    rm running
    exit
  fi
fi

# list any changes to the code
# get changes
echo "----------------------------------------------------------------------"
echo "CHANGES"
echo "----------------------------------------------------------------------"
git log > newlog
diff git.log newlog | grep '^> ' | sed 's/^> //'
mv newlog git.log

STATUS="OK"

# generate documentation
if [ "$DOCUMENTATION" == "yes" ]; then
  for p in ${PACKAGES}; do
    echo "----------------------------------------------------------------------"
    echo "DOCUMENTATION $p"
    echo "----------------------------------------------------------------------"
    make document
  done
fi

# build PEcAn
if [ ! -z  $R_LIBS_USER ]; then
  if [ ! -e ${R_LIBS_USER} ]; then
    mkdir -p ${R_LIBS_USER};
  fi
  rm -rf ${R_LIBS_USER}/PEcAn.*
  R_LIB_INC="--library=${R_LIBS_USER}"
fi

START=`date +'%s'`

# get version number
REVNO=$( git show -s --pretty=format:%T master )

make install

# check/install packages
if [ "$CHECK" == "yes" ]; then
  make check
fi

# all done
TIME=$(echo "`date +'%s'` - $START" |bc -l)
echo "----------------------------------------------------------------------"
echo "build took ${TIME} seconds."
echo "----------------------------------------------------------------------"

# cleanup
rm -rf out.log *.Rcheck PEcAn.*.tar.gz PEcAnRTM*.tar.gz PEcAn.*.tgz

# run tests
if [ "$TESTS" == "yes" ]; then
  START=`date +'%s'`
  make test
  cd tests
  for f in ${NAME}.*.xml; do
    rm -rf pecan
    Rscript --vanilla ../web/workflow.R --settings $f &> output.log
    if [ $? -ne 0 ]; then
      STATUS="BROKEN"
      echo "----------------------------------------------------------------------"
      echo "TEST $f FAILED (RETURN CODE != 0)"
      echo "----------------------------------------------------------------------"
      cat output.log
    elif [ $(grep -v DONE pecan/STATUS | wc -l) -ne 0 ]; then
      STATUS="BROKEN"
      echo "----------------------------------------------------------------------"
      echo "TEST $f FAILED (ERROR IN STATUS)"
      echo "----------------------------------------------------------------------"
      cat pecan/STATUS
      cat output.log
    else
      echo "----------------------------------------------------------------------"
      echo "TEST $f OK"
      echo "----------------------------------------------------------------------"
    fi
    rm -rf output.log pecan
  done
  cd ..

  # all done
  TIME=$(echo "`date +'%s'` - $START" |bc -l)
  echo "----------------------------------------------------------------------"
  echo "tests took ${TIME} seconds."
  echo "----------------------------------------------------------------------"
fi

# remove running marker
rm running

# exit with right status
if [ "$STATUS" != "OK" ]; then 
    echo "ERROR PEcAn BUILD BROKEN" >&2
    exit 1
fi
