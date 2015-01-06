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
DEPENDENCIES="no"
DOCUMENTATION="no"
EMAIL=""
FORCE="no"
GIT="no"
INSTALL="yes"
TESTS="no"

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
      echo " --dependencies  : install any dependencies"
      echo " --documentation : (re)generates all Rd files"
      echo " --email         : send email to following people on success"
      echo " --force         : force a build"
      echo " --git           : do a git pull"
      echo " --help          : this help text"
      echo " --install       : install all R packages (default)"
      echo " --tests         : run tests"
      echo ""
      echo "You can prefix any option with --no- to set it to ignore that option,"
      echo "for example --no-git will not do a git pull."
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

    --dependencies)
      DEPENDENCIES="yes"
      ;;

    --no-dependencies)
      DEPENDENCIES="no"
      ;;

    --documentation)
      DOCUMENTATION="yes"
      ;;

    --no-documentation)
      DOCUMENTATION="no"
      ;;

    --email)
      EMAIL="$2"
      shift
      ;;

    --no-email)
      EMAIL=""
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

# packages that are to be compiled
PACKAGES="utils db settings visualization"
PACKAGES="${PACKAGES} modules/priors modules/meta.analysis modules/uncertainty"
PACKAGES="${PACKAGES} modules/data.land modules/data.atmosphere modules/data.remote"
PACKAGES="${PACKAGES} modules/assim.batch modules/assim.sequential"
PACKAGES="${PACKAGES} modules/allometry modules/benchmark modules/photosynthesis"
PACKAGES="${PACKAGES} models/ed models/sipnet models/biocro models/dalec"
PACKAGES="${PACKAGES} all"

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

# when did the job run the last time?
touch running
touch lastrun

# pull any changes
if [ "$GIT" == "yes" ]; then
  git pull > changes.log
  if [ $? != 0 ]; then
    echo Error pulling
    rm running
    exit
  fi
  if ! grep --quiet 'Already' changes.log; then
    FORCE="yes"
  fi
fi

# list any changes to the code
if [ "$FORCE" == "yes" ]; then
  # get changes
  echo "----------------------------------------------------------------------" >> changes.log
  echo "CHANGES" >> changes.log
  echo "----------------------------------------------------------------------" >> changes.log
  git log > newlog
  diff git.log newlog | grep '^> ' | sed 's/^> //' > changes.log
  mv newlog git.log
  if [ "$EMAIL" == "" ]; then
    cat changes.log
    rm changes.log
  fi
fi

STATUS="OK"

# install all dependencies
if [ "$DEPENDENCIES" == "yes" ]; then
  echo "----------------------------------------------------------------------" >> changes.log
  echo "DEPENDENCIES" >> changes.log
  echo "----------------------------------------------------------------------" >> changes.log
  ./scripts/install.dependencies.R >> changes.log 2>&1
  if [ "$EMAIL" == "" ]; then
    cat changes.log
    rm changes.log
  fi
fi

# generate documentation
if [ "$DOCUMENTATION" == "yes" ]; then
  for p in ${PACKAGES}; do
    echo "----------------------------------------------------------------------" >> changes.log
    echo "DOCUMENTATION $p" >> changes.log
    echo "----------------------------------------------------------------------" >> changes.log
    echo "if (require(roxygen2)) roxygenise('$p', roclets=c('rd'))" | R --vanilla >> changes.log 2>&1
    if [ "$EMAIL" == "" ]; then
      cat changes.log
      rm changes.log
    fi
  done
fi

# build PEcAn
if [ "$FORCE" == "yes" ]; then
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

  # check/install packages
  for p in ${PACKAGES}; do
    PACKAGE="OK"
    ACTION=""
    BASENAME=$(basename "$p")

    if [ "$CHECK" == "yes" ]; then
      ACTION="CHECK"
	    R CMD check ${R_LIB_INC} $p &> out.log
	    if [ $? -ne 0 ]; then
	      STATUS="BROKEN"
	      PACKAGE="BROKEN"
	      echo "----------------------------------------------------------------------" >> changes.log
	      echo "CHECK $p BROKEN" >> changes.log
	      echo "----------------------------------------------------------------------" >> changes.log
	      cat out.log >> changes.log
        if [ -e "${BASENAME}.Rcheck/00install.out" ]; then
          echo "--- ${BASENAME}.Rcheck/00install.out" >> changes.log
          cat "${BASENAME}.Rcheck/00install.out" >> changes.log
        fi
        if [ -e "${BASENAME}.Rcheck/tests/testthat.Rout.fail" ]; then
          echo "--- ${BASENAME}.Rcheck/tests/testthat.Rout.fail" >> changes.log
          cat "${BASENAME}.Rcheck/tests/testthat.Rout.fail" >> changes.log
        fi
	    fi
	  fi

    if [ "$PACKAGE" == "OK" -a "$INSTALL" == "yes" ]; then
      if [ "$ACTION" == "" ]; then
        ACTION="INSTALL"
      else
        ACTION="$ACTION/INSTALL"
      fi
      R CMD INSTALL --build ${R_LIB_INC} $p &> out.log
      if [ $? -ne 0 ]; then
        STATUS="BROKEN"
        PACKAGE="BROKEN"
        echo "----------------------------------------------------------------------" >> changes.log
        echo "INSTALL $p BROKEN" >> changes.log
        echo "----------------------------------------------------------------------" >> changes.log
        cat out.log >> changes.log
        if [ -e "${BASENAME}.Rcheck/00install.out" ]; then
          echo "--- ${BASENAME}.Rcheck/00install.out" >> changes.log
          cat "${BASENAME}.Rcheck/00install.out" >> changes.log
        fi
      fi
    fi
    
    if [ "$PACKAGE" == "OK" ]; then
      if [ "$ACTION" == "" ]; then
        ACTION="DID NOTHING"
      fi
      echo "----------------------------------------------------------------------" >> changes.log
      echo "$ACTION $p OK" >> changes.log
      echo "----------------------------------------------------------------------" >> changes.log
      if [ "$EMAIL" == "" ]; then
        cat changes.log
        rm changes.log
      fi
    else
      if [ "$EMAIL" == "" ]; then
        cat changes.log
        rm changes.log
      fi
    fi
  done

  # all done
  TIME=$(echo "`date +'%s'` - $START" |bc -l)
  echo "----------------------------------------------------------------------" >> changes.log
  echo "build took ${TIME} seconds." >> changes.log
  echo "----------------------------------------------------------------------" >> changes.log

  if [ "$EMAIL" == "" ]; then
    cat changes.log
    rm changes.log
  fi

  # cleanup
  rm -rf out.log *.Rcheck PEcAn.*.tar.gz PEcAn.*.tgz
fi

# run tests
if [ "$TESTS" == "yes" ]; then
  START=`date +'%s'`
  cd tests
  for f in ${HOSTNAME}.*.xml; do
    rm -rf pecan
    Rscript --vanilla workflow.R --settings $f &> output.log
    if [ $? -ne 0 ]; then
      STATUS="BROKEN"
      echo "----------------------------------------------------------------------" >> changes.log
      echo "TEST $f FAILED (RETURN CODE != 0)" >> changes.log
      echo "----------------------------------------------------------------------" >> changes.log
      cat output.log >> changes.log
    elif [ $(grep -v DONE pecan/STATUS | wc -l) -ne 0 ]; then
      STATUS="BROKEN"
      echo "----------------------------------------------------------------------" >> changes.log
      echo "TEST $f FAILED (ERROR IN STATUS)" >> changes.log
      echo "----------------------------------------------------------------------" >> changes.log
      cat pecan/STATUS >> changes.log
      cat output.log >> changes.log
    else
      echo "----------------------------------------------------------------------" >> changes.log
      echo "TEST $f OK" >> changes.log
      echo "----------------------------------------------------------------------" >> changes.log
    fi
    rm -rf output.log pecan

    if [ "$EMAIL" == "" ]; then
      cat changes.log
      rm changes.log
    fi
  done
  cd ..

  # all done
  TIME=$(echo "`date +'%s'` - $START" |bc -l)
  echo "----------------------------------------------------------------------" >> changes.log
  echo "tests took ${TIME} seconds." >> changes.log
  echo "----------------------------------------------------------------------" >> changes.log
fi

# send email with report
if [ -e changes.log ]; then
  if [ "$EMAIL" == "" ]; then
    cat changes.log
  else
    cat changes.log | mail -s "PEcAn BUILD ${STATUS} : ${REVNO}" ${EMAIL}
  fi
  rm changes.log
fi

# remove running marker
rm running

# exit with right status
if [ "$STATUS" != "OK" ]; then 
    echo "ERROR PEcAn BUILD BROKEN" >&2
    exit 1
fi
