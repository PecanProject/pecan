#!/bin/bash

#exit on error
set -e

#check for environment variable
[ -z "${GITHUB_PAT}" ] && exit 0

# find version if we are develop/latest/release and if should be pushed

BRANCH="$(git rev-parse --abbrev-ref HEAD)"
PUBLISH="NOPE"
if [ "$BRANCH" = "master" ]; then
  PUBLISH="yes"
  VERSION="latest"
elif [ "$BRANCH" = "develop" ]; then
  PUBLISH="yes"
  VERSION="develop"
elif [ "$( echo $BRANCH | sed -e 's#^release/.*$#release#')" = "release" ]; then
  PUBLISH="yes"
  VERSION="$( echo $BRANCH | sed -e 's#^release/\(.*\)$#\1#' )"
else
  PUBLISH="no"
  VERSION="local"
fi

## ID Publish=yes push to pecan-documentation/branch

if [ $PUBLISH == yes ]; then

  #set USER 
  USER=${TRAVIS_REPO_SLUG%/*}

  # configure your name and email if you have not done so
  git config --global user.email "pecanproj@gmail.com"
  git config --global user.name "TRAVIS-DOC-BUILD"

  git clone https://${GITHUB_PAT}@github.com/${USER}/pecan-documentation.git book_hosted


  cd book_hosted
  
  ## Check if branch named directory exists 
  if [ -d $BRANCH ]; then
    cd $BRANCH
  else
    mkdir $BRANCH
    cd $BRANCH
  fi
  
  rsync -a --delete ../../_book/* .
  
  git add --all *
  git commit -m "Update the book `date`" || true
  git push -q origin master

elif [ $PUBLISH == no ]; then

  echo "Not Master, Develop, or Release Branch. Will not render Book."
  exit

else

  echo "Publish status not found. What did you do?"
  exit 
  
fi
