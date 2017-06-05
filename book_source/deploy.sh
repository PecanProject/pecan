#!/bin/bash

#exit on error
set -e

#check for environment variable
[ -z "${GITHUB_PAT}" ] && exit 0

#Print who made GITHUB_PAT variable
echo "GITHUB_PAT variable made by Tony Gardella"

# don't run on pull requests
[ "$TRAVIS_PULL_REQUEST" != "false" ]  && exit 0

# find version if we are develop/latest/release and if should be pushed
if [ "$TRAVIS_BRANCH" = "master" ]; then
  VERSION="master"
elif [ "$TRAVIS_BRANCH" = "develop" ]; then
  VERSION="develop"
elif [ "$( echo $TRAVIS_BRANCH | sed -e 's#^release/.*$#release#')" = "release" ]; then
  VERSION="$( echo $TRAVIS_BRANCH | sed -e 's#^release/\(.*\)$#\1#' )"
else
  echo "Not Master, Develop, or Release Branch. Will not render Book."
  exit 0
fi

#set USER 
USER=${TRAVIS_REPO_SLUG%/*}

# configure your name and email if you have not done so
git config --global user.email "pecanproj@gmail.com"
git config --global user.name "TRAVIS-DOC-BUILD"

# clone documentation git repo
git clone https://${GITHUB_PAT}@github.com/${USER}/pecan-documentation.git book_hosted
cd book_hosted

## Check if branch named directory exists 
if [ ! -d $VERSION ]; then
  mkdir $VERSION
fi

# copy new documentation
rsync -a --delete ../_book/ $VERSION/

# push updated documentation back up
git add --all *
git commit -m "Update the book `date`" || true
git push -q origin master
