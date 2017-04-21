#!/bin/bash

#exit on error
set -e

#check for environment variable
[ -z "${GITHUB_PAT}" ] && exit 0

# only deploy if this is the master branch build
branch_name=$(git symbolic-ref -q HEAD)
branch_name=${branch_name##refs/heads/}
branch_name=${branch_name:-HEAD}
[ "${branch_name}" != "master" ] && exit 0

#set USER 
USER=${TRAVIS_REPO_SLUG%/*}


# configure your name and email if you have not done so
git config --global user.email "pecanproj@gmail.com"
git config --global user.name "TRAVIS-DOC-BUILD"

git clone https://${GITHUB_PAT}@github.com/${USER}/pecan-documentation.git book_hosted


cp -r _book/* book_hosted


cd book_hosted
git add --all *
git commit -m"Update the book `date`" || true
git push -q origin master

