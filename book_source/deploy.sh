#!/bin/bash

set -e

#USER=$(git remote show origin | grep -Po '[^:/]+(?=/pecan)' | head -n1)
[ -z "${GITHUB_PAT}" ] && exit 0
[ "${TRAVIS_BRANCH}" != "master" ] && exit 0

# configure your name and email if you have not done so
git config --global user.email "tonygard@bu.edu"
git config --global user.name "Tony Gardella"

# clone the repository to the book-output directory
#if [ ! -d "../book_output" ]; then 
#git clone -b gh-pages \
#git@github.com:$USER/pecan \
#https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git \
#git@github.com:${GITHUB_PAT}/pecan_bookdown.git \
#https://github.com/PecanProject/pecan_bookdown.git 
#fi

git clone https://${GITHUB_PAT}@github.com/pecan_bookdown.git book_output

cd ../book_output


cp -r ../book/* ./
git add --all *
git commit -m"Update the book `date`" || true
git push -q origin master

