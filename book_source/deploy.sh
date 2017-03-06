#!/bin/bash

USER=$(git remote show origin | grep -Po '[^:/]+(?=/pecan)' | head -n1)

# configure your name and email if you have not done so
git config --global user.email "you@example.com"
git config --global user.name "Your Name"

# clone the repository to the book-output directory
if [ ! -d "../book_output" ]; then 
git clone -b gh-pages \
#git@github.com:$USER/pecan \
#https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git \
#git@github.com:${GITHUB_PAT}/pecan_bookdown.git \
https://github.com/PecanProject/pecan_bookdown.git 
../book_output
fi

pushd ../book_output

cp -r ../book/* ./
  
git add --all *
  
git commit -m "Update the Book `date`"

git push -q origin gh-pages
