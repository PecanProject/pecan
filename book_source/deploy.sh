#!/bin/bash

USER=$(git remote show origin | grep -Po '[^:/]+(?=/pecan)' | head -n1)

./check_bkd_pkg.R

if [ ! -d "../book_output" ]; then 
git clone -b gh-pages \
git@github.com:$USER/pecan \
../book_output
fi

pushd ../book_output

cp -r ../book/* ./
  
git add --all *
  
git commit -m "Update the Book `date`"

git push -q origin gh-pages
