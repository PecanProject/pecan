#!/usr/bin/env bash

targetdir=tutorials_rendered

mkdir -p ${targetdir}

while read f; do
    Rscript -e "rmarkdown::render('$f')"
    mv ${f/\.Rmd/.html} ${targetdir}
done < buildfiles
