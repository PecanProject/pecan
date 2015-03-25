#!/bin/bash
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

# packages needed which might not be required/library
ALL="ggplot2 randtoolbox gridExtra testthat roxygen2"

# packages that are not in cran
SKIP="(time)"

# find all packages needed (require and library)
for f in `find . -type d -name R`; do
  NAME=$( echo $f | sed -e 's#\./##' -e 's#/R##' )
  LIB=$( grep -h 'library(.*)' $f/*.R 2>/dev/null | grep -v 'PEcAn' | grep -v '^#' | sed -e 's/.*library("*\([A-Za-z0-9\.]*\).*/\1/' | sort -u )
  LIB=$( echo $LIB )

  REQ=$( grep -h 'require(.*)' $f/*.R 2>/dev/null | grep -v 'PEcAn' | grep -v '^#' | sed -e 's/.*require("*\([A-Za-z0-9\.]*\).*/\1/' | sort -u )
  REQ=$( echo $REQ )

  PACKAGE=$( echo $LIB $REQ | tr -s [:space:] \\n | sort -u )
  TEMP=$( echo $PACKAGE)
  echo "packages needed for $NAME : $TEMP"

#  for s in $SKIP; do
#    PACKAGE=$( echo $PACKAGE | grep -v $s )
#  done
  ALL="${ALL} ${PACKAGE}"
done

echo $ALL

# sort packages and create little R script
if [ ! -z "$ALL" ]; then
  ALL=$( echo $ALL | tr -s [:space:] \\n | sort -u | egrep -v "${SKIP}" )
  ALL=$( echo $ALL )
  ALL=$( echo "'$ALL'" | sed -e "s/ /', '/g" )
  echo "Make sure following packages are installed : "
  echo "$ALL"
  echo ""

  cat << EOF
echo "list.of.packages <- c($ALL)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,'Package'])]
if('BioCro' %in% new.packages){
  biocro <- TRUE
  new.packages <- new.packages[!new.packages %in% "BioCro"]
} else {
  biocro <- FALSE
}
if(length(new.packages)) {
  print('installing : ')
  print(new.packages)
  install.packages(new.packages, repos='http://cran.us.r-project.org')
} 
if(biocro){
   devtools::install_github("ebimodeling/biocro")
} | R --vanilla "
EOF
fi
