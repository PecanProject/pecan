#!/bin/bash
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

ALL="ggplot2 randtoolbox gridExtra testthat"
for f in `find . -type d -name R`; do
  NAME=$( echo $f | sed -e 's#\./##' -e 's#/R##' )
  LIB=$( grep 'library(.*)' $f/*.R 2>/dev/null | grep -v 'PEcAn' | grep -v '^#' | sed -e 's/.*library(\("*[A-Za-z0-9\.]*"*\).*/\1/' | sort -u )
  LIB=$( echo $LIB )

  REQ=$( grep 'require(.*)' $f/*.R 2>/dev/null | grep -v 'PEcAn' | grep -v '^#' | sed -e 's/.*require(\("*[A-Za-z0-9\.]*"*\).*/\1/' | sort -u )
  REQ=$( echo $REQ )

  PACKAGE=$( echo $LIB $REQ | tr -s [:space:] \\n | sort -u )
  PACKAGE=$( echo $PACKAGE)
  if [ ! -z "$PACKAGE" ]; then
    echo "packages needed for $NAME : $PACKAGE"
  fi

  ALL="${ALL} ${PACKAGE}"
done

if [ ! -z "$ALL" ]; then
  ALL=$( echo $ALL | tr -s [:space:] \\n | sort -u )
  ALL=$( echo $ALL )
  ALL=$( echo "'$ALL'" | sed -e "s/ /', '/g" )
  echo "Make sure following packages are installed : "
  echo "$ALL"
  echo ""

  cat << EOF
echo "list.of.packages <- c($ALL)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,'Package'])]
if(length(new.packages)) {
  print('installing : ')
  print(new.packages)
  install.packages(new.packages, lib=Sys.getenv('R_LIBS_USER'), repos='http://cran.us.r-project.org')
} " | R --vanilla
EOF
fi
