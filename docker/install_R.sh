#!/bin/bash

. /build/install_pecan_preprocessor.sh

echo "######################################################################"
echo "R"
echo "######################################################################"
if [ -z "${R_LIBS_USER}" ]; then
  echo 'export R_LIBS_USER=${HOME}/R/library' >> ${HOME}/.bashrc
  echo 'R_LIBS_USER=${HOME}/R/library' >> ${HOME}/.Renviron
  export export R_LIBS_USER=${HOME}/R/library
  mkdir -p ${R_LIBS_USER}

  case "$OS_VERSION" in
    RH_*)
      echo 'export PATH=${PATH}:/usr/pgsql-9.5/bin' >> ${HOME}/.bashrc
      export PATH=${PATH}:/usr/pgsql-9.5/bin
      ;;
  esac
fi
echo 'if(!"devtools" %in% installed.packages()) install.packages("devtools", repos="http://cran.rstudio.com/")' | R --vanilla
echo 'if(!"udunits2" %in% installed.packages()) install.packages("udunits2", configure.args=c(udunits2="--with-udunits2-include=/usr/include/udunits2"), repo="http://cran.rstudio.com")'  | R --vanilla

# packages for BrownDog shiny app
echo 'if(!"leaflet" %in% installed.packages()) install.packages("leaflet", repos="http://cran.rstudio.com/")' | R --vanilla
echo 'if(!"RJSONIO" %in% installed.packages()) install.packages("RJSONIO", repos="http://cran.rstudio.com/")' | R --vanilla

#echo 'update.packages(repos="http://cran.rstudio.com/", ask=FALSE)' |  R --vanilla
echo 'x <- rownames(old.packages(repos="http://cran.rstudio.com/")); update.packages(repos="http://cran.rstudio.com/", ask=FALSE, oldPkgs=x[!x %in% "rgl"])' |  R --vanilla

#echo 'update.packages(repos="http://cran.rstudio.com/", ask=FALSE)' | R --vanilla
echo 'x <- rownames(old.packages(repos="http://cran.rstudio.com/")); update.packages(repos="http://cran.rstudio.com/", ask=FALSE, oldPkgs=x[!x %in% "rgl"])' | R --vanilla
