###Initialization file to set up system for PECAn
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
###new commands added to .bashrc: buildpecan

    ##set environment variables
if ! grep buildpecan ~/.bashrc > /dev/null
then
    echo "alias buildpecan='/home/$USER/pecan/bash/pecanbuild.sh'" >> ~/.bashrc
fi

## make sure that appropriate software is installed
apt-get install -y python-software-properties
add-apt-repository ppa:marutter/rrutter
apt-get update

apt-get install -y emacs23 mysql-server mysql-client libdbd-mysql libmysqlclient16-dev r-base jags r-cran-rjags r-cran-xml r-cran-hdf5 r-cran-mass r-cran-rmysql --no-install-recommends

R --vanilla < ~/pecan/rscripts/install.dependencies.R 