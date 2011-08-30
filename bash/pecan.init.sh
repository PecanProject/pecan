###Initialization file to set up system for PECAn
###new commands added to .bashrc: buildpecan

if [ ! -d ~/lib ]
then
    mkdir ~/lib/ 
fi

if [ ! -d ~/lib/R ]
then
    mkdir ~/lib/R
fi

## move .my.cnf_forecast to /home/user/.my.cnf
if [ ! -f ~/.my.cnf ]
then 
    cp .my.cnf_forecast .my.cnf
fi

if ! grep kepler ~/.bashrc > /dev/null
then 
    KEPALIAS='/usr/local/Kepler-2.0/kepler.sh'
    echo "alias kepler='$KEPALIAS'" >> ~/.bashrc
fi

    ##set environment variables
if ! grep buildpecan ~/.bashrc > /dev/null
then
    echo "alias buildpecan='/home/$USER/pecan/bash/pecanbuild.sh'" >> ~/.bashrc
fi

## make sure that appropriate software is installed
add-apt-repository ppa:marutter/rrutter
apt-get update
apt-get install mysql-server mysql-client libdbd-mysql libmysqlclient16-dev r-base jags r-cran-rjags r-cran-xml
R --vanilla < ~/pecan/rscripts/pecan.init.R