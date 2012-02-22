###Initialization file to set up system for PECAn
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