#!/bin/bash

. /build/install_pecan_preprocessor.sh

echo "######################################################################"
echo "SETTING UP REPOS"
echo "######################################################################"
case "$OS_VERSION" in
  RH_5)
     yum install -y wget
     wget -O /etc/yum.repos.d/cornell.repo http://download.opensuse.org/repositories/home:cornell_vrdc/CentOS_CentOS-6/home:cornell_vrdc.repo
     rpm -Uvh http://download.fedoraproject.org/pub/epel/5/x86_64/epel-release-5-4.noarch.rpm
    ;;
  RH_6)
     yum install -y wget
     wget -O /etc/yum.repos.d/cornell.repo http://download.opensuse.org/repositories/home:cornell_vrdc/CentOS_CentOS-6/home:cornell_vrdc.repo
     yum -y localinstall https://download.postgresql.org/pub/repos/yum/9.5/redhat/rhel-6-x86_64/pgdg-centos95-9.5-2.noarch.rpm
     rpm -Uvh http://download.fedoraproject.org/pub/epel/6/x86_64/epel-release-6-8.noarch.rpm
    ;;
  RH_7)
     yum install -y wget
     wget -O /etc/yum.repos.d/cornell.repo wget http://download.opensuse.org/repositories/home:cornell_vrdc/CentOS_7/home:cornell_vrdc.repo
     yum -y localinstall https://download.postgresql.org/pub/repos/yum/9.5/redhat/rhel-7-x86_64/pgdg-centos95-9.5-2.noarch.rpm
     rpm -Uvh http://download.fedoraproject.org/pub/epel/7/x86_64/e/epel-release-7-6.noarch.rpm
    setsebool -P httpd_can_network_connect 1
    ;;
  Ubuntu)
    # if [ ! -e /etc/apt/sources.list.d/R.list ]; then
    #    sh -c 'echo "deb http://cran.rstudio.com/bin/linux/ubuntu `lsb_release -s -c`/" > /etc/apt/sources.list.d/R.list'
    #    apt-key adv --keyserver keyserver.ubuntu.com --recv E084DAB9
    # fi
    if [ ! -e /etc/apt/sources.list.d/ruby.list ]; then
       sh -c 'echo "deb http://ppa.launchpad.net/brightbox/ruby-ng/ubuntu xenial main" > /etc/apt/sources.list.d/ruby.list'
       apt-key adv --keyserver keyserver.ubuntu.com --recv C3173AA6
    fi
    # if [ ! -e /etc/apt/sources.list.d/pgdg.list ]; then
    #    sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt `lsb_release -s -c`-pgdg main" > /etc/apt/sources.list.d/pgdg.list'
    #   wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc |  apt-key add -
    # fi
     apt-get -qq -y update
    ;;
esac

echo "######################################################################"
echo "INSTALLING PACKAGES"
echo "######################################################################"
case "$OS_VERSION" in
  RH_*)
     yum install -y git R gfortran openssl-devel
     yum install -y openmpi openmpi-devel netcdf netcdf-openmpi netcdf-devel netcdf-openmpi-devel netcdf-fortran-devel netcdf-fortran-openmpi
     ln -s /usr/lib64/openmpi/bin/mpicc /usr/bin/mpicc
     ln -s /usr/lib64/openmpi/bin/mpif90 /usr/bin/mpif90
    # for ED
     yum install -y hdf5-openmpi-devel
    # for LPJ-GUESS
     yum install -y cmake
    # for DALEC
     yum install -y gsl-devel liblas-devel lapack-devel
    # for PEcAn
     yum install -y ed libpng-devel libpng12-devel libjpeg-turbo-devel jags4 jags4-devel python-devel udunits2-devel gdal-devel proj-devel proj-devel proj-nad proj-epsg libxml2-devel udunits2-devel gmp-devel
    # for PostgreSQL
     yum install -y postgresql95-server postgresql95-devel postgis2_95
    # web gui
     yum install -y httpd php php-pgsql php-xml
    ;;
  Ubuntu)
     apt-get -y install build-essential gfortran git r-base-core r-base r-base-dev jags liblapack-dev libnetcdf-dev netcdf-bin bc libcurl4-gnutls-dev curl udunits-bin libudunits2-dev libgmp-dev python-dev libgdal1-dev libproj-dev expect
     apt-get -y install openmpi-bin libopenmpi-dev
     apt-get -y install libgsl0-dev libssl-dev
    #
     apt-get -y install r-cran-ggplot2
    # for maeswrap
     apt-get -y install r-cran-rgl
    # for R doc
     apt-get -y install texinfo texlive-latex-base texlive-latex-extra texlive-fonts-recommended
    # ruby
     apt-get -y install ruby2.1 ruby2.1-dev
    # for LPJ-GUESS
     apt-get -y install cmake
    # for PostgreSQL
    # apt-get -y install libdbd-pgsql postgresql-9.5 postgresql-client-9.5 libpq-dev postgresql-9.5-postgis-2.2 postgresql-9.5-postgis-scripts
     apt-get -y install postgresql-client-9.5
    # for web gui
    # apt-get -y install apache2 libapache2-mod-php7.0 php7.0 libapache2-mod-passenger php7.0-xml php-ssh2 php7.0-pgsql
    # Ubuntu 14.04 php5-pgsql libapache2-mod-php5 php5 and no php-xml
    ;;
esac
