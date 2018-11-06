#!/bin/bash

set -e

if [ "`whoami`" == "root" ]; then
  echo "Don't run this script as root"
  exit -1
fi

# configuration
BROWNDOG_URL="http://dap.ncsa.illinois.edu:8184/convert/";
BROWNDOG_USERNAME="";
BROWNDOG_PASSWORD="";

GOOGLE_MAP_KEY=""

SETUP_VM=""
SETUP_PALEON=""
REBUILD=""

PECAN_BRANCH=""

RSTUDIO_SERVER="1.1.456"
SHINY_SERVER="1.5.7.907"

if [ -e $(dirname $0)/install_pecan.config ]; then
  . $(dirname $0)/install_pecan.config
fi


if [ -e /etc/redhat-release ]; then
  OS_VERSION="RH_$( sed -r 's/.* ([0-9]+)\..*/\1/' /etc/redhat-release )"
  HTTP_CONF="/etc/httpd/conf.d/"
  chmod o+x ${HOME}
else
  OS_VERSION="Ubuntu"
  HTTP_CONF="/etc/apache2/conf-available/"
fi

# actual install/update
echo "######################################################################"
echo "UPDATING MACHINE"
echo "######################################################################"
chmod 755 /home/carya/
case "$OS_VERSION" in
  RH_*)
    sudo yum update -y
    if [ "$SETUP_VM" != "" ]; then
      sudo sed -i -e "s/^127.0.0.1 .*\$/127.0.0.1 ${HOSTNAME}.pecan ${HOSTNAME} localhost localhost.localdomain localhost4 localhost4.localdomain4/" /etc/hosts
    fi
    ;;
  Ubuntu)
    sudo apt-get -qq -y update
    sudo apt-get -y dist-upgrade
    sudo apt-get -y purge --auto-remove
    if [ "$SETUP_VM" != "" ]; then
      sudo sed -i -e "s/^127.0.0.1[ \t].*\$/127.0.0.1\t${HOSTNAME}.vm ${HOSTNAME} localhost/" /etc/hosts
    fi
    ;;
  *)
    echo "Unknown OS"
    exit 1
    ;;
esac

echo "######################################################################"
echo "SETTING UP REPOS"
echo "######################################################################"
case "$OS_VERSION" in
  RH_5)
    sudo yum install -y wget
    sudo wget -O /etc/yum.repos.d/cornell.repo http://download.opensuse.org/repositories/home:cornell_vrdc/CentOS_CentOS-6/home:cornell_vrdc.repo
    sudo rpm -Uvh http://download.fedoraproject.org/pub/epel/5/x86_64/epel-release-5-4.noarch.rpm
    ;;
  RH_6)
    sudo yum install -y wget
    sudo wget -O /etc/yum.repos.d/cornell.repo http://download.opensuse.org/repositories/home:cornell_vrdc/CentOS_CentOS-6/home:cornell_vrdc.repo
    sudo yum -y localinstall https://download.postgresql.org/pub/repos/yum/9.5/redhat/rhel-6-x86_64/pgdg-centos95-9.5-2.noarch.rpm
    sudo rpm -Uvh http://download.fedoraproject.org/pub/epel/6/x86_64/epel-release-6-8.noarch.rpm
    ;;
  RH_7)
    sudo yum install -y wget
    sudo wget -O /etc/yum.repos.d/cornell.repo wget http://download.opensuse.org/repositories/home:cornell_vrdc/CentOS_7/home:cornell_vrdc.repo
    sudo yum -y localinstall https://download.postgresql.org/pub/repos/yum/9.5/redhat/rhel-7-x86_64/pgdg-centos95-9.5-2.noarch.rpm
    sudo rpm -Uvh http://download.fedoraproject.org/pub/epel/7/x86_64/e/epel-release-7-6.noarch.rpm
    setsebool -P httpd_can_network_connect 1
    ;;
  Ubuntu)
    # if [ ! -e /etc/apt/sources.list.d/R.list ]; then
    #   sudo sh -c 'echo "deb http://cran.rstudio.com/bin/linux/ubuntu `lsb_release -s -c`/" > /etc/apt/sources.list.d/R.list'
    #   sudo apt-key adv --keyserver keyserver.ubuntu.com --recv E084DAB9
    # fi
    #if [ ! -e /etc/apt/sources.list.d/ruby.list ]; then
    #  sudo sh -c 'echo "deb http://ppa.launchpad.net/brightbox/ruby-ng/ubuntu trusty main" > /etc/apt/sources.list.d/ruby.list'
    #  sudo apt-key adv --keyserver keyserver.ubuntu.com --recv C3173AA6
    #fi
    if [ ! -e /etc/apt/sources.list.d/pgdg.list ]; then
      sudo sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt `lsb_release -s -c`-pgdg main" > /etc/apt/sources.list.d/pgdg.list'
      wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -
    fi
    sudo apt-get -qq -y update
    ;;
esac

echo "######################################################################"
echo "INSTALLING PACKAGES"
echo "######################################################################"
case "$OS_VERSION" in
  RH_*)
    sudo yum install -y git R gfortran openssl-devel
    sudo yum install -y openmpi openmpi-devel netcdf netcdf-openmpi netcdf-devel netcdf-openmpi-devel netcdf-fortran-devel netcdf-fortran-openmpi
    sudo ln -s /usr/lib64/openmpi/bin/mpicc /usr/bin/mpicc
    sudo ln -s /usr/lib64/openmpi/bin/mpif90 /usr/bin/mpif90
    # for ED
    sudo yum install -y hdf5-openmpi-devel
    # for LPJ-GUESS
    sudo yum install -y cmake
    # for DALEC
    sudo yum install -y gsl-devel liblas-devel lapack-devel
    # for PEcAn
    sudo yum install -y ed libpng-devel libpng12-devel libjpeg-turbo-devel jags4 jags4-devel python-devel udunits2-devel gdal-devel proj-devel proj-devel proj-nad proj-epsg libxml2-devel udunits2-devel gmp-devel
    # for PostgreSQL
    sudo yum install -y postgresql95-server postgresql95-devel postgis2_95
    # web gui
    sudo yum install -y httpd php php-pgsql php-xml expect expectk
    ;;
  Ubuntu)
    sudo apt-get -y install build-essential gfortran git r-base-core jags liblapack-dev libnetcdf-dev netcdf-bin bc libcurl4-gnutls-dev curl udunits-bin libudunits2-dev libgmp-dev python-dev libgdal-dev libproj-dev expect librdf0-dev
    sudo apt-get -y install openmpi-bin libopenmpi-dev
    sudo apt-get -y install libgsl0-dev libssl-dev
    # for maeswrap
    sudo apt-get -y install r-cran-rgl
    # for R doc
    sudo apt-get -y install texinfo texlive-latex-base texlive-latex-extra texlive-fonts-recommended
    # BETY
    sudo apt-get -y install ruby ruby-dev nodejs
    # for LPJ-GUESS
    sudo apt-get -y install cmake
    # for PostgreSQL
    sudo apt-get -y install libdbd-pgsql postgresql-9.5 postgresql-client-9.5 libpq-dev postgresql-9.5-postgis-2.2 postgresql-9.5-postgis-scripts
    # for web gui
    sudo apt-get -y install apache2 libapache2-mod-php php libapache2-mod-passenger php-xml php-ssh2 php-pgsql
    # Ubuntu 14.04 php5-pgsql libapache2-mod-php5 php5 and no php-xml
    ;;
esac

echo "######################################################################"
echo "POSTGRES"
echo "######################################################################"
#    ADD export PATH=${PATH}:/usr/pgsql-9.5/bin
#    ADD exclude=postgresql* to /etc/yum.repos.d/CentOS-Base.repo or /etc/yum/pluginconf.d/rhnplugin.conf
#    SEE https://wiki.postgresql.org/wiki/YUM_Installation#Configure_your_YUM_repository
case "$OS_VERSION" in
  RH_5)
    echo "No PostgreSQL configuration (yet) for RedHat 5"
    exit 1
    ;;
  RH_6)
    sudo service postgresql-9.5 initdb
    sudo sh -c 'if ! grep -Fq "bety" /var/lib/pgsql/9.5/data/pg_hba.conf ; then
      sed -i "/# TYPE/ a\
local   all             bety                                    trust\n\
host    all             bety            127.0.0.1/32            trust\n\
host    all             bety            ::1/128                 trust" /var/lib/pgsql/9.5/data/pg_hba.conf
    fi'
    chkconfig postgresql-9.5 on
    sudo service postgresql-9.5 start
    ;;
  RH_7)
    sudo /usr/pgsql-9.5/bin/postgresql95-setup initdb
    sudo sh -c 'if ! grep -Fq "bety" /var/lib/pgsql/9.5/data/pg_hba.conf ; then
      sed -i "/# TYPE/ a\
local   all             bety                                    trust\n\
host    all             bety            127.0.0.1/32            trust\n\
host    all             bety            ::1/128                 trust" /var/lib/pgsql/9.5/data/pg_hba.conf
    fi'
    sudo systemctl enable postgresql-9.5.service
    sudo systemctl start postgresql-9.5.service
    ;;
  Ubuntu)
    sudo sh -c 'if ! grep -Fq "bety" /etc/postgresql/9.5/main/pg_hba.conf ; then
      sed -i "/# TYPE/ a\
local   all             bety                                    trust\n\
host    all             bety            127.0.0.1/32            trust\n\
host    all             bety            ::1/128                 trust" /etc/postgresql/9.5/main/pg_hba.conf
fi'
    sudo service postgresql restart
    ;;
esac

echo "######################################################################"
echo "R"
echo "######################################################################"
if [ -z "${R_LIBS_USER}" ]; then
  echo 'export R_LIBS_USER=${HOME}/R/library' >> ${HOME}/.bashrc
  echo 'R_LIBS_USER=${HOME}/R/library' >> ${HOME}/.Renviron
  export export R_LIBS_USER=${HOME}/R/library
  mkdir -p ${R_LIBS_USER}

  echo "options(shiny.port = 6438)" >> ${HOME}/.Rprofile
  echo "options(shiny.launch.browser = 'FALSE')" >> ${HOME}/.Rprofile

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

# packages for other shiny apps
echo 'if(!"DT" %in% installed.packages()) install.packages("DT", repos="http://cran.rstudio.com/")' | R --vanilla

#echo 'update.packages(repos="http://cran.rstudio.com/", ask=FALSE)' | sudo R --vanilla
echo 'x <- rownames(old.packages(repos="http://cran.rstudio.com/")); update.packages(repos="http://cran.rstudio.com/", ask=FALSE, oldPkgs=x[!x %in% "rgl"])' | sudo R --vanilla

#echo 'update.packages(repos="http://cran.rstudio.com/", ask=FALSE)' | R --vanilla
echo 'x <- rownames(old.packages(repos="http://cran.rstudio.com/")); update.packages(repos="http://cran.rstudio.com/", ask=FALSE, oldPkgs=x[!x %in% "rgl"])' | R --vanilla

echo "######################################################################"
echo "ED"
echo "######################################################################"
if [ ! -e ${HOME}/ED2 ]; then
  cd
  git clone https://github.com/EDmodel/ED2.git
  cd ${HOME}/ED2/ED/build
  curl -o make/include.mk.VM http://isda.ncsa.illinois.edu/~kooper/EBI/include.mk.opt.`uname -s`
  if [ "$OS_VERSION" == "RH_6" ]; then
    sed -i 's/ -fno-whole-file//' make/include.mk.opt.VM
  fi
fi
cd ${HOME}/ED2
git pull
cd ED/build
./install.sh -g -p VM -c
./install.sh -g -p VM
sudo cp ed_2.1-opt /usr/local/bin/ed2.git
./install.sh -g -p VM -c

echo "######################################################################"
echo "SIPNET"
echo "######################################################################"
if [ ! -e ${HOME}/sipnet_unk ]; then
  cd
  curl -o sipnet_unk.tar.gz http://isda.ncsa.illinois.edu/~kooper/PEcAn/sipnet/sipnet_unk.tar.gz
  tar zxf sipnet_unk.tar.gz
  rm sipnet_unk.tar.gz
fi
cd ${HOME}/sipnet_unk/
make clean
make
sudo cp sipnet /usr/local/bin/sipnet.runk
make clean

if [ ! -e ${HOME}/sipnet_r136 ]; then
  cd
  curl -o sipnet_r136.tar.gz http://isda.ncsa.illinois.edu/~kooper/PEcAn/sipnet/sipnet_r136.tar.gz
  tar zxf sipnet_r136.tar.gz
  rm sipnet_r136.tar.gz
  sed -i 's#$(LD) $(LIBLINKS) \(.*\)#$(LD) \1 $(LIBLINKS)#' ${HOME}/sipnet_r136/Makefile
fi
cd ${HOME}/sipnet_r136/
make clean
make
sudo cp sipnet /usr/local/bin/sipnet.r136
make clean

echo "######################################################################"
echo "MAESPA"
echo "######################################################################"
if [ ! -e ${HOME}/maespa ]; then
  cd
  git clone https://bitbucket.org/remkoduursma/maespa.git
  echo 'install.packages("Maeswrap", repo="http://cran.rstudio.com")'  | R --vanilla
fi
cd ${HOME}/maespa
make clean
make
sudo cp maespa.out /usr/local/bin/maespa.git
make clean

echo "######################################################################"
echo "BioCro"
echo "######################################################################"
echo 'devtools::install_github("ebimodeling/biocro")' | R --vanilla

echo "######################################################################"
echo "Linkages"
echo "######################################################################"
echo 'devtools::install_github("araiho/linkages_package")' | R --vanilla

echo "######################################################################"
echo "Preles"
echo "######################################################################"
echo 'devtools::install_url("http://isda.ncsa.illinois.edu/~kooper/PEcAn/models/Rpreles_1.0.tar.gz")' | R --vanilla

echo "######################################################################"
echo "DALEC"
echo "######################################################################"
if [ ! -e ${HOME}/dalec_EnKF_pub ]; then
  cd
  curl -o dalec_EnKF_pub.tgz http://isda.ncsa.illinois.edu/~kooper/EBI/dalec_EnKF_pub.tgz
  tar zxf dalec_EnKF_pub.tgz
  rm dalec_EnKF_pub.tgz
fi
cd ${HOME}/dalec_EnKF_pub
make clean
make dalec_EnKF
make dalec_seqMH
sudo cp dalec_EnKF dalec_seqMH /usr/local/bin
make clean

echo "######################################################################"
echo "GDAY"
echo "######################################################################"
if [ ! -e ${HOME}/GDAY ]; then
  cd
  git clone https://github.com/mdekauwe/GDAY.git
fi
cd ${HOME}/GDAY
git pull
cd src
make
sudo cp gday /usr/local/bin
make clean

echo "######################################################################"
echo "PECAN"
echo "######################################################################"
if [ ! -e ${HOME}/pecan ]; then
  cd
  git clone https://github.com/PecanProject/pecan.git
fi
cd ${HOME}/pecan
git pull
if [ "${PECAN_BRANCH}" != "" ]; then
  git checkout ${PECAN_BRANCH}
fi
make

sudo curl -o /var/www/html/pecan.pdf https://www.gitbook.com/download/pdf/book/pecan/pecan-documentation
sudo rm /var/www/html/index.html
sudo ln -s  ${HOME}/pecan/documentation/index_vm.html /var/www/html/index.html

if [ ! -e ${HOME}/pecan/web/config.php ]; then
  sed -e "s#browndog_url=.*#browndog_url=\"${BROWNDOG_URL}\";#" \
      -e "s#browndog_username=.*#browndog_username=\"${BROWNDOG_USERNAME}\";#" \
      -e "s#browndog_password=.*#browndog_password=\"${BROWNDOG_PASSWORD}\";#" \
      -e "s#googleMapKey=.*#googleMapKey=\"${GOOGLE_MAP_KEY}\";#" \
      -e "s/carya/$USER/g" ${HOME}/pecan/web/config.example.php > ${HOME}/pecan/web/config.php
fi

if [ ! -e ${HTTP_CONF}/pecan.conf ]; then
  cat > /tmp/pecan.conf << EOF
Alias /pecan ${HOME}/pecan/web
<Directory ${HOME}/pecan/web>
  DirectoryIndex index.php
  Options +ExecCGI
  Require all granted
</Directory>
EOF
  sudo cp /tmp/pecan.conf ${HTTP_CONF}/pecan.conf
  rm /tmp/pecan.conf
fi

echo "######################################################################"
echo "BETY"
echo "######################################################################"
if [ ! -e ${HOME}/bety ]; then
  cd
  git clone https://github.com/PecanProject/bety.git
fi
cd ${HOME}/bety
git pull
sudo gem install bundler
bundle install --without development:test:javascript_testing:debug --path vendor/bundle

if [ ! -e paperclip/files ]; then
  mkdir -p paperclip/files
  chmod 777 paperclip/files
fi
if [ ! -e paperclip/file_names ]; then
  mkdir -p paperclip/file_names
  chmod 777 paperclip/file_names
fi

if [ ! -e log ]; then
  mkdir log
  touch log/production.log
  chmod 0666 log/production.log
fi

# chmod go+w public/javascripts/cache/

if [ ! -e config/database.yml ]; then
  cat > config/database.yml << EOF
production:
  adapter: postgis
  encoding: utf-8
  reconnect: false
  database: bety
  pool: 5
  username: bety
  password: bety
EOF
fi

if [ ! -e ${HTTP_CONF}/bety.conf ]; then
  cat > /tmp/bety.conf << EOF
RailsEnv production
RailsBaseURI /bety
PassengerRuby /usr/bin/ruby
SetEnv SECRET_KEY_BASE $(bundle exec rake secret)
<Directory /var/www/html/bety>
  Options +FollowSymLinks
  Require all granted
</Directory>
EOF
  sudo cp /tmp/bety.conf ${HTTP_CONF}/bety.conf
  rm /tmp/bety.conf
  sudo ln -s $HOME/bety/public /var/www/html/bety

  # sudo sh -c "echo '\n## BETY secret key\nexport SECRET_KEY_BASE=$(cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 120 | head -n 1)' >> /etc/apache2/envvars"
  # cp $HOME/bety/public/javascripts/cache/all.js-sample $HOME/bety/public/javascripts/cache/all.js
fi

bundle exec rake assets:precompile RAILS_ENV=production RAILS_RELATIVE_URL_ROOT=/bety

echo "######################################################################"
echo "DATA"
echo "######################################################################"
cd ${HOME}

if [ ! -e ${HOME}/sites ]; then
  curl -s -o sites.tgz http://isda.ncsa.illinois.edu/~kooper/EBI/sites.tgz
  tar zxf sites.tgz
  sed -i -e "s#/home/kooper/Projects/EBI#${HOME}#" sites/*/ED_MET_DRIVER_HEADER
  rm sites.tgz
fi

if [ ! -e ${HOME}/inputs ]; then
  curl -s -o inputs.tgz http://isda.ncsa.illinois.edu/~kooper/EBI/inputs.tgz
  tar zxf inputs.tgz
  rm inputs.tgz
fi

if [ ! -e ${HOME}/lpj-guess/cru_1901_2006.bin ]; then
  if [ ! -d ${HOME}/lpj-guess ]; then
    mkdir ${HOME}/lpj-guess
  fi
  curl -s -o ${HOME}/lpj-guess/cru_1901_2006.bin http://isda.ncsa.illinois.edu/~kooper/PEcAn/data/cru_1901_2006.bin
fi

if [ ! -e ${HOME}/plot ]; then
  curl -s -o plot.tgz http://isda.ncsa.illinois.edu/~kooper/EBI/plot.tgz
  tar zxf plot.tgz
  rm plot.tgz
fi

if [ ! -e ${HOME}/sites/Santarem_Km83 ]; then
  curl -s -o Santarem_Km83.zip http://isda.ncsa.illinois.edu/~kooper/EBI/Santarem_Km83.zip
  unzip -q -d sites Santarem_Km83.zip
  sed -i -e "s#/home/pecan#${HOME}#" sites/Santarem_Km83/ED_MET_DRIVER_HEADER
  rm Santarem_Km83.zip
fi

if [ ! -e ${HOME}/testrun.s83 ]; then
  curl -s -o testrun.s83.zip http://isda.ncsa.illinois.edu/~kooper/EBI/testrun.s83.zip
  unzip -q testrun.s83.zip
  sed -i -e "s#/home/pecan#${HOME}#" testrun.s83/ED2IN
  rm testrun.s83.zip
fi

if [ ! -e ${HOME}/ed2ws.harvard ]; then
  curl -s -o ed2ws.harvard.tgz http://isda.ncsa.illinois.edu/~kooper/EBI/ed2ws.harvard.tgz
  tar zxf ed2ws.harvard.tgz
  mkdir ed2ws.harvard/analy ed2ws.harvard/histo
  sed -i -e "s#/home/pecan#${HOME}#g" ed2ws.harvard/input_harvard/met_driver/HF_MET_HEADER ed2ws.harvard/ED2IN ed2ws.harvard/*.r
  rm ed2ws.harvard.tgz
fi

if [ ! -e ${HOME}/testrun.PDG ]; then
  curl -s -o testrun.PDG.zip http://isda.ncsa.illinois.edu/~kooper/EBI/testrun.PDG.zip
  unzip -q testrun.PDG.zip
  sed -i -e "s#/home/pecan#${HOME}#" testrun.PDG/Met/PDG_MET_DRIVER testrun.PDG/Template/ED2IN
  sed -i -e 's#/n/scratch2/moorcroft_lab/kzhang/PDG/WFire_Pecan/##' testrun.PDG/Template/ED2IN
  rm testrun.PDG.zip
fi

if [ ! -e ${HOME}/create_met_driver ]; then
  curl -s -o create_met_driver.tar.gz http://isda.ncsa.illinois.edu/~kooper/EBI/create_met_driver.tar.gz
  tar zxf create_met_driver.tar.gz
  rm create_met_driver.tar.gz
fi

echo "######################################################################"
echo "DATABASE"
echo "######################################################################"
FOUND_BETY=$( sudo -H -u postgres psql -lqt | awk '/^ bety / { print $1 }' )
if [ -z "$FOUND_BETY" ]; then
  sudo -H -u postgres psql -c "CREATE ROLE bety WITH LOGIN CREATEDB NOSUPERUSER NOCREATEROLE PASSWORD 'bety';"
  sudo -H -u postgres psql -c "CREATE DATABASE bety OWNER bety;"
fi

sudo rm -rf ${HOME}/output
mkdir ${HOME}/output
chmod 2777 ${HOME}/output
sudo -u postgres ${HOME}/pecan/scripts/load.bety.sh -g -m 99 -r 0 -c -u -w https://ebi-forecast.igb.illinois.edu/pecan/dump/all//bety.tar.gz
${HOME}/pecan/scripts/add.models.sh
${HOME}/pecan/scripts/add.data.sh

echo "######################################################################"
echo "RSTUDIO SERVER"
echo "######################################################################"
if [ "${RSTUDIO_SERVER}" != "" ]; then
  case "$OS_VERSION" in
    RH_*)
      curl -s -o rstudio.rpm https://download2.rstudio.org/rstudio-server-rhel-${RSTUDIO_SERVER}-x86_64.rpm
      sudo yum -y install --nogpgcheck rstudio.rpm
      rm rstudio.rpm
      ;;
    Ubuntu)
      sudo apt-get -y install gdebi-core
      if [ $( uname -m ) == "x86_64" ]; then
        curl -s -o rstudio.deb https://download2.rstudio.org/rstudio-server-${RSTUDIO_SERVER}-amd64.deb
      elif [ $( uname -m ) == "i686" ]; then
        curl -s -o rstudio.deb https://download2.rstudio.org/rstudio-server-${RSTUDIO_SERVER}-i386.deb
      fi
      sudo gdebi -q -n rstudio.deb
      rm rstudio.deb
      ;;
  esac

  if [ ! -e ${HTTP_CONF}/rstudio.conf  ]; then
    sudo sh -c 'echo "www-address=127.0.0.1" >> /etc/rstudio/rserver.conf'
    sudo sh -c 'echo "r-libs-user=~/R/library" >> /etc/rstudio/rsession.conf'

    cat > /tmp/rstudio.conf << EOF
ProxyPass        /rstudio/ http://localhost:8787/
ProxyPassReverse /rstudio/ http://localhost:8787/
RedirectMatch permanent ^/rstudio$ /rstudio/
EOF
    sudo cp /tmp/rstudio.conf ${HTTP_CONF}/rstudio.conf
    rm /tmp/rstudio.conf
  fi
fi

echo "######################################################################"
echo "SHINY SERVER"
echo "######################################################################"
if [ "${SHINY_SERVER}" != "" -a $( uname -m ) == "x86_64" ]; then
  sudo su - -c "R -e \"install.packages(c('rmarkdown', 'shiny'), repos='https://cran.rstudio.com/')\""

  R -e "install.packages(c('https://www.bioconductor.org/packages/release/bioc/src/contrib/BiocGenerics_0.26.0.tar.gz', 'http://www.bioconductor.org/packages/release/bioc/src/contrib/graph_1.58.0.tar.gz'), repos=NULL)"
  R -e "devtools::install_github('duncantl/CodeDepends')"
  R -e "devtools::install_github('OakleyJ/SHELF')"
  R -e "install.packages(c('shinythemes', 'shinytoastr', 'SHELF', 'shiny'), repos='https://cran.rstudio.com/')"

  case "$OS_VERSION" in
    RH_*)
      curl -s -o shiny.rpm https://download3.rstudio.org/centos5.9/x86_64/shiny-server-${SHINY_SERVER}-rh5-x86_64.rpm
      sudo yum -y install --nogpgcheck shiny.rpm
      rm shiny.rpm
      ;;
    Ubuntu)
      sudo apt-get -y install gdebi-core
      curl -s -o shiny.deb https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-${SHINY_SERVER}-amd64.deb
      sudo gdebi -q -n shiny.deb
      rm shiny.deb
      ;;
  esac

  if [ ! -e ${HTTP_CONF}/shiny.conf ]; then
    cat > /tmp/shiny-server.conf << EOF
run_as shiny;
server {
  listen 3838;
  location / {
    run_as ${USER};
    site_dir ${HOME}/pecan/shiny;
    log_dir /var/log/shiny-server;
    directory_index on;
  }
}
EOF
    sudo cp /tmp/shiny-server.conf /etc/shiny-server/shiny-server.conf
    rm /tmp/shiny-server.conf

    cat > /tmp/shiny.conf << EOF
ProxyPass        /shiny/ http://localhost:3838/
ProxyPassReverse /shiny/ http://localhost:3838/
RedirectMatch permanent ^/shiny$ /shiny/
EOF
    sudo cp /tmp/shiny.conf ${HTTP_CONF}/shiny.conf
    rm /tmp/shiny.conf
  fi
fi

echo "######################################################################"
echo "RESTART SERVICES"
echo "######################################################################"
case "$OS_VERSION" in
  RH_6)
    cat > /tmp/iptables << EOF
*filter
:INPUT ACCEPT [0:0]
:FORWARD ACCEPT [0:0]
:OUTPUT ACCEPT [3:556]
-A INPUT -m state --state RELATED,ESTABLISHED -j ACCEPT
-A INPUT -p icmp -j ACCEPT
-A INPUT -i lo -j ACCEPT
-A INPUT -p tcp -m state --state NEW -m tcp --dport 22 -j ACCEPT
-A INPUT -p tcp -m tcp --dport 80 -j ACCEPT
-A INPUT -p tcp -m tcp --dport 443 -j ACCEPT
-A INPUT -j REJECT --reject-with icmp-host-prohibited
-A FORWARD -j REJECT --reject-with icmp-host-prohibited
COMMIT
EOF
    cp /tmp/iptables /etc/sysconfig/iptables
    rm /tmp/iptables
    /sbin/service iptables reload

    if [ ! -e /etc/init.d/rstudio-server -a -e /usr/lib/rstudio-server/extras/init.d/redhat/rstudio-server ]; then
      sudo cp /usr/lib/rstudio-server/extras/init.d/redhat/rstudio-server /etc/init.d
      sudo chkconfig rstudio-server on
    fi
    sudo service rstudio-server restart
    if [ ! -e /etc/init.d/shiny-server -a -e /opt/shiny-server/config/init.d/redhat/shiny-server ]; then
      sudo cp /opt/shiny-server/config/init.d/redhat/shiny-server /etc/init.d
      sudo chkconfig shiny-server on
    fi
    sudo service shiny-server restart
    sudo service httpd restart
    ;;
  RH_7)
    sudo systemctl restart rstudio-server.service
    sudo systemctl restart shiny-server.service
    sudo systemctl restart httpd.service
    ;;
  Ubuntu)
    sudo a2enmod proxy_http
    sudo a2enconf bety pecan
    if [ -e ${HTTP_CONF}/shiny.conf ]; then
      sudo a2enconf rstudio
      sudo service rstudio-server restart
    fi
    if [ -e ${HTTP_CONF}/shiny.conf ]; then
      sudo a2enconf shiny
      sudo service shiny-server restart
    fi
    sudo service apache2 restart
    ;;
esac

echo "######################################################################"
echo "PalEON"
echo "######################################################################"
if [ "$SETUP_PALEON" != "" ]; then
  echo 'if(!"neotoma" %in% installed.packages()) install.packages("neotoma", repos="http://cran.rstudio.com/")' | R --vanilla
  echo 'if(!"R2jags" %in% installed.packages()) install.packages("R2jags", repos="http://cran.rstudio.com/")' | R --vanilla

  if [ ! -e ${HOME}/Camp2016 ]; then
    cd
    git clone https://github.com/PalEON-Project/Camp2016.git
  fi
  cd ${HOME}/Camp2016
  git pull
  R --vanilla < testInstall.R

  if [ ! -e ${HOME}/LinBacon_2.2 ]; then
    cd
    curl -o LinBacon_2.2.zip http://chrono.qub.ac.uk/blaauw/LinBacon_2.2.zip
    unzip LinBacon_2.2.zip
    rm LinBacon_2.2.zip
  fi
  cd ${HOME}/LinBacon_2.2/cpp
  rm -f *.o
  make -f makefileLinux
  rm *.o

  if [ ! -e ${HOME}/clam ]; then
    cd
    curl -o clam.zip http://chrono.qub.ac.uk/blaauw/clam.zip
    unzip clam.zip
    rm clam.zip
  fi
fi

echo "######################################################################"
echo "SETUP_VM"
echo "######################################################################"
if [ "${SETUP_VM}" != "" ]; then
  # extra packages
  case "$OS_VERSION" in
    RH_*)
      sudo yum -y install hdf5-tools cdo nco netcdf-bin ncview gdb emacs ess nedit
      ;;
    Ubuntu)
      sudo apt-get -y install hdf5-tools cdo nco netcdf-bin ncview gdb emacs ess nedit
      ;;
  esac

  # MOTD
  VERSION=$( awk '/Version: / { print $2 }' $HOME/pecan/base/all/DESCRIPTION )
  cat > /tmp/motd << EOF
PEcAn version ${VERSION}

For more information about:
Pecan    - http://pecanproject.org
BETY     - http://www.betydb.org

For a list of all models currently supported see:
https://pecan.gitbooks.io/pecan-documentation/content/models/
EOF
  sudo cp /tmp/motd /etc/motd
  rm /tmp/motd

  # fix prompt
  sudo sed -i 's/\\h/\\H/g' /etc/bash.bashrc
  sudo sed -i 's/\\h/\\H/g' /etc/profile
  sed -i 's/\\h/\\H/g' ${HOME}/.bashrc
fi
