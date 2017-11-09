# CentOS / RHEL

These are specific notes for installing PEcAn on RedHat/CentOS and will be referenced from the main [installing PEcAn](Installing-PEcAn) page. You will at least need to install the build environment and Postgres sections. If you want to access the database/PEcAn using a web browser you will need to install Apache. To access the database using the BETY interface, you will need to have Ruby installed.

This document also contains information on how to install the Rstudio server edition as well as any other packages that can be helpful.

## Install build environment

#### Install and configure PostgreSQL, udunits2, NetCDF

[Reference: centoshelp.org](http://www.postgresql.org/download/linux/redhat/)

```bash
yum install -y epel-release 
yum -y install http://yum.postgresql.org/9.4/redhat/rhel-7-x86_64/pgdg-centos94-9.4-1.noarch.rpm
yum -y install git R postgresql94-server postgresql94-contrib postgis2_94 udunits2 netcdf
/usr/pgsql-9.4/bin/postgresql94-setup initdb
systemctl enable postgresql-9.4
systemctl start postgresql-9.4
```

#### Install and start Apache

```bash
yum -y install httpd
systemctl enable httpd
systemctl start httpd
```

#### Install PHP

```bash
sudo yum -y install php php-pgsql
```

#### Install ruby-netcdf gem 

```bash
cd $RUBY_APPLICATION_HOME
export $NETCDF_URL=http://www.gfd-dennou.org/arch/ruby/products/ruby-netcdf/release/ruby-netcdf-0.6.6.tar.gz
export $NETCDF_DIR=/usr/local/netcdf
gem install narray
export NARRAY_DIR="$(ls $GEM_HOME/gems | grep 'narray-')"
export NARRAY_PATH="$GEM_HOME/gems/$NARRAY_DIR"
cd $MY_RUBY_HOME/bin
wget $NETCDF_URL -O ruby-netcdf.tgz
tar zxf ruby-netcdf.tgz && cd ruby-netcdf-0.6.6/
ruby -rubygems extconf.rb --with-narray-include=$NARRAY_PATH --with-netcdf-dir=/usr/local/netcdf-4.3.0
sed -i 's|rb/$|rb|' Makefile
make
make install
cd ../ && sudo rm -rf ruby-netcdf*

cd $RUBY_APPLICATION
bundle install --without development
```

## Apache Configuration

## Install and configure Rstudio-server

based on [Rstudio Server documentation](http://www.rstudio.com/ide/docs/server/getting_started)

* add `PATH=$PATH:/usr/sbin:/sbin` to `/etc/profile`
```bash
   cat "PATH=$PATH:/usr/sbin:/sbin; export PATH" >> /etc/profile
```
* add [rstudio.conf](https://gist.github.com/dlebauer/6921889) to /etc/httpd/conf.d/ 
```bash
   wget https://gist.github.com/dlebauer/6921889/raw/d1e0f945228e5519afa6223d6f49d6e0617262bd/rstudio.conf
   sudo mv rstudio.conf /httpd/conf.d/
```
* download and install server:
```bash
   wget http://download2.rstudio.org/rstudio-server-0.97.551-i686.rpm
   sudo yum install --nogpgcheck rstudio-server-0.97.551-i686.rpm
```
* restart server `sudo httpd restart`
* now you should be able to access `http://<server>/rstudio`

## Install Postgres

See [documentation under the BETYdb Wiki](https://github.com/PecanProject/bety/wiki/Installing-Postgresql)