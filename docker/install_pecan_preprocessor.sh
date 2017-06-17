#!/bin/bash

set -e

#if [ "`whoami`" == "root" ]; then
#  echo "Don't run this script as root"
#  exit -1
#fi

# overiding environment variables

export HOME='/home/carya'

# configuration
# BROWNDOG_URL="http://dap.ncsa.illinois.edu:8184/convert/";
# BROWNDOG_USERNAME="";
# BROWNDOG_PASSWORD="";
#
# GOOGLE_MAP_KEY=""

#SETUP_VM=""
#SETUP_PALEON=""
#REBUILD=""

# commented out might need it later for communication purpose
#RSTUDIO_SERVER="1.0.136"
#SHINY_SERVER="1.5.3.838"

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
