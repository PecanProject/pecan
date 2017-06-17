#!/bin/bash

. /build/install_pecan_preprocessor.sh

# actual install/update
echo "######################################################################"
echo "UPDATING MACHINE"
echo "######################################################################"
if [ ! -e /home/carya/ ]; then
  mkdir /home/carya/
  chmod 755 /home/carya/
fi

case "$OS_VERSION" in
  RH_*)
     yum update -y
    if [ "$SETUP_VM" != "" ]; then
       sed -i -e "s/^127.0.0.1 .*\$/127.0.0.1 ${HOSTNAME}.pecan ${HOSTNAME} localhost localhost.localdomain localhost4 localhost4.localdomain4/" /etc/hosts
    fi
    ;;
  Ubuntu)
     apt-get -qq -y update
     apt-get -y dist-upgrade
     apt-get -y purge --auto-remove
    if [ "$SETUP_VM" != "" ]; then
       sed -i -e "s/^127.0.0.1 .*\$/127.0.0.1 ${HOSTNAME}.pecan ${HOSTNAME} localhost/" /etc/hosts
    fi
    ;;
  *)
    echo "Unknown OS"
    exit 1
    ;;
esac
