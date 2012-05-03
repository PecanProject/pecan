#!/bin/bash

# run this as:
# history -c && ./cleanvm.sh

sudo apt-get --purge remove
sudo apt-get clean
sudo rm /etc/udev/rules.d/70-persistent-net.rules
sudo rm -rf /root/.ssh
sudo rm /root/.bash_history
sudo touch /root/.bash_history

sudo rm -rf /var/log
sudo mkdir -p /var/log/{apparmor,apt,dist-upgrade,fsck,installer,landscape,mysql,news,unattended-upgrades}
sudo chown landscape /var/log/landscape
sudo chown mysql.adm /var/log/mysql
sudo chmod 710 /var/log/mysql

sudo touch /var/log/wtmp /var/log/btmp
sudo chown root.utmp /var/log/wtmp /var/log/btmp

sudo rm /etc/ssh/ssh_host*key*

mysql -u bety -pbety bety -e 'delete from runs'

history -c
rm ${HOME}/testrun.ed/history.xml ${HOME}/testrun.ed/analysis*.h5
rm -rf ${HOME}/testrun.pecan/{out,pecan,pft,run,jobs,ssh.pid,check.sh,launcher.sh,setup.sh} ${HOME}/testrun.pecan/*.{Rout,png}
rm -rf ${HOME}/{.ssh,.cache,.bash_history,.bzr.log,.mysql_history,.sudo_as_admin_successful,.viminfo}
touch ${HOME}/.bash_history

sudo halt
