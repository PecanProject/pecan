#!/bin/bash
export LC_ALL=C
export DEBIAN_FRONTEND=noninteractive
minimal_apt_get_install='apt-get install -y --no-install-recommends'

## temporarily disable dpkg fsync to make building faster.
if [[ ! -e /etc/dpkg/dpkg.cfg.d/docker-apt-speedup ]]; then
	echo force-unsafe-io > /etc/dpkg/dpkg.cfg.d/docker-apt-speedup
fi

## prevent initramfs updates from trying to run grub and lilo.
export INITRD=no
mkdir -p /etc/container_environment
echo -n no > /etc/container_environment/INITRD

## enable Ubuntu Universe and Multiverse.
sed -i 's/^#\s*\(deb.*universe\)$/\1/g' /etc/apt/sources.list
sed -i 's/^#\s*\(deb.*multiverse\)$/\1/g' /etc/apt/sources.list
apt-get update

## fix some issues with APT packages.
dpkg-divert --local --rename --add /sbin/initctl
ln -sf /bin/true /sbin/initctl

## replace the 'ischroot' tool to make it always return true.
dpkg-divert --local --rename --add /usr/bin/ischroot
ln -sf /bin/true /usr/bin/ischroot

## upgrade all packages.
apt-get dist-upgrade -y --no-install-recommends

## install HTTPS support for APT.
$minimal_apt_get_install apt-utils apt-transport-https ca-certificates language-pack-en

## fix locale.
locale-gen en_US.UTF-8
update-locale LANG=en_US.UTF-8 LC_CTYPE=en_US.UTF-8 LANGUAGE=en_US:en LC_ALL=en_US.UTF-8
echo -n en_US.UTF-8 > /etc/container_environment/LANG
echo -n en_US.UTF-8 > /etc/container_environment/LC_CTYPE
echo -n en_US:en > /etc/container_environment/LANGUAGE
echo -n en_US.UTF-8 > /etc/container_environment/LC_ALL

## install init process.
cp /build/bin/my_init /sbin/
chmod 750 /sbin/my_init
mkdir -p /etc/my_init.d
mkdir -p /etc/container_environment
touch /etc/container_environment.sh
touch /etc/container_environment.json
chmod 700 /etc/container_environment

groupadd -g 8377 docker_env
chown :docker_env /etc/container_environment.sh /etc/container_environment.json
chmod 640 /etc/container_environment.sh /etc/container_environment.json
ln -s /etc/container_environment.sh /etc/profile.d/
echo ". /etc/container_environment.sh" >> /root/.bashrc

## install runit.
$minimal_apt_get_install runit cron

## install cron daemon.
mkdir -p /etc/service/cron
mkdir -p /var/log/cron
chmod 600 /etc/crontabs
cp /build/runit/cron /etc/service/cron/run
cp /build/config/cron_log_config /var/log/cron/config
chown -R nobody  /var/log/cron
chmod +x /etc/service/cron/run

## remove useless cron entries.
rm -f /etc/cron.daily/standard
rm -f /etc/cron.daily/upstart
rm -f /etc/cron.daily/dpkg
rm -f /etc/cron.daily/password
rm -f /etc/cron.weekly/fstrim

## often used tools.
$minimal_apt_get_install curl less nano psmisc wget

## fix other small problem.
rm /bin/sh
ln -s /bin/bash /bin/sh
echo `. /etc/lsb-release; echo ${DISTRIB_CODENAME/*, /}` >> /etc/container_environment/DISTRIB_CODENAME

## cleanup
apt-get clean
rm -rf /build
rm -rf /tmp/* /var/tmp/*
rm -rf /var/lib/apt/lists/*
rm -f /etc/dpkg/dpkg.cfg.d/02apt-speedup
