#!/bin/bash

mkdir -p /home/${USER}
#if [ ! -e "/home/${USER}/.Renviron" ]; then
	 echo "# Environment Variables" >> "/home/${USER}/.Renviron"
#fi

for x in ${KEEP_ENV}; do
	value="$(echo "${!x}" | sed 's/\//\\\//g')"
	sed -i -e "/^$x=/{h;s/=.*/=${value}/};\${x;/^\$/{s//$x=${value}/;H};x}" "/home/${USER}/.Renviron"
done

exec /init
