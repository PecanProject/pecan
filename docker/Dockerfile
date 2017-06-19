# Dockerfile for the pecan web
FROM amanskywalker/pecan-dev:0.1
MAINTAINER Aman Kumar (ak47su30@gmail.com)

# copy the installation script inside the container
ADD . /build

# Set script mod +x for preprocessors
RUN chmod 750 /build/*.sh

# Run the OS System setup script
RUN /build/system_services.sh

# run update machine to update machine
RUN /build/update_machine.sh

# install pecan web
RUN /build/install_pecan_web.sh

# simple scripts to do the startup task
RUN mkdir -p /etc/my_init.d
COPY apache2/startup.sh /etc/my_init.d/startup.sh
RUN chmod +x /etc/my_init.d/startup.sh

# adding demons of apache2
RUN mkdir -p /etc/service/rserver ; sync
COPY apache2/runserver.sh /etc/service/rserver/run
RUN chmod +x /etc/service/rserver/run

# add the pecan-web configuration
COPY web/config.docker.php /home/carya/pecan/web/config.php

# Clean up APT when done.
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /build/*

# startup
CMD ["/sbin/my_init"]
