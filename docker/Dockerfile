FROM amanskywalker/pecan-dev:latest
MAINTAINER Aman Kumar (ak47su30ac@gmail.com)

# copy the installation script inside the container
ADD docker/ /build

# Set script mod +x for preprocessors
RUN chmod 750 /build/*.sh

# Run the OS System setup script
RUN /build/system_services.sh

# run update machine to update machine
RUN /build/update_machine.sh

# install pecan web
RUN /build/install_pecan_web.sh

# Clean up APT when done.
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /build/*

# startup
CMD ["/sbin/my_init"]
