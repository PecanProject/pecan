FROM ubuntu:16.04
MAINTAINER amanskywalker (ak47su30ac@gmail.com)

# expose port 80 for the web interface
EXPOSE 80

# expose port 22 for ssh maintance
EXPOSE 22

# updated ppa's
RUN echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" > /etc/apt/sources.list.d/R.list &&\
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

# copy the installation script inside the container
ADD docker/ /build

# Run the OS System setup script
RUN chmod 750 /build/system_services.sh
RUN /build/system_services.sh

# Set script mod +x for preprocessors
RUN chmod 750 /build/*

# run update machine to update machine
RUN /build/update_machine.sh

# run inatall packages to install required packages
RUN /build/install_packages.sh

# run install R to install R packages
RUN /build/install_R.sh

# run install pecan to install pecan cores
RUN /build/install_pecan.sh

# Clean up APT when done.
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# startup
CMD ["/sbin/my_init"]
