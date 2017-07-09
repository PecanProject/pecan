FROM ubuntu:16.04
MAINTAINER Aman Kumar (ak47su30@gmail.com)

# updated ppa's
RUN echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" > /etc/apt/sources.list.d/R.list &&\
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

# copy the installation script inside the container
ADD docker/ /build

# Set script mod +x for preprocessors
RUN chmod 750 /build/*.sh

# Run the OS System setup script
RUN /build/system_services.sh

# run update machine to update machine
RUN /build/update_machine.sh

# run install packages to install required packages
RUN /build/install_packages.sh

# run install R to install R packages
RUN /build/install_R.sh

# run install pecan to install pecan cores
RUN /build/install_pecan.sh

# run install sipnet to install SIPNET (default testing Model)
RUN /build/install_sipnet.sh

# Clean up APT when done.
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /build/*

# startup
CMD ["/sbin/my_init"]
