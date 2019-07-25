FROM debian:stretch as rstudio-server
MAINTAINER Rob Kooper <kooper@illinois.edu>

RUN apt-get install gdebi-core \
    sudo \
    && rm -rf /var/lib/apt/lists/*

# install rstudio server
RUN wget https://download2.rstudio.org/server/trusty/amd64/rstudio-server-1.2.1335-amd64.deb
RUN sudo gdebi rstudio-server-1.2.1335-amd64.deb 
    && rm rstudio-server-1.2.1335-amd64.deb

# configuration files
RUN sudo sh -c 'echo "www-address=127.0.0.1" >> /etc/rstudio/rserver.conf'
    && sudo sh -c 'echo "r-libs-user=~/R/library" >> /etc/rstudio/rsession.conf'

#FROM nginx:alpine

#COPY nginx.conf /etc/nginx/conf.d/default.conf
