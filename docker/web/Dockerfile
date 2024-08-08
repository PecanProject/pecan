FROM php:8-apache

# ----------------------------------------------------------------------
# install rabbitmq and postgresql extentions
# ----------------------------------------------------------------------
RUN apt-get update \
    && apt-get install -y \
        libpq-dev \
        librabbitmq-dev \
    && rm -rf /var/lib/apt/lists/* \
    && docker-php-ext-install -j$(nproc) pdo pdo_pgsql \
    && pecl install amqp \
    && docker-php-ext-enable amqp \
    && sed -i -e 's/ 80/ 8080/g' -e 's/ 443/ 8443/g' /etc/apache2/ports.conf \
    && sed -i -e 's/:80/:8080/g' -e 's/:443/:8443/g' /etc/apache2/sites-enabled/000-default.conf

# ----------------------------------------------------------------------
# copy webpages
# ----------------------------------------------------------------------
COPY documentation/index_vm.html /var/www/html/index.html
COPY web /var/www/html/pecan/
COPY docker/web/config.docker.php /var/www/html/pecan/config.php

# ----------------------------------------------------------------------
# PEcAn version information
# ----------------------------------------------------------------------
ARG PECAN_VERSION="develop"
ARG PECAN_GIT_BRANCH="unknown"
ARG PECAN_GIT_CHECKSUM="unknown"
ARG PECAN_GIT_DATE="unknown"

# variables to store in docker image
ENV PECAN_VERSION=${PECAN_VERSION} \
    PECAN_GIT_BRANCH=${PECAN_GIT_BRANCH} \
    PECAN_GIT_CHECKSUM=${PECAN_GIT_CHECKSUM} \
    PECAN_GIT_DATE=${PECAN_GIT_DATE}

EXPOSE 8080
