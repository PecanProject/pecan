FROM php:apache
MAINTAINER Rob Kooper <kooper@illinois.edu>

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
    && mkdir -p /data/workflows /data/dbfiles \
    && chown www-data /data/workflows /data/dbfiles

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

ARG USER_ID
ARG GROUP_ID

RUN if [ ${USER_ID:-0} -ne 0 ] && [ ${GROUP_ID:-0} -ne 0 ]; then \
    userdel -f www-data &&\
    if getent group www-data ; then groupdel www-data; fi && \
    groupadd -g $GROUP_ID -r www-data && useradd -l -u $USER_ID -r -g www-data www-data && \
    install -d -m 0777 -o www-data -g www-data /home/www-data && \
    chown --changes --silent --no-dereference --recursive \
          --from=33:33 ${USER_ID}:${GROUP_ID} \
        /home/www-data \
        /var/www \
        /usr/sbin/nologin \
;fi
