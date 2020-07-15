# this needs to be at the top, what version are we building
ARG IMAGE_VERSION="latest"
ARG FROM_IMAGE="depends"
FROM pecan/${FROM_IMAGE}:${IMAGE_VERSION}
MAINTAINER Rob Kooper <kooper@illinois.edu>

# ----------------------------------------------------------------------
# PEcAn version information
# ----------------------------------------------------------------------
ARG PECAN_VERSION="develop"
ARG PECAN_GIT_BRANCH="unknown"
ARG PECAN_GIT_CHECKSUM="unknown"
ARG PECAN_GIT_DATE="unknown"

# ----------------------------------------------------------------------
# PEcAn installation from local source
# ----------------------------------------------------------------------
# copy folders
COPY Makefile Makefile.depends /pecan/
COPY scripts/time.sh /pecan/scripts/time.sh
COPY scripts/travis /pecan/scripts/travis/
COPY base     /pecan/base/
COPY modules  /pecan/modules/
COPY models   /pecan/models/

RUN cd /pecan && make && rm -rf /tmp/downloaded_packages

# COPY WORKFLOW
WORKDIR /work
COPY web/workflow.R /work/

# COMMAND TO RUN
CMD Rscript --vanilla workflow.R | tee workflow.Rout

# install pika and copy sender.py
RUN apt-get update \
    && apt-get install -y --no-install-recommends python3-pip \
    && pip3 install pika==1.0.0 \
    && rm -rf /var/lib/apt/lists/*
COPY docker/executor/sender.py /work/

# variables to store in docker image
ENV PECAN_VERSION=${PECAN_VERSION} \
    PECAN_GIT_BRANCH=${PECAN_GIT_BRANCH} \
    PECAN_GIT_CHECKSUM=${PECAN_GIT_CHECKSUM} \
    PECAN_GIT_DATE=${PECAN_GIT_DATE}

ARG USER_ID
ARG GROUP_ID

RUN if [ ${USER_ID:-0} -ne 0 ] && [ ${GROUP_ID:-0} -ne 0 ]; then \
    userdel -f rstudio &&\
    if getent group rstudio ; then groupdel rstudio; fi && \
    groupadd -g $GROUP_ID -r rstudio && useradd -l -u $USER_ID -r -g rstudio rstudio && \
    install -d -m 0777 -o rstudio -g rstudio /home/rstudio && \
    chown --changes --silent --no-dereference --recursive \
          --from=1000:1000 $USER_ID:$GROUP_ID \
        /home/rstudio \
;fi
