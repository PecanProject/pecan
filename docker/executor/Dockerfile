ARG IMAGE_VERSION="latest"

# ----------------------------------------------------------------------
# PECAN FOR MODEL BASE IMAGE
# ----------------------------------------------------------------------
FROM pecan/base:${IMAGE_VERSION}
MAINTAINER Rob Kooper <kooper@illinois.edu>

# ----------------------------------------------------------------------
# SETUP FOR PYTHON CODE
# ----------------------------------------------------------------------
RUN apt-get update \
    && apt-get install -y --no-install-recommends python3-pip \
    && pip3 install pika \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /work

# variables to store in docker image
ENV RABBITMQ_URI="amqp://guest:guest@rabbitmq/%2F" \
    RABBITMQ_QUEUE="pecan" \
    APPLICATION="R CMD BATCH workflow.R"

# actual application that will be executed
COPY executor.py sender.py /work/
CMD python3 /work/executor.py
