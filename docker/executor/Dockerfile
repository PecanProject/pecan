# this needs to be at the top, what version are we building
ARG IMAGE_VERSION="latest"

# ----------------------------------------------------------------------
# PECAN FOR MODEL BASE IMAGE
# ----------------------------------------------------------------------
FROM pecan/base:${IMAGE_VERSION}

# ----------------------------------------------------------------------
# SETUP FOR PYTHON CODE
# ----------------------------------------------------------------------
RUN apt-get update \
    && apt-get install -y --no-install-recommends python3-pip \
    && pip3 install pika==1.0.0 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /work

# variables to store in docker image
ENV RABBITMQ_URI="amqp://guest:guest@rabbitmq/%2F" \
    RABBITMQ_QUEUE="pecan" \
    APPLICATION="workflow"

# actual application that will be executed
COPY executor.py /work/
CMD ["python3", "/work/executor.py"]
