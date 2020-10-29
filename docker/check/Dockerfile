FROM python:3.7-alpine

ENV RABBITMQ_URI=amqp://guest:guest@rabbitmq/%2F \
    PGHOST=posgres \
    PGPORT=5432 \
    PGUSER=postgres

COPY requirements.txt /src/
RUN pip install -r /src/requirements.txt \
    && apk --no-cache add bash curl postgresql-client

COPY check_* /src/

USER 1000
WORKDIR /src
CMD ./check_rabbitmq
