#!/bin/bash

# Send a "rebuild" message to a running Dockerized PEcAn instance.

RABBITMQ_USER=${RABBITMQ_USER:-guest}
RABBITMQ_PASSWORD=${RABBITMQ_PASSWORD:-guest}
RABBITMQ_HOST=${RABBITMQ_HOST:-"http://localhost:8000/rabbitmq/"}

curl --anyauth --user ${RABBITMQ_USER}:${RABBITMQ_PASSWORD} \
     -d '{"properties" : {"delivery_mode" : 2}, "routing_key": "pecan", "payload" : "{\"rebuild\" : \"true\"}", "payload_encoding" : "string"}' \
     -H "Content-Type: application/json" \
     -X POST "${RABBITMQ_HOST}/api/exchanges/%2f//publish"
