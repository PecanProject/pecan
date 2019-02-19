FROM python:3.5

ENV RABBITMQ_URI="amqp://guest:guest@rabbitmq/%2F" \
    RABBITMQ_MGMT_PORT="15672" \
    RABBITMQ_MGMT_PATH="/rabbitmq/" \
    POSTGRES_PARAM="host=postgres dbname=bety user=bety password=bety connect_timeout=10" \
    FQDN="pecan"

EXPOSE 9999

RUN pip install pika==0.11.2 \
                requests==2.18.4 \
                psycopg2-binary==2.7.6.1 \
                python-dateutil==2.7.5

WORKDIR /src
COPY monitor.py /src/

CMD python monitor.py
