FROM python:3.12

ENV RABBITMQ_URI="amqp://guest:guest@rabbitmq/%2F" \
    RABBITMQ_MGMT_PORT="15672" \
    RABBITMQ_MGMT_PATH="/rabbitmq/" \
    PGHOST="postgres" \
    BETYUSER="bety" \
    BETYPASSWORD="bety" \
    BETYDATABASE="bety" \
    FQDN="pecan"

EXPOSE 9999

WORKDIR /src

COPY requirements.txt /src/
RUN pip3 install -r /src/requirements.txt

COPY . /src/
CMD ["python3", "./monitor.py"]
