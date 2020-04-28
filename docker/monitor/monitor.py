#!/usr/bin/env python

import datetime
import dateutil.parser
import http.server
import json
import logging
import os
import threading
import time
import urllib.parse

import pika
import psycopg2
import requests

# parameters to connect to RabbitMQ
rabbitmq_uri = os.getenv('RABBITMQ_URI', 'amqp://guest:guest@localhost/%2F')
rabbitmq_mgmt_port = os.getenv('RABBITMQ_MGMT_PORT', '15672')
rabbitmq_mgmt_path = os.getenv('RABBITMQ_MGMT_PATH', '/')
rabbitmq_mgmt_url = os.getenv('RABBITMQ_MGMT_URL', '')
rabbitmq_username = None
rabbitmq_password = None

# parameters to connect to BETY database
postgres_host = os.getenv('PGHOST', 'postgres')
postgres_port = os.getenv('PGPORT', '5432')
postgres_user = os.getenv('BETYUSER', 'bety')
postgres_password = os.getenv('BETYPASSWORD', 'bety')
postgres_database = os.getenv('BETYDATABASE', 'bety')
postgres_uri = None

# name of host when registering the model
pecan_fqdn = os.getenv('FQDN', 'docker')

# list of all models.
models = {}

# frequency with which the counts of the queue is updated in seconds
update_frequency = 60

# number of seconds before a consumer is removed
remove_model_timout = 15 * 60


# ----------------------------------------------------------------------
# WEB SERVER
# ----------------------------------------------------------------------
class MyServer(http.server.SimpleHTTPRequestHandler):
    """
    Handles the responses from the web server. Only response that is
    handled is a GET that will return all known models.
    """
    def do_GET(self):
        self.path = os.path.basename(self.path)
        if self.path == '':
            self.path = '/'

        if self.path.startswith('models.json'):
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            self.wfile.write(bytes(json.dumps(models), 'utf-8'))
        else:
            super().do_GET()


def http_server(host_port=9999):
    """
    Start a webserver to return all models that registered since this
    application started.
    """
    server = http.server.HTTPServer(("", host_port), MyServer)
    try:
        server.serve_forever()
    finally:
        server.server_close()


# ----------------------------------------------------------------------
# MESSAGES IN QUEUES
# ----------------------------------------------------------------------
def get_mgmt_queue_messages(queue):
    """
    Get the number of messages waiting in the queue.
    """
    global rabbitmq_username, rabbitmq_password, rabbitmq_mgmt_url

    try:
        response = requests.get(rabbitmq_mgmt_url + queue, auth=(rabbitmq_username, rabbitmq_password), timeout=5)
        if response.status_code == 404:
            # queue does not exist, so we assume 0 messages
            return 0
        response.raise_for_status()
        return response.json()['messages']
    except Exception:
        logging.exception("Error getting list of messages in %s" % queue)
        return 0


def keep_entry(consumer):
    """
    Check to see if the last time the consumer was seen is more than timeout seconds.
    """
    global remove_model_timout
    
    now = datetime.datetime.now()
    delta = now - dateutil.parser.parse(consumer['last_seen'])
    return delta.total_seconds() < remove_model_timout


def update_counts():
    """
    Contacts the RabbitMQ server and checks the number of jobs in the queue. It will
    also clean up the list of consumers based on the last time a heartbeat was seen
    from the consumer.
    This will run continuously and should be run as a daemon thread.
    """
    global models, update_frequency

    while True:
        try:
            for versions in models.values():
                for model in versions.values():
                    # use management api to get counts
                    waiting = get_mgmt_queue_messages(model['queue'])
                    model['messages'] = waiting

                    model['consumers'] = {k: v for k, v in model['consumers'].items() if keep_entry(v)}
        except Exception:
            logging.exception("ERROR")
        time.sleep(update_frequency)


# ----------------------------------------------------------------------
# EXTRACTOR HEARTBEATS
# ----------------------------------------------------------------------
def insert_model(model_info):
    """
    Insert the model info into the database. If the host, modeltype or
    model does not exist it will be inserted in the database as well.
    """
    global postgres_uri, postgres_host, postgres_port, postgres_database
    global postgres_user, postgres_password

    if not postgres_uri:
        postgres_uri = "host=%s port=%s dbname=%s user=%s password=%s connect_timeout=10" % (
            postgres_host, postgres_port, postgres_database, postgres_user, postgres_password
        )

    conn = None

    try:
        # connect to the PostgreSQL database
        conn = psycopg2.connect(postgres_uri)

        # make sure host exists
        cur = conn.cursor()
        cur.execute("SELECT id FROM machines WHERE hostname=%s", (pecan_fqdn,))
        result = cur.fetchone()
        cur.close()
        if result:
            host_id = result[0]
        else:
            logging.debug("Adding host")
            cur = conn.cursor()
            cur.execute('INSERT INTO machines (hostname) '
                        'VALUES (%s) RETURNING id', (pecan_fqdn, ))
            result = cur.fetchone()
            cur.close()
            if not result:
                logging.error("Could not insert host.")
                return
            host_id = result[0]
        logging.debug("Found hostname for %s == %d" % (pecan_fqdn, host_id))

        # Make sure modeltype exists
        cur = conn.cursor()
        cur.execute("SELECT id FROM modeltypes WHERE name=%s", (model_info['type'],))
        result = cur.fetchone()
        cur.close()
        if result:
            model_type_id = result[0]
        else:
            logging.debug("Adding modeltype")
            cur = conn.cursor()
            cur.execute('INSERT INTO modeltypes (name) '
                        'VALUES (%s) RETURNING id', (model_info['type']))
            result = cur.fetchone()
            cur.close()
            if not result:
                logging.error("Could not insert modeltypes.")
                return
            model_type_id = result[0]
        logging.debug("Found modeltype for %s == %d" % (model_info['type'], model_type_id))

        # Make sure model exists
        cur = conn.cursor()
        cur.execute("SELECT id FROM models WHERE model_name=%s AND modeltype_id=%s AND revision=%s",
                    (model_info['name'], model_type_id, model_info['version']))
        result = cur.fetchone()
        cur.close()
        if result:
            model_id = result[0]
        else:
            logging.debug("Adding model")
            cur = conn.cursor()
            cur.execute('INSERT INTO models (model_name, modeltype_id, revision) '
                        'VALUES (%s, %s, %s) RETURNING id',
                        (model_info['name'], model_type_id, model_info['version']))
            result = cur.fetchone()
            cur.close()
            if not result:
                logging.error("Could not insert model.")
                return
            model_id = result[0]
        logging.debug("Found model for %s %s (%s) == %d" %
                      (model_info['name'], model_info['type'], model_info['version'], model_id))

        # check if binary already added
        cur = conn.cursor()
        cur.execute("SELECT id FROM dbfiles "
                    "WHERE container_type='Model' AND container_id=%s "
                    "AND file_name=%s AND file_path=%s and machine_id=%s",
                    (model_id, os.path.basename(model_info['binary']), os.path.dirname(model_info['binary']), host_id))
        result = cur.fetchone()
        cur.close()
        if result:
            dbfile_id = result[0]
        else:
            logging.debug("Adding model binary")
            cur = conn.cursor()
            cur.execute("INSERT INTO dbfiles (container_type, container_id, file_name, file_path,"
                        " machine_id)"
                        " VALUES ('Model', %s, %s, %s, %s) RETURNING id",
                        (model_id, os.path.basename(model_info['binary']),
                         os.path.dirname(model_info['binary']), host_id))
            result = cur.fetchone()
            cur.close()
            if not result:
                logging.error("Could not insert model binary.")
                return
            dbfile_id = result[0]
        logging.debug("Found model binary for %s %s (%s) on %s == %d" %
                      (model_info['name'], model_info['type'], model_info['version'], pecan_fqdn, dbfile_id))

        # commit all changes to the database
        conn.commit()
    except (Exception, psycopg2.DatabaseError):
        logging.exception("Error adding model to database")
    finally:
        if conn is not None:
            conn.close()


def callback(ch, method, properties, body):
    """
    A heartbeat of a model is received. Register the model with the database.
    """
    global models

    data = json.loads(body.decode('utf-8'))
    data['updated'] = datetime.datetime.now().isoformat()
    if 'id' not in data and 'model_info' not in data and 'queue' not in data:
        logging.error("missing fields in json : %r " % body)
        return

    model_info = data['model_info']

    if model_info['type'] not in models:
        models[model_info['type']] = {}

    if model_info['version'] not in models[model_info['type']]:
        insert_model(model_info)
        waiting = get_mgmt_queue_messages(data['queue'])
        models[model_info['type']][model_info['version']] = {
            'model_info': model_info,
            'queue': data['queue'],
            'messages': waiting,
            'consumers': {}
        }
    model = models[model_info['type']][model_info['version']]

    model['consumers'][data['id']] = {
        'hostname': data['hostname'],
        'last_seen': datetime.datetime.now().isoformat(),
    }

    if model['queue'] != data['queue']:
        logging.error("mismatched queue names %s != %s." % (data['queue'], model['queue']))
        model['queue'] = data['queue']


def rabbitmq_monitor():
    """
    Create a connection with RabbitMQ and wait for heartbeats. This
    will run continuously. This will run as the main thread. If this
    is stopped the appliation will stop.
    """
    global rabbitmq_mgmt_url, rabbitmq_mgmt_port, rabbitmq_mgmt_path, rabbitmq_username, rabbitmq_password

    params = pika.URLParameters(rabbitmq_uri)
    connection = pika.BlockingConnection(params)

    # create management url
    if not rabbitmq_mgmt_url:
        if params.ssl_options:
            rabbitmq_mgmt_protocol = 'https://'
        else:
            rabbitmq_mgmt_protocol = 'http://'
        rabbitmq_mgmt_url = "%s%s:%s%sapi/queues/%s/" % (rabbitmq_mgmt_protocol, params.host, rabbitmq_mgmt_port,
                                                         rabbitmq_mgmt_path,
                                                         urllib.parse.quote_plus(params.virtual_host))
        rabbitmq_username = params.credentials.username
        rabbitmq_password = params.credentials.password

    # connect to channel
    channel = connection.channel()

    # create models exchange for fanout
    channel.exchange_declare(exchange='models', exchange_type='fanout', durable=True)

    # create anonymous queue
    result = channel.queue_declare('', exclusive=True)
    channel.queue_bind(exchange='models', queue=result.method.queue)

    # listen for messages
    channel.basic_consume(on_message_callback=callback, queue=result.method.queue, auto_ack=True)

    channel.start_consuming()


# ----------------------------------------------------------------------
# MAIN
# ----------------------------------------------------------------------
if __name__ == "__main__":
    logging.basicConfig(format='%(asctime)-15s [%(threadName)-15s] %(levelname)-7s :'
                               ' %(name)s - %(message)s',
                        level=logging.INFO)
    logging.getLogger('requests.packages.urllib3.connectionpool').setLevel(logging.WARN)

    thread = threading.Thread(target=http_server)
    thread.setDaemon(True)
    thread.start()

    thread = threading.Thread(target=update_counts)
    thread.setDaemon(True)
    thread.start()

    rabbitmq_monitor()
