import json
import logging
import os
import socket
import subprocess
import sys
import threading
import time
import uuid

import pika
import shutil

rabbitmq_uri = os.getenv('RABBITMQ_URI', 'amqp://guest:guest@rabbitmq/%2F')
default_application = os.getenv('APPLICATION', 'job.sh')
model_info = None


class Worker:
    def __init__(self, method, properties, body):
        self.method = method
        self.properties = properties
        self.body = body
        self.finished = False

    def runfunc(self):
        try:
            logging.debug(self.body)
            jbody = json.loads(self.body.decode('UTF-8'))

            folder = jbody.get('folder')
            rebuild = jbody.get('rebuild')
            pecan_xml = jbody.get('pecan_xml')
            custom_application = jbody.get('custom_application')

            if rebuild is not None:
                logging.info("Rebuilding PEcAn with make")
                application = 'make'
                folder = '/pecan'
            elif pecan_xml is not None:
                # Passed entire pecan XML as a string
                logging.info("Running XML passed directly")
                try:
                    os.mkdir(folder)
                except OSError as e:
                    logging.info("Caught the following OSError. ",
                                 "If it's just that the directory exists, ",
                                 "this can probably be ignored: ", e)
                workflow_path = os.path.join(folder, "workflow.R")
                shutil.copyfile("/pecan/web/workflow.R", workflow_path)
                xml_file = open(os.path.join(folder, "pecan.xml"), "w")
                xml_file.write(pecan_xml)
                xml_file.close()

                # Set variables for execution
                application = "R CMD BATCH workflow.R"
            elif custom_application is not None:
                application = custom_application
            else:
                logging.info("Running default command: %s" % default_application)
                application = default_application

            logging.info("Running command: %s" % application)
            logging.info("Starting command in directory %s." % folder)
            try:
                output = subprocess.check_output(application, stderr=subprocess.STDOUT, shell=True, cwd=folder)
                status = 'OK'
            except subprocess.CalledProcessError as e:
                logging.exception("Error running job.")
                output = e.output
                status = 'ERROR'
            except Exception as e:
                logging.exception("Error running job.")
                output = str(e)
                status = 'ERROR'

            logging.info("Finished running job with status " + status)
            logging.info(output)

            try:
                with open(os.path.join(folder, 'rabbitmq.out'), 'w') as out:
                    out.write(str(output) + "\n")
                    out.write(status + "\n")
            except Exception:
                logging.exception("Error writing status.")
        finally:
            # done processing, set finished to true
            self.finished = True


# called for every message, this will start the program and ack message if all is ok.
def callback(ch, method, properties, body):
    global worker

    # do not pass channel, pika is not threadsafe, only receiver is allowed to use channel
    worker = Worker(method, properties, body)
    thread = threading.Thread(target=worker.runfunc)
    thread.start()


# connect to rabbitmq and receive jobs, only this function can use the channel.
def receiver():
    global worker

    # create connection to rabbitmq
    connection = pika.BlockingConnection(pika.URLParameters(rabbitmq_uri))
    channel = connection.channel()

    # make sure queue exists
    channel.queue_declare(queue=rabbitmq_queue, durable=True)

    # receive 1 message at a time, call callback function
    channel.basic_qos(prefetch_count=1)
    channel.basic_consume(on_message_callback=callback, queue=rabbitmq_queue)

    # receive messages
    worker = None
    logging.info('[*] Waiting for messages. To exit press CTRL+C')
    try:
        while True:
            # use polling to allow for heartbeats, the actual work is done
            # in another thread which should not talk in channel!
            channel.connection.process_data_events(time_limit=1)  # 1 second
            if worker and worker.finished:
                channel.basic_ack(delivery_tag=worker.method.delivery_tag)
                worker = None
    except KeyboardInterrupt:
        pass
    finally:
        connection.close()


class RabbitMQBroadcast:
    """
    This class is responsible for announcing a new model to PEcAn.
    This will send out a message every so often to ennounce the model
    and PEcAn will listen for these messages and register the model
    in the database. At that point the model will be shown in the
    web ui as well.
    """
    def __init__(self, rabbitmq_uri, exchange, model_info, heartbeat):
        self.exchange = exchange
        self.model_info = model_info
        self.heartbeat = heartbeat

        # create connection to rabbitmq
        parameters = pika.URLParameters(rabbitmq_uri)
        self.connection = pika.BlockingConnection(parameters)

        # connect to channel
        self.channel = self.connection.channel()

        # create extractors exchange for fanout
        self.channel.exchange_declare(exchange=self.exchange, exchange_type='fanout', durable=True)

        # create thread and start it
        self.thread = threading.Thread(target=self.send_heartbeat)
        self.thread.setDaemon(True)
        self.thread.start()

    def send_heartbeat(self):
        # create the message we will send
        message = {
            'id': str(uuid.uuid4()),
            'hostname': socket.gethostname(),
            'queue': '%s_%s' % (model_info['type'], model_info['version']),
            'model_info': self.model_info
        }
        while self.thread:
            try:
                time.sleep(self.heartbeat)
                self.channel.basic_publish(exchange=self.exchange, routing_key='', body=json.dumps(message))
            except SystemExit:
                raise
            except KeyboardInterrupt:
                raise
            except GeneratorExit:
                raise
            except Exception:  # pylint: disable=broad-except
                logging.getLogger(__name__).exception("Error while sending heartbeat.")


if __name__ == "__main__":
    logging.basicConfig(format='%(asctime)-15s [%(threadName)-15s] %(levelname)-7s : %(name)s - %(message)s',
                        level=logging.INFO)
    logging.getLogger('requests.packages.urllib3.connectionpool').setLevel(logging.WARN)

    # load model information
    if not os.path.exists("model.json"):
        logging.error("Missing model.json file, model will not be announced and is not discoverable.")
        sys.exit(1)
    model_info = json.load(open('model.json', 'r'))

    # set the rabbitmq Queue
    rabbitmq_queue = '%s_%s' % (model_info['type'], model_info['version'])

    # start the model announcer
    announcer = RabbitMQBroadcast(rabbitmq_uri, 'models', model_info, 5)

    # start listening for new jobs
    receiver()
