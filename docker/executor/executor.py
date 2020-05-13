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
rabbitmq_queue = os.getenv('RABBITMQ_QUEUE', 'pecan')
default_application = os.getenv('APPLICATION', 'job.sh')


class Worker:
    def __init__(self, method, properties, body):
        self.method = method
        self.properties = properties
        self.body = body
        self.finished = False

    def runfunc(self):
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
            shutil.copyfile("/work/workflow.R", workflow_path)
            xml_file = open(os.path.join(folder, "pecan.xml"), "w")
            xml_file.write(pecan_xml)
            xml_file.close()

            # Set variables for execution
            application = "R CMD BATCH workflow.R"
        elif custom_application is not None:
            application = custom_application
        elif default_application == "workflow":
            application = "R CMD BATCH"
            if jbody.get("continue") == True:
                application = application + " --continue workflow.R workflow2.Rout";
            else:
                if jbody.get("modeledit") == True:
                    application = application + " --advanced"
                application = application + " workflow.R workflow.Rout";
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


if __name__ == "__main__":
    logging.basicConfig(format='%(asctime)-15s [%(threadName)-15s] %(levelname)-7s : %(name)s - %(message)s',
                        level=logging.INFO)
    logging.getLogger('requests.packages.urllib3.connectionpool').setLevel(logging.WARN)

    # start listening for new jobs
    receiver()
