import json
import logging
import os
import subprocess
import traceback

import pika


rabbitmq_uri   = os.getenv('RABBITMQ_URI',   'amqp://guest:guest@rabbitmq/%2F')
rabbitmq_queue = os.getenv('RABBITMQ_QUEUE', 'pecan')
application    = os.getenv('APPLICATION',    'job.sh')

# called for every message, this will start the program and ack message if all is ok.
def callback(ch, method, properties, body):
    logging.info(body)
    jbody = json.loads(body)

    logging.info("Starting job in %s." % jbody['folder'])
    try:
        output = subprocess.check_output(application, stderr=subprocess.STDOUT, shell=True, cwd=jbody['folder'])
        status = 'OK'
        logging.info("Finished running job.")
    except subprocess.CalledProcessError as e:
        logging.exception("Error running job.", e)
        output = e.output
        status = 'ERROR'
    except Exception as e:
        logging.exception("Error running job.", e)
        output = str(e)
        status = 'ERROR'
    finally:
        ch.basic_ack(delivery_tag=method.delivery_tag)

    try:
        with open(os.path.join(jbody['folder'], 'rabbitmq.out'), 'w') as out:
            out.write(str(output) + "\n")
            out.write(status + "\n")
    except Exception as e:
        logging.exception("Error writing status.", e)


def start_rabbitmq():
    # create connection to rabbitmq
    connection = pika.BlockingConnection(pika.URLParameters(rabbitmq_uri))
    channel = connection.channel()

    # make sure queue exists
    channel.queue_declare(queue=rabbitmq_queue, durable=True)

    # receive 1 message at a time, call callback function
    channel.basic_qos(prefetch_count=1)
    channel.basic_consume(callback, queue=rabbitmq_queue)

    # receive messages
    try:
        logging.info(' [*] Waiting for messages. To exit press CTRL+C')
        channel.start_consuming()
    except KeyboardInterrupt:
        pass
    finally:
        connection.close()


if __name__ == "__main__":
    logging.basicConfig(format='%(asctime)-15s [%(threadName)-15s] %(levelname)-7s : %(name)s - %(message)s',
                        level=logging.INFO)
    logging.getLogger('requests.packages.urllib3.connectionpool').setLevel(logging.WARN)
    start_rabbitmq()
