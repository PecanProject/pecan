import json
import sys

import pika


# parse command line arguments
# send.py <rabbitmq_uri> <queue> <message>
rabbitmq_uri     = sys.argv[1]
rabbitmq_queue   = sys.argv[2]
rabbitmq_message = {
    'folder': sys.argv[3]
}

# create connection to rabbitmq
connection = pika.BlockingConnection(pika.URLParameters(rabbitmq_uri))
channel = connection.channel()

# make sure queue exists
channel.queue_declare(queue=rabbitmq_queue, durable=True)

# publish message on queue
channel.basic_publish(exchange='',
                      routing_key=rabbitmq_queue,
                      body=json.dumps(rabbitmq_message),
                      properties=pika.BasicProperties(
                         delivery_mode = 2, # make message persistent
                      ))

# close connection
connection.close()
