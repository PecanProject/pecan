test_that("`rabbitmq_parse_uri()` able to parse the rabbitmq uri to smaller variables", {
  uri <- "amqp://guest:guest@localhost:15672/myvhost"
  result <- rabbitmq_parse_uri(uri)
  expect_equal(result$url, "http://localhost:15672/")
  expect_equal(result$vhost$path, c("myvhost"))
  expect_equal(result$username, "guest")
  expect_equal(result$password, "guest")
})

test_that("`rabbitmq_send_message()` able to return content if the status code is between 200 and 299", {
  mockery::stub(rabbitmq_send_message, 'httr::GET', data.frame(status_code = 200))
  mockery::stub(rabbitmq_send_message, 'httr::content', "test")
  res <- rabbitmq_send_message(url = 'test/', auth = 'test', body = 'test', action = "GET")
  expect_equal(res, "test")
})

test_that("`rabbitmq_send_message()` throws error where it should", {
  PEcAn.logger::logger.setUseConsole(TRUE, FALSE)
  on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE), add = TRUE)

  # errors if the action specified is unknown
  expect_output(
    rabbitmq_send_message(url = 'test/', auth = 'test', body = 'test', action = "TEST"),
    "uknown action TEST"
  )

  # errors if the status code is 401 (username/password may be incorrect)
  mockery::stub(rabbitmq_send_message, 'httr::GET', data.frame(status_code = 401))
  expect_output(
    rabbitmq_send_message(url = 'test/', auth = 'test', body = 'test', action = "GET"),
    "error sending message to rabbitmq"
  )

  # errors if the status code is outside of 200-299 and not 401
  mockery::stub(rabbitmq_send_message, 'httr::GET', data.frame(status_code = 501))
  mockery::stub(rabbitmq_send_message, 'httr::content', "test")
  expect_output(
    rabbitmq_send_message(url = 'test/', auth = 'test', body = 'test', action = "GET"),
    "error sending message to rabbitmq \\[ 501 \\]"
  )
})

test_that("`rabbitmq_create_queue()` able to take care of condition if the queue already exists or not while creating a queue", {
  mocked_res <- mockery::mock(NA, 'test')
  mockery::stub(rabbitmq_create_queue, 'rabbitmq_send_message', mocked_res)
  res <- rabbitmq_create_queue(url = 'test', auth = 'test', vhost = 'test', queue = 'test')
  args <- mockery::mock_args(mocked_res)
  expect_equal(res, TRUE)
  expect_equal(args[[1]][[4]], 'GET')
  expect_equal(args[[2]][[4]], 'PUT')
})

test_that("`rabbitmq_post_message()` passes the right params to send message to rabbitmq", {
  mocked_res <- mockery::mock('test')
  mockery::stub(rabbitmq_post_message, 'rabbitmq_send_message', mocked_res)
  mockery::stub(rabbitmq_post_message, 'rabbitmq_create_queue', TRUE)
  res <- rabbitmq_post_message(uri = 'amqp://guest:guest@localhost:15672/myvhost', queue = 'test_queue', message = 'test_message')
  args <- mockery::mock_args(mocked_res)
  expect_equal(res, 'test')
  expect_equal(args[[1]][[1]], 'http://localhost:15672/api/exchanges/myvhost//publish')
  expect_equal(args[[1]][[3]]$properties$delivery_mode, 2)
  expect_equal(args[[1]][[3]]$routing_key, 'test_queue')
  expect_equal(args[[1]][[3]]$payload, jsonlite::toJSON('test_message', auto_unbox = TRUE))
  expect_equal(args[[1]][[3]]$payload_encoding, 'string')
  expect_equal(args[[1]][[4]], 'POST')
})

test_that("`rabbitmq_get_message()` passes the right params to send message to rabbitmq", {
  mocked_res <- mockery::mock(NA)
  mockery::stub(rabbitmq_get_message, 'rabbitmq_send_message', mocked_res)
  mockery::stub(rabbitmq_get_message, 'rabbitmq_create_queue', TRUE)
  res <- rabbitmq_get_message(uri = 'amqp://guest:guest@localhost:15672/myvhost', queue = 'test_queue')
  args <- mockery::mock_args(mocked_res)
  expect_equal(args[[1]][[1]], 'http://localhost:15672/api/queues/myvhost/test_queue/get')
  expect_equal(args[[1]][[3]]$count, 1)
  expect_equal(args[[1]][[3]]$ackmode, 'ack_requeue_false')
  expect_equal(args[[1]][[3]]$encoding, 'auto')
  expect_equal(args[[1]][[4]], 'POST')
})