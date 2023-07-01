test_that("`rabbitmq_parse_uri()` able to parse the rabbitmq uri to smaller variables", {
  uri <- "amqp://guest:guest@localhost:15672/myvhost"
  result <- rabbitmq_parse_uri(uri)
  expect_equal(result$url, "http://localhost:15672/")
  expect_equal(result$vhost$path, c("myvhost"))
  expect_equal(result$username, "guest")
  expect_equal(result$password, "guest")
})
