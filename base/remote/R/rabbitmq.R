#' parse the RabbiMQ URI. This will parse the uri into smaller pieces that can
#' be used to talk to the rest endpoint for RabbitMQ.
#'
#' @param uri the amqp URI
#' @param prefix the prefix that the RabbitMQ managmenet interface uses
#' @param port the port for rabbitmq managment interface
#' @return a list that contains the url to the mangement interface, username
#'    password and vhost.
rabbitmq_parse_uri <- function(uri, prefix="", port=15672) {
  # save username/password
  if (!grepl("@", uri, fixed = TRUE)) {
    PEcAn.logger::logger.info("rabbitmq uri is not recognized, missing username and password, assuming guest/guest")
    upw <- c("guest", "guest")
  } else {
    upw <- strsplit(sub(".*://([^@]*).*", "\\1", uri), ":")[[1]]
    if (length(upw) != 2) {
      PEcAn.logger::logger.error("rabbitmq uri is not recognized, missing username or password")
      return(NA)
    }
  }

  # split uri and check scheme
  url_split <- urltools::url_parse(uri)
  if (!startsWith(url_split$scheme, "amqp")) {
    PEcAn.logger::logger.error("rabbitmq uri is not recognized, invalid scheme (need amqp(s) or http(s))")
    return(NA)
  }

  # convert uri to rabbitmq rest/management call
  url_split["scheme"] <- sub("amqp", "http", url_split["scheme"])
  url_split["port"] <- port
  vhost <- url_split["path"]
  prefix <- sub("^/+", "", prefix)
  if (prefix == "") {
    url_split["path"] <- ""
  } else if (endsWith(prefix, "/")) {
    url_split["path"] <- prefix
  } else {
    url_split["path"] <- paste0(prefix, "/")
  }

  url <- urltools::url_compose(url_split)

  return(list(url=url, vhost=vhost, username=upw[[1]], password=upw[[2]]))
}

#' Send a message to RabbitMQ rest API. It will check the resulting status code
#' and print a message in case something goes wrong.
#'
#' @param url the full endpoint rest url
#' @param auth authentication for rabbitmq in httr:auth
#' @param body the actual body to send, this is a rabbitmq message.
#' @param action the rest action to perform
#' @return will return NA if message failed, otherwise it will either
#'    return the resulting message, or if not availble an empty string "".
rabbitmq_send_message <- function(url, auth, body, action="POST") {
  if (action == "GET") {
    result <- httr::GET(url, auth, body = jsonlite::toJSON(body, auto_unbox = TRUE))
  } else if (action == "PUT") {
    result <- httr::PUT(url, auth, body = jsonlite::toJSON(body, auto_unbox = TRUE))
  } else if (action == "DELETE") {
    result <- httr::DELETE(url, auth, body = jsonlite::toJSON(body, auto_unbox = TRUE))
  } else if (action == "POST") {
    result <- httr::POST(url, auth, body = jsonlite::toJSON(body, auto_unbox = TRUE))
  } else {
    PEcAn.logger::logger.error(paste("error sending message to rabbitmq, uknown action", action))
    return(NA)
  }

  if (result$status_code >= 200 && result$status_code < 300) {
    content <- httr::content(result)
    if (length(content) == 0) {
      return("")
    } else {
      return(content)
    }
  } else if (result$status_code == 401) {
    PEcAn.logger::logger.error("error sending message to rabbitmq, make sure username/password is correct")
    return(NA)
  } else {
    output <- httr::content(result)
    if ("reason" %in% names(output)) {
      PEcAn.logger::logger.error(paste("error sending message to rabbitmq,", output$reason))
    } else {
      PEcAn.logger::logger.error("error sending message to rabbitmq")
    }
    return(NA)
  }
}

#' Post message to RabbitMQ. This will submit a message to RabbitMQ, if the
#' queue does not exist it will be created. The message will be converted to
#' a json message that is submitted.
#'
#' @param uri RabbitMQ URI or URL to rest endpoint
#' @param queue the queue the message is submitted to
#' @param message the message to submit, will beconverted to json.
#' @param prefix prefix for the rabbitmq api endpoint, default is for no prefix.
#' @param port port for the management interface, the default is 15672.
#' @return the result of the post if message was send, or NA if it failed.
#' @author Alexey Shiklomanov, Rob Kooper
#' @export
rabbitmq_post <- function(uri, queue, message, prefix="", port=15672) {
  # parse rabbitmq URI
  rabbitmq <- rabbitmq_parse_uri(uri, prefix, port)
  if (length(rabbitmq) != 4) {
    return(NA)
  }

  # create authentication
  auth <- httr::authenticate(rabbitmq$username, rabbitmq$password)

  # create message to be send to create the queue
  body <- list(
    auto_delete = FALSE,
    durable = FALSE
  )
  url <- paste0(rabbitmq$url, "api/queues/", rabbitmq$vhost, "/", queue)
  if (is.na(rabbitmq_send_message(url, auth, body, "PUT"))) {
    return(NA)
  }

  # send actual message to queue
  body <- list(
    properties = list(delivery_mode = 2),
    routing_key = queue,
    payload = jsonlite::toJSON(message, auto_unbox = TRUE),
    payload_encoding = "string"
  )
  url <- paste0(rabbitmq$url, "api/exchanges/", rabbitmq$vhost, "//publish")
  return(rabbitmq_send_message(url, auth, body, "POST"))
}

#' Get message from RabbitMQ. This will get a message from RabbitMQ, if the
#' queue does not exist it will be created. The message will be converted to
#' a json message that is returned.
#'
#' @param uri RabbitMQ URI or URL to rest endpoint
#' @param queue the queue the message is received from.
#' @param count the number of messages to retrieve from the queue.
#' @param port port for the management interface, the default is 15672.
#' @return NA if no message was retrieved, or a list of the messages payload.
#' @author Alexey Shiklomanov, Rob Kooper
#' @export
rabbitmq_get <- function(uri, queue, count=1, prefix="", port=15672) {
  # parse rabbitmq URI
  rabbitmq <- rabbitmq_parse_uri(uri, prefix, port)
  if (length(rabbitmq) != 4) {
    return(NA)
  }

  # create authentication
  auth <- httr::authenticate(rabbitmq$username, rabbitmq$password)

  # create message to be send to create the queue
  body <- list(
    auto_delete = FALSE,
    durable = FALSE
  )
  url <- paste0(rabbitmq$url, "api/queues/", rabbitmq$vhost, "/", queue)
  if (is.na(rabbitmq_send_message(url, auth, body, "PUT"))) {
    return(NA)
  }

  # get actual message from queue
  body <- list(
    count = count,
    ackmode = "ack_requeue_false",
    encoding = "auto"
  )
  url <- paste0(rabbitmq$url, "api/queues/", rabbitmq$vhost, "/", queue, "/get")
  result <- rabbitmq_send_message(url, auth, body, "POST")
  if (length(result) == 1 && is.na(result)) {
    return(NA)
  } else {
    return(lapply(result, function(x) { tryCatch(jsonlite::fromJSON(x$payload), error=function(e) { x$payload }) }))
  }
}
