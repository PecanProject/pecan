#' Post complete settings list as RabbitMQ message
#'
#' @param settings PEcAn settings list object
#' @param rabbitmq_hostname RabbitMQ server hostname (character.
#'   Default = `"localhost"`)
#' @param rabbitmq_port RabbitMQ server port (character or numeric.
#'   Default = 8000)
#' @param rabbitmq_user RabbitMQ user name (character. Default = `"guest"`)
#' @param rabbitmq_password RabbitMQ password (character. Default = `"guest"`)
#' @param rabbitmq_prefix Complete RabbitMQ API prefix. If `NULL`
#'   (default), this is constructed from the other arguments. If this
#'   argument is not `NULL`, it overrides all other arguments except
#'   `httr_auth` and `settings`.
#' @param httr_auth Whether or not to use [httr::authenticate] to
#'   generate CURL authentication (default = `TRUE`). If `FALSE`, you
#'   must pass the authentication as part of the RabbitMQ hostname or prefix.
#' @param https Whether or not to use `https`. If `FALSE` (default),
#'   use `http`.
#' @return Curl `POST` output, parsed by [httr::content]
#' @author Alexey Shiklomanov
#' @export
submit_workflow <- function(settings,
                            rabbitmq_hostname = "localhost",
                            rabbitmq_frontend = "/rabbitmq",
                            rabbitmq_port = 8000,
                            rabbitmq_user = "guest",
                            rabbitmq_password = "guest",
                            rabbitmq_prefix = NULL,
                            httr_auth = TRUE,
                            https = FALSE) {
  if (is.numeric(rabbitmq_port)) rabbitmq_port <- as.character(rabbitmq_port)
  settings_json <- jsonlite::toJSON(list(pecan_json = settings), auto_unbox = TRUE)
  bod_raw <- list(
    properties = list(delivery_mode = 2),
    routing_key = "pecan",
    payload = settings_json,
    payload_encoding = "string"
  )
  auth <- NULL
  if (httr_auth) {
    auth <- httr::authenticate(rabbitmq_user, rabbitmq_password)
  } 
  bod <- jsonlite::toJSON(bod_raw, auto_unbox = TRUE)
  if (is.null(rabbitmq_prefix)) {
    httpstring <- "http"
    if (https) httpstring <- "https"
    rabbitmq_prefix <- sprintf(
      "%s://%s:%s%s",
      httpstring, rabbitmq_hostname, rabbitmq_port, rabbitmq_frontend
    )
  }
  result <- httr::POST(
    paste0(rabbitmq_prefix, "/api/exchanges/%2F//publish"),
    auth,
    body = bod
  )
  httr::content(result)
}
