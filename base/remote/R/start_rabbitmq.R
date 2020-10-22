#' Start model execution using rabbitmq
#'
#' @return Output of execution command, as a character (see [rabbitmq_post_message()]).
start_rabbitmq <- function(folder, rabbitmq_uri, rabbitmq_queue) {
  message <- list("folder"=folder)
  prefix <- Sys.getenv("RABBITMQ_PREFIX", "")
  port <- Sys.getenv("RABBITMQ_PORT", "15672")
  out <- rabbitmq_post_message(rabbitmq_uri, rabbitmq_queue, message, prefix, port)
  return(out)
}
