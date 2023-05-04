#' Start model execution using rabbitmq
#'
#' @param folder Directory containing jobs to be started
#' @param rabbitmq_uri RabbitMQ uri where messages should be posted
#' @param rabbitmq_queue Queue to which messages are submitted
#' @return Output of execution command, as a character
#'  (see [rabbitmq_post_message()]).
#' @export
start_rabbitmq <- function(folder, rabbitmq_uri, rabbitmq_queue) {
  message <- list("folder"=folder)
  prefix <- Sys.getenv("RABBITMQ_PREFIX", "")
  port <- Sys.getenv("RABBITMQ_PORT", "15672")
  out <- rabbitmq_post_message(rabbitmq_uri, rabbitmq_queue, message, prefix, port)
  return(out)
}
