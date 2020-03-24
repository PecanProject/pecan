#' Start model execution using rabbitmq
#'
#' @return Output of execution command, as a character (see [remote.execute.cmd()]).
start_rabbitmq <- function(folder, rabbitmq_uri, rabbitmq_queue) {
  out <- system2('python3', c('/work/sender.py', rabbitmq_uri, rabbitmq_queue, folder), stdout = TRUE, stderr = TRUE)
  return(out)
}
