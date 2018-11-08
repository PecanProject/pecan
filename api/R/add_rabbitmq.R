#' Add RabbitMQ configuration
#'
#' @inheritParams add_workflow
#' @param model_queue Name of RabbitMQ model queue (character, default
#'   = `NULL`). This should be in the form `modelname_modelrevision`.
#'   If this is `NULL`, this function will try to figure it out based
#'   on the model ID in the settings object, which requires access to
#'   the database (i.e. `con` must not be `NULL`).
#' @param con Database connection object (default = `NULL`). Ignored
#'   unless `model_queue` is `NULL`. 
#' @param rabbitmq_user Username for RabbitMQ server (character,
#'   default = "guest")
#' @param rabbitmq_password Password for RabbitMQ server (character,
#'   default = "guest")
#' @param rabbitmq_service Name of RabbitMQ `docker-compose` service
#'   (character, default = "rabbitmq")
#' @param rabbitmq_service_port RabbitMQ service port (numeric or
#'   character, default = 5672). Note that this is internal to the
#'   Docker stack, _not_. The only reason this should be changed is if you
#'   changed low-level RabbitMQ sett
#' @param rabbitmq_vhost RabbitMQ vhost (character, default = "%2f").
#'   The only reason this should be changed is if you change the
#'   low-level RabbitMQ setup in the `docker-compose.yml` file.
#' @return Modified settings list with RabbitMQ configuration added.
#' @author Alexey Shiklomanov
#' @export
add_rabbitmq <- function(settings,
                         model_queue = NULL,
                         con = NULL,
                         rabbitmq_user = "guest",
                         rabbitmq_password = "guest",
                         rabbitmq_service = "rabbitmq",
                         rabbitmq_service_port = 5672,
                         rabbitmq_vhost = "%2f",
                         overwrite = FALSE) {
  if (is.null(model_queue) && is.null(settings[["rabbitmq"]][["queue"]])) {
    # Deduce model queue from settings and database
    if (is.null(con)) {
      stop("Database connection object (`con`) required to automatically determine model queue.")
    }
    model_id <- settings[["model"]][["id"]]
    if (is.null(model_id)) {
      stop("Settings list must include model ID to automatically determine model queue.")
    }
    model_dat <- param_query(con, (
      "SELECT model_name, revision FROM models WHERE id = $1"
    ), list(model_id))
    if (!nrow(model_dat) > 0) stop("Multiple models found. Unable to automatically determine model queue.")
    model_queue <- paste(model_dat[["model_name"]], model_dat[["revision"]], sep = "_")
  }

  rabbitmq_settings <- list(
    uri = sprintf("amqp://%s:%s@%s:%d/%s",
                  rabbitmq_user, rabbitmq_password,
                  rabbitmq_service, rabbitmq_service_port, rabbitmq_vhost),
    queue = model_queue
  )
  new_settings <- modifyList(settings, list(host = list(rabbitmq = rabbitmq_settings)))
  if (!overwrite) {
    new_settings <- modifyList(new_settings, settings)
  }
  new_settings
}
