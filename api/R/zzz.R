#' Package-specific options
#'
#' To minimize the number of changes that have to happen for scripts
#' using `pecanapi` to be shared across machines, most
#' Docker/RabbitMQ/database-related configurations can be configured
#' via `options`. All options take the form `pecanapi.optionname`,
#' where `optionname` is the specific option. Note that these values
#' are used only as default function arguments, and can be substituted
#' for any individual function call by passing the appropriate argument.
#'
#' The following options (prefixed with `pecanapi.`) are used by
#' `pecanapi`. To see the default values, just call `options()` with
#' no arguments. These are sorted in order of decreasing likelihood of
#' needing to be set by the user (first options are most likely to be
#' changed across different systems):
#'
#' - `docker_hostname`, `docker_port` -- The hostname and port of the
#' Docker service. You can check that these values work by browsing to
#' `docker_hostname:docker_port` (by default, `localhost:8000`) in a
#' web browser.
#'
#' - `docker_rabbitmq_frontend` -- The "frontend rule" for RabbitMQ.
#' By default, this is `/rabbitmq`, meaning that the RabbitMQ console
#' is accessible at `localhost:8000/rabbitmq` (adjusted for whatever
#' combination of `docker_hostname` and `docker_port` you are using).
#'
#' `docker_https` -- (Logical) If `TRUE`, all URLs use `https` access.
#' By default, this is `FALSE`.
#'
#' - `db_hostname` -- The name of the PostgreSQL container service
#' inside the PEcAn stack. This is the same as its service name in
#' `docker-compose.yml`. This is the hostname used by the `executor`
#' service to access the database, and which is written into each
#' `pecan.xml` file.
#'
#' - `db_user`, `db_password`, `db_dbname`, `db_driver`, `db_write` --
#' These correspond to the `user`, `password`, `dbname`, `driver`, and
#' `write` tags in the `database/bety` part of the PEcAn XML.
#'
#' - `rabbitmq_user`, `rabbitmq_password` -- The RabbitMQ
#' authentication credentials. These are set in the
#' `docker-compose.yml` file, under the `rabbitmq` service.
#'
#' - `rabbitmq_service`, `rabbitmq_service_port`, `rabbitmq_vhost` --
#' The name, internal port, and `vhost` of the RabbitMQ service.
#' Unless you are making major changes to the guts of
#' `docker-compose.yml`, you shouldn't change these values (i.e. they
#' should be the same on most machines).
#'
#' - `workflow_hostname` -- The hostname passed to the `host` section
#' of the `pecan.xml`. By default, this is "docker".
#'
#' - `workflow_prefix` -- The location and directory prefix for
#' storing workflow outputs. By default, this is
#' `/data/workflows/PEcAn_`. The workflow ID will be appended directly
#' to this value.
#' @name pecanapi_options
NULL

.onLoad <- function(libname, packagename) {
  op <- options()
  api_opts <- list(
    # Docker options (submit_workflow)
    pecanapi.docker_hostname = "localhost",
    pecanapi.docker_port = 8000,
    pecanapi.docker_rabbitmq_frontend = "/rabbitmq",
    pecanapi.docker_https = FALSE,
    # Database settings (add_database)
    pecanapi.db_hostname = "postgres",
    pecanapi.db_user = "bety",
    pecanapi.db_password = "bety",
    pecanapi.db_dbname = "bety",
    pecanapi.db_driver = "PostgreSQL",
    pecanapi.db_dbfiles = "/data/dbfiles",
    pecanapi.db_write = TRUE,
    # Workflow options (insert_new_workflow)
    pecanapi.workflow_hostname = "docker",
    pecanapi.workflow_prefix = "/data/workflows/PEcAn_",
    # RabbitMQ options (add_rabbitmq)
    pecanapi.rabbitmq_user = "guest",
    pecanapi.rabbitmq_password = "guest",
    pecanapi.rabbitmq_service = "rabbitmq",
    pecanapi.rabbitmq_service_port = 5672,
    pecanapi.rabbitmq_vhost = "%2F"
  )
  toset <- !(names(api_opts) %in% names(op))
  if (any(toset)) options(api_opts[toset])

  invisible()
}
