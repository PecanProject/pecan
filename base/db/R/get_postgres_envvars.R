#' Look up Postgres connection parameters from environment variables
#'
#' Retrieves database connection parameters stored in any of the
#'  environment variables known by Postgres,
#'  using defaults from `...` for parameters not set in the environment.
#'  In a standard PEcAn installation only a few of these parameters
#'  will ever be set, but we check all of them anyway in case you need to do
#'  anything unusual.
#'
#' The list of environment variables we check is taken from the
#'  [Postgres 12 manual](https://postgresql.org/docs/12/libpq-envars.html),
#'  but it should apply to older Postgres versions as well.
#'  Note that this function only looks for environment variables that control
#'  connection parameters; it does not retrieve any of the variables related to
#'  per-session behavior (e.g. PGTZ, PGSYSCONFDIR).
#'
#' @param ... defaults for parameters not found in the environment,
#'  in `name = value` form
#' @return list of connection parameters suitable for passing on to `db.open`
#'
#' @examples
#'  host <- Sys.getenv("PGHOST") # to restore environment after demo
#'
#'  Sys.unsetenv("PGHOST")
#'  get_postgres_envvars()$host # NULL
#'  get_postgres_envvars(host = "default", port = 5432)$host # "default"

#'  # defaults are ignored for a variable that exists
#'  Sys.setenv(PGHOST = "localhost")
#'  get_postgres_envvars()$host # "localhost"
#'  get_postgres_envvars(host = "postgres")$host # still "localhost"
#'
#'  # To override a set variable, edit the returned list before using it
#'  con_parms <- get_postgres_envvars()
#'  con_parms$host # "localhost"
#'  con_parms$host <- "postgres"
#'  # db.open(con_parms)
#'
#'  Sys.setenv(PGHOST = host)
#' @export
get_postgres_envvars <- function(...) {
  pg_vars <- list(
    host = "PGHOST",
    hostaddr = "PGHOSTADDR",
    port = "PGPORT",
    dbname = "PGDATABASE",
    user = "PGUSER",
    password = "PGPASSWORD",
    passfile = "PGPASSFILE",
    service = "PGSERVICE",
    options = "PGOPTIONS",
    application_name = "PGAPPNAME",
    ssl_mode = "PGSSLMODE",
    requiressl = "PGREQUIRESSL",
    sslcompression = "PGSSLCOMPRESSION",
    sslcert = "PGSSLCERT",
    sslkey = "PGSSLKEY",
    sslrootcert = "PGSSLROOTCERT",
    sslcrl = "PGSSLCRL",
    requirepeer = "PGREQUIREPEER",
    krbsrvname = "PGKRBSRVNAME",
    gsslib = "PGGSSLIB",
    connect_timeout = "PGCONNECT_TIMEOUT",
    client_encoding = "PGCLIENTENCODING",
    target_session_attrs = "PGTARGETSESSIONATTRS")

  vals <- Sys.getenv(pg_vars)
  names(vals) <- names(pg_vars)
  vals <- vals[vals != ""]

  defaults <- list(...)
  defaults <- defaults[!(names(defaults) %in% names(vals))]

  append(vals, defaults)
}
