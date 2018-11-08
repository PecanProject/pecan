#' Add PEcAn database information to settings list
#'
#' @param settings Input settings list (list)
#' @param host Database server hostname (character, default =
#'   "postgres", so it works with Docker)
#' @param user Database user name (character, default = "bety")
#' @param password Database password (character, default = "bety")
#' @param dbname Database name (character, default = "bety")
#' @param driver Database driver (character, default = "PostgreSQL")
#' @param dbfiles Path to `dbfiles` directory (character, default = "/data/dbfiles")
#' @param write Whether or not to write to the database (logical,
#'   default = `TRUE`)
#' @param overwrite Whether or not to overwrite any already existing
#'   input settings (logical, default = `FALSE`)
#' @param ... Additional named PEcAn database configuration tags
#' @return Updated settings list with database information included
#' @author Alexey Shiklomanov
#' @export
add_database <- function(settings,
                         host = "postgres",
                         user = "bety",
                         password = "bety",
                         dbname = "bety",
                         driver = "PostgreSQL",
                         dbfiles = "/data/dbfiles/",
                         write = TRUE,
                         overwrite = FALSE,
                         ...) {
  bety_list <- list(
    host = host,
    user = user,
    password = password,
    dbname = dbname,
    driver = driver,
    write = write,
    ...
  )
  db_list <- list(database = list(bety = bety_list, dbfiles = dbfiles))
  new_settings <- modifyList(settings, db_list)
  if (!overwrite) new_settings <- modifyList(new_settings, settings)
  new_settings
}
