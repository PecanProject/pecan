#' Add PEcAn database information to settings list
#'
#' @param settings Input settings list (list)
#' @param host Database server hostname (character, default = `pecanapi.db_hostname`)
#' @param user Database user name (character, default = option `pecanapi.db_username`))
#' @param password Database password (character, default = option `pecanapi.db_password`)
#' @param dbname Database name (character, default = option `pecanapi.db_dbname`)
#' @param driver Database driver (character, default = option `pecanapi.db_driver`)
#' @param dbfiles Path to `dbfiles` directory (character, default =
#'   option `pecanapi.db_dbfiles`)
#' @param write Whether or not to write to the database (logical,
#'   default = option `pecanapi.db_write`)
#' @param overwrite Whether or not to overwrite any already existing
#'   input settings (logical, default = `FALSE`)
#' @param ... Additional named PEcAn database configuration tags
#' @return Updated settings list with database information included
#' @author Alexey Shiklomanov
#' @export
add_database <- function(settings,
                         host = getOption("pecanapi.db_hostname"),
                         user = getOption("pecanapi.db_user"),
                         password = getOption("pecanapi.db_password"),
                         dbname = getOption("pecanapi.db_dbname"),
                         driver = getOption("pecanapi.db_driver"),
                         dbfiles = getOption("pecanapi.db_dbfiles"),
                         write = getOption("pecanapi.db_write"),
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
