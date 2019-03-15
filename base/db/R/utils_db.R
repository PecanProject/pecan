#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

.db.utils <- new.env()
.db.utils$created <- 0
.db.utils$queries <- 0
.db.utils$deprecated <- 0
.db.utils$showquery <- FALSE
.db.utils$connections <- list()

#---------------- Base database query function. ---------------------------------------------------#
##' Generic function to query database
##'
##' Given a connection and a query, will return a query as a data frame. Either con or params need
##' to be specified. If both are specified it will use con.
##'
##' @param query SQL query string
##' @param con Database connection object
##' @param params Named list of database connection parameters. See
##'   `params` argument to [db.open()].
##' @return data frame with query results
##' @author Rob Kooper, Alexey Shiklomanov
##' @export
##' @examples
##' \dontrun{
##' db.query("SELECT count(id) FROM traits;", params = settings$database$bety)
##' }
db.query <- function(query, con = NULL, params = NULL) {
  if (is.null(con)){
    if (is.null(params)) {
      PEcAn.logger::logger.severe("No parameters or connection specified")
    }
    con <- db.open(params)
    on.exit(db.close(con))
  }
  if (.db.utils$showquery) {
    PEcAn.logger::logger.debug(query)
  }
  data <- DBI::dbGetQuery(con, query)
  # The newer RPostgres driver doesn't have a dbGetException method.
  # If the query fails, it throws an error directly in R.
  if (inherits(con, "PostgreSQLConnection")) {
    res <- DBI::dbGetException(con)
    if (res$errorNum != 0 || (res$errorMsg != "OK" && res$errorMsg != "")) {
      PEcAn.logger::logger.severe(
        paste0("Error executing db query '", query,
              "' errorcode=", res$errorNum,
              " message='", res$errorMsg, "'")
      )
    }
  }
  .db.utils$queries <- .db.utils$queries + 1
  invisible(data)
}

##' Open a database connection
##'
##' Create a connection to a database using the specified parameters.
##' The `params` list will be passed as arguments to [DBI::dbConnect()].
##'
##' Typical arguments are as follows:
##' - `driver` -- The name of the database driver. Only `"PostgreSQL"`
##' and `"Postgres"` are supported. If no driver is specified, default
##' to `"PostgreSQL"`.
##' - `user` -- The database username. For local instances of PEcAn,
##' this is usually `"bety"`.
##' - `password` -- The database password. For local instances of
##' PEcAn, this is usually `"bety"`.
##' - `host` -- The database hostname. For local instances of PEcAn,
##' this is usually `"localhost"`. Inside the PEcAn Docker stack, this
##' may be `"postgres"`.
##' - `port` (optional) -- The port for accessing the database. If
##' omitted, this will use the PostgreSQL default (5432).
##'
##' @param params Named list of database connection options. See details
##' @return Database connection object
##' @author Rob Kooper
##' @export
##' @examples
##' \dontrun{
##' db.open(settings$database$bety)
##' }
db.open <- function(params) {
  # These values are used elsewhere, but are not valid arguments to
  # DBI::dbConnect, so remove them here.
  params[["dbfiles"]] <- NULL
  params[["write"]] <- NULL

  driver <- params[["driver"]]
  params[["driver"]] <- NULL
  if (is.null(driver)) {
    PEcAn.logger::logger.info(
      "Missing `driver` argument. ",
      "Assuming `RPostgreSQL::PostgreSQL()` driver."
    )
    driver <- "PostgreSQL"
  }

  if (!driver %in% c("PostgreSQL", "Postgres")) {
    PEcAn.logger::logger.severe(paste0(
      "Driver `", driver, "` is not supported. ",
      'You must use either `"PostgreSQL"` or `"Postgres"`.'
    ))
  }

  if (driver == "PostgreSQL") {
    drv <- RPostgreSQL::PostgreSQL()
  } else if (driver == "Postgres") {
    drv <- RPostgres::Postgres()
  }

  args <- c(drv = drv, params, recursive = TRUE)

  c <- do.call(DBI::dbConnect, as.list(args))

  # Assign the connection a unique ID number between 0 and 1000
  id <- sample(1000, size = 1)
  while (any(.db.utils$connections$id == id)) {
    id <- sample(1000, size = 1)
  }
  attr(c, "pecanid") <- id
  dump.log <- NULL
  utils::dump.frames(dumpto = "dump.log")
  .db.utils$created <- .db.utils$created+1
  .db.utils$connections$id <- append(.db.utils$connections$id, id)
  .db.utils$connections$con <- append(.db.utils$connections$con, c)
  .db.utils$connections$log <- append(.db.utils$connections$log, list(dump.log))
  invisible(c)
}

##' Generic function to close a database connection
##'
##' Close a previously opened connection to a database.
##' @param con database connection to be closed
##' @param showWarnings logical: report possible issues with connection?
##' @return `TRUE`, invisibly (see [DBI::dbDisconnect()])
##' @author Rob Kooper
##' @export
##' @examples
##' \dontrun{
##' db.close(con)
##' }
db.close <- function(con, showWarnings = TRUE) {
  if (is.null(con)) {
    return(invisible(TRUE))
  }

  id <- attr(con, "pecanid")
  if (showWarnings && is.null(id)) {
    PEcAn.logger::logger.warn("Connection created outside of PEcAn.DB package")
  } else {
    deleteme <- which(.db.utils$connections$id == id)
    if (showWarnings && length(deleteme) == 0) {
      PEcAn.logger::logger.warn("Connection might have been closed already.");
    } else {
      .db.utils$connections$id <- .db.utils$connections$id[-deleteme]
      .db.utils$connections$con <- .db.utils$connections$con[-deleteme]
      .db.utils$connections$log <- .db.utils$connections$log[-deleteme]
    }
  }
  DBI::dbDisconnect(con)
}

##' Debug leaked connections
##'
##' Prints the number of connections opened as well as any connections
##' that have never been closes.
##' @author Rob Kooper
##' @export
##' @examples
##' \dontrun{
##' db.print.connections()
##' }
db.print.connections <- function() {
  PEcAn.logger::logger.info("Created", .db.utils$created, "connections and executed", .db.utils$queries, "queries")
  if (.db.utils$deprecated > 0) {
    PEcAn.logger::logger.info("Used", .db.utils$deprecated, "calls to deprecated functions")
  }
  PEcAn.logger::logger.info("Created", .db.utils$created, "connections and executed", .db.utils$queries, "queries")
  if (length(.db.utils$connections$id) == 0) {
    PEcAn.logger::logger.debug("No open database connections.\n")
  } else {
    for (x in 1:length(.db.utils$connections$id)) {
      PEcAn.logger::logger.info(paste("Connection", x, "with id", .db.utils$connections$id[[x]], "was created at:\n"))
      PEcAn.logger::logger.info(paste("\t", names(.db.utils$connections$log[[x]]), "\n"))
      #      cat("\t database object : ")
      #      print(.db.utils$connections$con[[x]])
    }
  }
}

##' Test connection to database
##'
##' Useful to only run tests that depend on database when a connection exists
##' @param params database connection information
##' @param write logical: test whether we have write access?
##' @param table name of database table to check
##' @return TRUE if database connection works; else FALSE
##' @export
##' @author David LeBauer, Rob Kooper
db.exists <- function(params, write = TRUE, table = NA) {
  # open connection
  con <- tryCatch({
    invisible(db.open(params))
  }, error = function(e) {
    PEcAn.logger::logger.error("Could not connect to database.\n\t", e)
    invisible(NULL)
  })
  if (is.null(con)) {
    return(invisible(FALSE))
  } else {
    on.exit(db.close(con))
  }

  #check table's privilege about read and write permission
  user.permission <<- tryCatch({
    invisible(db.query(
      paste0("SELECT privilege_type FROM information_schema.role_table_grants ",
             "WHERE grantee='", params$user,
             "' AND table_catalog = '", params$dbname,
             "' AND table_name='", table, "'"),
      con
    ))
  }, error = function(e) {
    PEcAn.logger::logger.error("Could not query database.\n\t", e)
    db.close(con)
    invisible(NULL)
  })

  # If table is NA, this is just a generic check for database access,
  # so we're done!
  if (is.na(table)) return(invisible(TRUE))

  # We're enquiring about permissions related to a specific table, so
  # need to do more here.
  read.perm <- FALSE
  write.perm <- FALSE

  # check read permission
  user_privilege <- user.permission[["privilege_type"]]
  if ("SELECT" %in% user_privilege) {
    read.perm <- TRUE
  }

  # read permission requested, but not granted
  if (!read.perm) return(invisible(FALSE))

  # Read permissions granted. Now, does it actually work? To test, try
  # to read a row from the database
  read.result <- tryCatch({
    invisible(db.query(query = paste("SELECT * FROM", table, "LIMIT 1"), con = con))
  }, error = function(e) {
    PEcAn.logger::logger.error("Could not query database.\n\t", e)
    db.close(con)
    invisible(NULL)
  })
  if (is.null(read.result)) return(invisible(FALSE))

  # get the table's primary key column
  get.key <- tryCatch({
    db.query(query = paste("SELECT pg_attribute.attname, format_type(pg_attribute.atttypid, pg_attribute.atttypmod)
                     FROM pg_index, pg_class, pg_attribute
                     WHERE
                     pg_class.oid = '", table, "'::regclass AND
                     indrelid = pg_class.oid AND
                     pg_attribute.attrelid = pg_class.oid AND
                     pg_attribute.attnum = any(pg_index.indkey)
                     AND indisprimary"), con = con)
  }, error = function(e) {
    PEcAn.logger::logger.error("Could not query database.\n\t", e)
    db.close(con)
    invisible(NULL)
  })
  if (is.null(read.result)) return(invisible(FALSE))

  # If write permission not requested, we're done!
  if (!write) return(invisible(TRUE))

  # Write permission requested. Was it granted?
  if ("INSERT" %in% user_privilege && "UPDATE" %in% user_privilege ) {
    write.perm <- TRUE
  }
  # Write permission not granted
  if (!write.perm) return(invisible(FALSE))

  # Write permission granted, but does it actually work?
  key <- get.key$attname
  key.value <- read.result[key]
  coln.name <- names(read.result)
  write.coln <- ""
  for (name in coln.name) {
    if (name != key) {
      write.coln <- name
      break
    }
  }
  write.value <- read.result[write.coln]
  result <- tryCatch({
    db.query(query = paste0("UPDATE ", table, " SET ", write.coln, "='", write.value,
                            "' WHERE ",  key, "=", key.value),
             con = con)
    invisible(TRUE)
  }, error = function(e) {
    PEcAn.logger::logger.error("Could not write to database.\n\t", e)
    invisible(FALSE)
  })

  invisible(result)
}

##' Sets if the queries should be shown that are being executed
##'
##' Useful to print queries when debuging SQL statements
##' @title db.showQueries
##' @param show set to TRUE to show the queries, FALSE by default
##' @export
##' @author Rob Kooper
db.showQueries <- function(show) {
  .db.utils$showquery <- show
}

##' Returns if the queries should be shown that are being executed
##'
##' @title db.getShowQueries
##' @return will return TRUE if queries are shown
##' @export
##' @author Rob Kooper
db.getShowQueries <- function() {
  invisible(.db.utils$showquery)
}

##' Retrieve id from a table matching query
##'
##' @title get.id
##' @param table name of table
##' @param colnames names of one or more columns used in where clause
##' @param values values to be queried in fields corresponding to colnames
##' @param con database connection object,
##' @param create logical: make a record if none found?
##' @param dates logical: update created_at and updated_at timestamps? Used only if `create` is TRUE
##' @return will numeric
##' @export
##' @author David LeBauer
##' @examples
##' \dontrun{
##' pftid <- get.id("pfts", "name", "salix", con)
##' pftid <- get.id("pfts", c("name", "modeltype_id"), c("ebifarm.salix", 1), con)
##' }
get.id <- function(table, colnames, values, con, create=FALSE, dates=FALSE){
  values <- lapply(values, function(x) ifelse(is.character(x), shQuote(x), x))
  where_clause <- paste(colnames, values , sep = " = ", collapse = " and ")
  query <- paste("select id from", table, "where", where_clause, ";")
  id <- db.query(query = query, con = con)[["id"]]
  if (is.null(id) && create) {
    colinsert <- paste0(colnames, collapse=", ")
    if (dates) colinsert <- paste0(colinsert, ", created_at, updated_at")
    valinsert <- paste0(values, collapse=", ")
    if (dates) valinsert <- paste0(valinsert, ", NOW(), NOW()")
    PEcAn.logger::logger.info("INSERT INTO ", table, " (", colinsert, ") VALUES (", valinsert, ")")
    db.query(query = paste0("INSERT INTO ", table, " (", colinsert, ") VALUES (", valinsert, ")"), con = con)
    id <- db.query(query, con)[["id"]]
  }
  return(id)
}

##' Convenience function to fix hostname if localhost
##'
##' @title default_hostname
##' @param hostname character
##' @return hostname
##' @export
default_hostname <- function(hostname) {
  if (hostname == "localhost") {
    hostname <- PEcAn.remote::fqdn();
  }
  return(hostname)
}
