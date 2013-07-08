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
.db.utils$showquery <- FALSE
.db.utils$connections <- list()

#---------------- Base database query function. ---------------------------------------------------#
##' Generic function to query database
##'
##' Given a connection and a query, will return a query as a data frame. Either con or params need
##' to be specified. If both are specified it will use con.
##' @name db.query
##' @title Query database
##' @param query SQL query string
##' @param con database connection object
##' @param params database connection information
##' @return data frame with query results
##' @author Rob Kooper
##' @export
##' @examples
##' \dontrun{
##' db.query('select count(id) from traits;', params=settings$database)
##' }
db.query <- function(query, con=NULL, params=NULL) {
  iopened <- 0
  if(is.null(con)){
    if (is.null(params)) {
      logger.error("No parameters or connection specified")
      stop()
    }
    con <- db.open(params)
    iopened <- 1
  }
  if (.db.utils$showquery) {
    logger.debug(query)
  }
  data <- dbGetQuery(con, query)
  res <- dbGetException(con)
  if (res$errorNum != 0 || (res$errorMsg != 'OK' && res$errorMsg != '')) {
    logger.severe(paste("Error executing db query '", query, "' errorcode=", res$errorNum, " message='", res$errorMsg, "'", sep=''))
  }
  .db.utils$queries <- .db.utils$queries+1
  if(iopened==1) {
    db.close(con)
  }
  invisible(data)
}

##' Generic function to open a database connection
##'
##' Create a connection to a database usign the specified parameters. If the paramters contain
##' driver element it will be used as the database driver, otherwise it will use MySQL.
##' @name db.open
##' @title Open database connection
##' @param params database connection information
##' @return connection to database
##' @author Rob Kooper
##' @export
##' @examples
##' \dontrun{
##' db.open(settings$database)
##' }
db.open <- function(params) {
  params$dbfiles <- NULL
  if (is.null(params$driver)) {
    args <- c(drv=dbDriver("MySQL"), params, recursive=TRUE)
  } else {
    args <- c(drv=dbDriver(params$driver), params, recursive=TRUE)
    args[['driver']] <- NULL
  }
  c <- do.call(dbConnect, as.list(args))
  id <- sample(1000, size=1)
  while(length(which(.db.utils$connections$id==id)) != 0) {
    id <- sample(1000, size=1)
  }
  attr(c, "pecanid") <- id
  dump.log <- NULL
  dump.frames(dumpto="dump.log")
  .db.utils$created <- .db.utils$created+1
  .db.utils$connections$id <- append(.db.utils$connections$id, id)
  .db.utils$connections$con <- append(.db.utils$connections$con, c)
  .db.utils$connections$log <- append(.db.utils$connections$log, list(dump.log))
  invisible(c)
}

##' Generic function to close a database connection
##'
##' Close a previously opened connection to a database.
##' @name db.close
##' @title Close database connection
##' @param con database connection to be closed
##' @return connection to database
##' @author Rob Kooper
##' @export
##' @examples
##' \dontrun{
##' db.close(con)
##' }
db.close <- function(con) {
  if (is.null(con)) {
    return
  }

  id <- attr(con, "pecanid")
  if (is.null(id)) {
    logger.warn("Connection created outside of PEcAn.db package")
  } else {
    deleteme <- which(.db.utils$connections$id==id)
    if (length(deleteme) == 0) {
      logger.warn("Connection might have been closed already.");
    } else {
      .db.utils$connections$id <- .db.utils$connections$id[-deleteme]
      .db.utils$connections$con <- .db.utils$connections$con[-deleteme]
      .db.utils$connections$log <- .db.utils$connections$log[-deleteme]
    }
  }
  dbDisconnect(con)
}

##' Debug method for db.open and db.close
##'
##' Prints the number of connections opened as well as any connections
##' that have never been closes.
##' @name db.print.connections
##' @title Debug leaked connections
##' @author Rob Kooper
##' @export
##' @examples
##' \dontrun{
##' db.print.connections()
##' }
db.print.connections <- function() {
  logger.info("Created", .db.utils$created, "connections and executed", .db.utils$queries, "queries")
  if (length(.db.utils$connections$id) == 0) {
    logger.debug("No open database connections.\n")
  } else {
    for(x in 1:length(.db.utils$connections$id)) {
      logger.info(paste("Connection", x, "with id", .db.utils$connections$id[[x]], "was created at:\n"))
      logger.info(paste("\t", names(.db.utils$connections$log[[x]]), "\n"))
#      cat("\t database object : ")
#      print(.db.utils$connections$con[[x]])
    }
  }
}

##' Test connection to database
##' 
##' Useful to only run tests that depend on database when a connection exists
##' @title db.exists
##' @param params database connection information
##' @return TRUE if database connection works; else FALSE
##' @export
##' @author David LeBauer, Rob Kooper
db.exists <- function(params, write=TRUE) {
	# open connection
	con <- tryCatch({
		invisible(db.open(params))
	}, error = function(e) {
		logger.error("Could not connect to database.\n\t", e)
		invisible(NULL)
	})
	if (is.null(con)) {
		return(invisible(FALSE))
	}

	# read a row from the database
	read.result <- tryCatch({
		invisible(db.query("SELECT * FROM users LIMIT 1", con))
	}, error = function(e) {
		logger.error("Could not query database.\n\t", e)
		db.close(con)
		invisible(NULL)
	})
	if (is.null(read.result)) {
	  return(invisible(FALSE))
	}
	
	# if requested write a row to the database
	if (write) {
		result <- tryCatch({
			db.query(paste("UPDATE users SET created_at='", read.result$created_at, "' WHERE id=", read.result$id, sep=""), con)
			invisible(TRUE)
		}, error = function(e) {
			logger.error("Could not write to database.\n\t", e)
			invisible(FALSE)
		})
	} else {
    result <- TRUE
  }

	# close database, all done
	tryCatch({
		db.close(con)
	}, error = function(e) {
		logger.warn("Could not close database.\n\t", e)
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
