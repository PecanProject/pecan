#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

.local <- new.env() 
.local$created <- 0
.local$queries <- 0
.local$connections <- list()

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
      logger.error("Excpected value to connect to database.")
      stop()
    }
    con <- db.open(params)
    iopened <- 1
  }
  logger.debug(query)
  data <- dbGetQuery(con, query)
  .local$queries <- .local$queries+1
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
  if (is.null(params$driver)) {
    args <- c(drv=dbDriver("MySQL"), params, recursive=TRUE)
  } else {
    args <- c(drv=dbDriver(params$driver), params, recursive=TRUE)
    args[['driver']] <- NULL
  }
  c <- do.call(dbConnect, as.list(args))
  id <- sample(1000, size=1)
  while(length(which(.local$connections$id==id)) != 0) {
    id <- sample(1000, size=1)
  }
  attr(c, "pecanid") <- id
  dump.frames(dumpto="dump.log")
  .local$created <- .local$created+1
  .local$connections$id <- append(.local$connections$id, id)
  .local$connections$con <- append(.local$connections$con, c)
  .local$connections$log <- append(.local$connections$log, list(dump.log))
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
  id <- attr(con, "pecanid")
  if (is.null(id)) {
    logger.warn("Connection created outside of PEcAn.db package")
  } else {
    deleteme <- which(.local$connections$id==id)
    if (length(deleteme) == 0) {
      logger.warn("Connection might have been closed already.");
    } else {
      .local$connections$id <- .local$connections$id[-deleteme]
      .local$connections$con <- .local$connections$con[-deleteme]
      .local$connections$log <- .local$connections$log[-deleteme]
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
  logger.info("Created", .local$created, "connections and executed", .local$queries, "queries")
  if (length(.local$connections$id) == 0) {
    logger.debug("No open database connections.\n")
  } else {
    for(x in 1:length(.local$connections$id)) {
      logger.info(paste("Connection", x, "with id", .local$connections$id[[x]], "was created at:\n"))
      logger.info(paste("\t", names(.local$connections$log[[x]]), "\n"))
#      cat("\t database object : ")
#      print(.local$connections$con[[x]])
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
		logger.error("Could not connect to database.", e)
		invisible(NULL)
	})
  if (is.null(con)) {
    invisible(FALSE)
  }

	# read a row from the database
	res <- tryCatch({
		invisible(db.query("SELECT * FROM users LIMIT 1", con))
	}, error = function(e) {
		logger.error("Could not query database.", e)
		db.close(con)
		invisible(NULL)
	})
	if (is.null(res)) {
	  invisible(FALSE)
	}
	
	# if requested write a row to the database
	if (write) {
		res <- tryCatch({
			invisible(db.query(paste("UPDATE users SET created_at='", res$created_at, "' WHERE id=", res$id, sep=""), con))
		}, error = function(e) {
			logger.error("Could not write to database.", e)
			db.close(con)
			invisible(NULL)
		})
		if (is.null(res)) {
		  invisible(FALSE)
		}
	}

	# close database, all done
	tryCatch({
		db.close(con)
	}, error = function(e) {
		logger.warn("Could not close database.", e)
	})

	invisible(TRUE)
}
