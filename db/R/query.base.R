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
  data  <- dbGetQuery(con, query)
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
##' Clone a previously opened connection to a database.
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

db.print.connections <- function() {
  print(paste("Created", .local$created, "connections."))
  if (length(.local$connections$id) == 0) {
    cat("No open database connections.\n")
  } else {
    for(x in 1:length(.local$connections$id)) {
      cat(paste("Connection", x, "with id", .local$connections$id[[x]], "was created at:\n"))
      cat(paste("\t", names(.local$connections$log[[x]]), "\n"))
      cat("\t database object : ")
      print(.local$connections$con[[x]])
    }
  }
}

#---------------- Base database query function. ---------------------------------------------------#
##' Generic function to query trait database
##'
##' Given a connection and a query, will return a query as a data frame
##'
##' Deprecated, please use db.query
##'
##' @name query.base
##' @title Query database
##' @param query SQL query string
##' @param con database connection object
##' @param ... optional arguments for connecting to database (e.g. password, user name, database)
##' @return data frame with query results
##' @export
##' @examples
##' \dontrun{
##' query.base('select count(id) from traits;')
##' }
query.base <- function(query, con=NULL, ...){
  iopened <- 0
  if(is.null(con)){
    con <- db.open(settings$database)
    iopened <- 1
  }
  data  <- dbGetQuery(con, query)
  if(iopened==1) {
    dbDisconnect(con)
  }
  invisible(data)
}
#==================================================================================================#

#---------------- Base database connection function. ----------------------------------------------#
##' Creates database connection object.
##'
##' Also removes any existing connections. 
##'
##' Deprecated, please use db.open
##'
##' @name query.base.con
##' @title Query database connection
##' @param ... optional arguments for connecting to database (e.g. password, user name, database)
##' @return database connection object
##' @export
##' @examples
##' \dontrun{
##' con <- query.base.con(settings)
##' }
query.base.con <- function(settings,...){
  return(db.open(settings$database))
}
#==================================================================================================#

#---------------- Close open database connections. --------------------------------------------#
##' Close database connection
##'
##' Deprecated, please use db.close
##'
##' Closes a database connection
##' @name query.close
##' @title Close existing database connections 
##' @param con database connection object
##' @return nothing, as a side effect closes all open connections
##' @author Rob Kooper
##' @export
query.close <- function(con) {
  invisible(db.close(con))
}
#==================================================================================================#

#---------------- Close all open database connections. --------------------------------------------#
##' Kill existing database connections
##'
##' Deprecated, this should never be called
##'
##' resolves (provides workaround to) bug #769 caused by having too many open connections \code{Error during wrapup: RS-DBI driver: (cannot allocate a new connection -- maximum of 16 connections already opened)}
##' @name killdbcons
##' @title Kill existing database connections 
##' @return nothing, as a side effect closes all open connections
##' @author David LeBauer
killdbcons <- function(){
  for (i in dbListConnections(MySQL())) db.close(i)
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.      				
####################################################################################################
