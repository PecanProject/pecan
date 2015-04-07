#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

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
  .Deprecated("db.query")
  .db.utils$deprecated <- .db.utils$deprecated+1
  if(is.null(con)){
    invisible(db.query(query, params=settings$database$bety))
  } else {
    invisible(db.query(query, con))
  }
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
  .Deprecated("db.open")
  .db.utils$deprecated <- .db.utils$deprecated+1
  invisible(db.open(settings$database$bety))
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
  .Deprecated("db.close")
  .db.utils$deprecated <- .db.utils$deprecated+1
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
  .Deprecated("NeverCallThisFunction")
  .db.utils$deprecated <- .db.utils$deprecated+1
  for (i in dbListConnections(MySQL())) db.close(i)
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.      				
####################################################################################################
