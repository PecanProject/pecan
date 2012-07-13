#---------------- Base database query function. ---------------------------------------------------#
##' Generic function to query trait database
##'
##' Given a connection and a query, will return a query as a data frame
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
query.base <- function(query, con=NULL,...){
  iopened <- 0
  if(is.null(con)){
    con <- query.base.con(settings)
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
  lapply(dbListConnections(MySQL()), dbDisconnect) #first kill all connections
  dvr <- dbDriver ("MySQL")
  
  #con <- dbConnect(dvr, group  = 'ebi_analysis',...)
  con <- dbConnect(dvr, group  = settings$database$name,
                   dbname = settings$database$name, 
                   password = settings$database$passwd, 
                   username = settings$database$userid, 
                   host = settings$database$host)
                   
  #KLUDGE: not all invocations of query.base.con() make use of the settings file.
  #This effectively limits PEcAn to using ebi_analysis at certain places.
  #What follows is a quick fix - it relies on settings as a global variable,
  #which are generally recommended against
  #if(exists('settings')){
  #  con <- dbConnect(dvr, group  = settings$database$name,
  #      dbname = settings$database$name, 
  #      password = settings$database$passwd, 
  #      username = settings$database$userid, 
  #      host = settings$database$host)
  #  return(con)
 #}
  
  return(con)
}
#==================================================================================================#

#---------------- Close open database connections. --------------------------------------------#
##' Close database connection
##'
##' Closes a database connection
##' @name query.close
##' @title Close existing database connections 
##' @param con database connection object
##' @return nothing, as a side effect closes all open connections
##' @author Rob Kooper
##' @export
query.close <- function(con) {
  invisible(dbDisconnect(con))
}
#==================================================================================================#

#---------------- Close all open database connections. --------------------------------------------#
##' Kill existing database connections
##'
##' resolves (provides workaround to) bug #769 caused by having too many open connections \code{Error during wrapup: RS-DBI driver: (cannot allocate a new connection -- maximum of 16 connections already opened)}
##' @name killdbcons
##' @title Kill existing database connections 
##' @return nothing, as a side effect closes all open connections
##' @author David LeBauer
killdbcons <- function(){
  for (i in dbListConnections(MySQL())) dbDisconnect(i)
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.      				
####################################################################################################
