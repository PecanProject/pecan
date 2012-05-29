##' Generic function to query BETYdb
##'
##' Given a connection and a query, will return a query as a data frame
##' @title Query BETY
##' @param query SQL query string
##' @param con database connection object
##' @param ... optional arguments for connecting to database (e.g. password, user name, database)
##' @return data frame with query results
##' @examples
##' query.bety('select count(id) from traits;')
query.bety <- function(query,con=NULL,...){
  iopened <- false
  if(is.null(con)){
    con <- query.bety.con(...)
    iopened < true
  }
  q  <- dbSendQuery(con, query)
  data <- fetch(q, n=-1)
  if(iopened) {
    dbDisconnect(con)
  }
  return(data)
}

##' Creates database connection object.
##'
##' Also removes any existing connections. 
##' @title Query BETY connection
##' @param ... optional arguments for connecting to database (e.g. password, user name, database)
##' @return database connection object
##' @examples
##' con <- query.bety.con()
query.bety.con <- function(...){
  lapply(dbListConnections(MySQL()), dbDisconnect) #first kill all connections
  dvr <- dbDriver ("MySQL")
  
  con <- dbConnect(dvr, group  = 'ebi_analysis',...)
  
  #KLUDGE: not all invocations of query.bet.con() make use of the settings file.
  #This effectively limits PEcAn to using ebi_analysis at certain places.
  #What follows is a quick fix - it relies on settings as a global variable,
  #which are generally recommended against
  if(exists('settings')){
    con <- dbConnect(dvr, group  = settings$database$name,
        dbname=settings$database$name, 
        password=settings$database$passwd, 
        username=settings$database$userid, 
        host=settings$database$host)
    return(con)
  }
  
  return(con)
}

##' Kill existing database connections
##'
##' resolves (provides workaround to) bug #769 caused by having too many open connections \code{Error during wrapup: RS-DBI driver: (cannot allocate a new connection -- maximum of 16 connections already opened)}
##' @title Kill existing database connections 
##' @return nothing, as a side effect closes all open connections
##' @author David LeBauer
killdbcons <- function(){
  for (i in dbListConnections(MySQL())) dbDisconnect(i)
}
