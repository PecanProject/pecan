##' This is a generic function that sets a connection to BETYdb
##'
##' \details{
##'   This function requires a file in $HOME/.my.cnf that allows user to automatically log in to BETYdb. This file is used to specify the database name, <username>, and <password>. The contents of the file should be:
##' \code{
##'    [ebi_analysis]
##'    user = <username>
##'    password = <password>
##'    host = ebi-forecast.igb.uiuc.edu
##'    database = ebi_analysis
##'  }
##'  or, if database is on localhost:
##'  \code{
##'    [ebi_analysis]
##'    user = <username>
##'    password = <password>
##'    host = localhost
##'    database = ebi_analysis
##'  }
##' @title Setup connection to BETYdb
##' @param ... 
##' @return con, database connection 
query.bety.con <- function(...){
  lapply(dbListConnections(MySQL()), dbDisconnect) #first kill all connections
  dvr <- dbDriver ("MySQL")
  con <- dbConnect(dvr, group  = 'ebi_analysis',...)
  return(con)
}
##' @examples
##' ## query all SLA data from database
##' con <- query.bety.con()
##' query <- "select mean from traits where variable_id in (select id from variables where name like 'SLA');"
##' sla.data <- fetch(dbSendQuery(con, query), n=-1) 
##' hist(sla.data)

