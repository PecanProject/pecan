query.bety.con <- function(){
  lapply(dbListConnections(MySQL()), dbDisconnect) #first kill all connections
  dvr <- dbDriver ("MySQL")
  con <- dbConnect(dvr, group  = 'ebi_analysis' )
  return(con)
}
