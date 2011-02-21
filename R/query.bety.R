query.bety <- function(query,con=NULL,...){
  if(is.null(con)){
    con <- query.bety.con(...)
  }
  q  <- dbSendQuery(con, query)
  data <- fetch(q, n=-1)
  return(data)
}
