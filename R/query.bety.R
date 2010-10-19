query.bety <- function(query){
  con <- query.bety.con()
  q  <- dbSendQuery(con, query)
  data <- fetch(q, n=-1)
  return(data)
}
