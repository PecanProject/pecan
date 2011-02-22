## returns a table of the priors available for a given variable
query.bety.prior.var <- function(var,con=NULL,...){

  var = var[1] ## only look up one at a time
  
  if(is.null(con)){
    con <- query.bety.con(...)
  }

  ## query 1: query variable name
  query1 <- paste("select * from variables where name = '",var,"';", sep = "")
  q1 <- dbSendQuery(con, query1)
  varID <- fetch ( q1, n = -1 )
  if(nrow(varID) != 1){
    print("VAR not unique")
    print(varID)
    return(NULL)
  }
  
  ## query 2: query priors.
  query2 <- paste("select * from priors where variable_id = (",varID$id,");", sep = "")
  q2 <- dbSendQuery(con, query2)
  priors <- fetch ( q2, n = -1 )
  
}
