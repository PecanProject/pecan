query.bety.priors <- function(pft, trstr, out=NULL,con=NULL,...){
  if(is.null(con)){
    con <- query.bety.con(...)
  }
  if(is.list(con)){
    print("query.bety.priors")
    print("WEB QUERY OF DATABASE NOT IMPLEMENTED")
    return(NULL)
  }
    
  query.text <- paste("select variables.name, distn, parama, paramb, n",
                  "from priors",
                  "join variables on priors.variable_id = variables.id",
                  "join pfts_priors on pfts_priors.prior_id = priors.id",
                  "join pfts on pfts.id = pfts_priors.pft_id",
                  "where pfts.name in (", vecpaste(pft), ")",
                  "and variables.name in (", trstr, ");")
  query    <- dbSendQuery(con, query.text)
  priors <- fetch ( query, n = -1 )
  
  #HACK: this rename does not belong here. 
  #There should be a separate function for this called after query.bety.priors. 
  #-carl
  priors$name[priors$name == 'SLA_m2_per_gC'] <- 'SLA'

  if(nrow(priors) <= 0){
    warning(paste("No priors found for pft(s): ", pft))
    priors <- priors[, which(colnames(priors)!='name')]
    return(priors)
  }
  else {    
    rownames(priors) <- priors$name
    priors <- priors[, which(colnames(priors)!='name')]
    return(priors)
  }

#  if(!is.null(out)){
#    sink(file = paste(out,'priors.tex',sep=""), split = FALSE)
#    xtable(priors, caption="Raw table of priors")
#    sink()
#  }
  
}
