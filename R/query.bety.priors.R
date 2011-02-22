query.bety.priors <- function(pft, trstr,out=NULL,con=NULL,...){

  if(is.null(con)){
    con <- query.bety.con(...)
  }
  if(is.list(con)){
    print("query.bety.priors")
    print("WEB QUERY OF DATABASE NOTE IMPLEMENTED")
    return(NULL)
  }
  
  ## prior traits will be used in meta-analysis for traits w/ data
  ## or to query priors for traits w/o data
  
  ## query 1: query the prior_id s assoc. with pft
  query1 <- paste("select pfts_priors.prior_id from pfts_priors where pfts_priors.pft_id in (select pfts.id from pfts where pfts.name in ('",pft,"'));", sep = "")
  q1    <- dbSendQuery(con, query1)
  prior.id <- fetch(q1, n = -1 )$prior_id
  pr.id.str <- vecpaste(prior.id)
  if(is.null(prior.id)){
    print("**** NO PRIORS FOUND ****")
    return(prior.id)
  }
  
  ## query 2: query the variable names assoc. with priors.
  query2 <- paste("select distinct variables.name, distn, parama, paramb, n from priors join variables on priors.variable_id = variables.id where priors.id in (",pr.id.str,") and variables.name in (",trstr,");", sep = "")
  q2 <- dbSendQuery(con, query2)
  priors <- fetch ( q2, n = -1 )
  priors$name[priors$name == 'SLA_gC_per_m2'] <- 'SLA'
  rownames(priors) <- priors$name
  priors <- priors[, -which(colnames(priors)=='name')]
  if(priors['leaf_width', 'distn']=='lnorm') {
    priors['leaf_width','parama'] <- priors['leaf_width','parama'] - log(1000)
<<<<<<< TREE
  } elseif(priors['leaf_width', 'distn']=='unif') {
    priors['leaf_width','parama'] <- priors['leaf_width','parama'] / 1000
    priors['leaf_width','paramb'] <- priors['leaf_width','paramb'] / 1000
  } else {
=======
  } else if(priors['leaf_width', 'distn']=='unif'){
    priors['leaf_width','parama'] <- priors['leaf_width','parama']/1000
    priors['leaf_width','paramb'] <- priors['leaf_width','paramb']/1000
  }
  else 
  {
>>>>>>> MERGE-SOURCE
    stop (paste('leaf_width prior not transformed \n
           change prior distribution on leaf_width to \n
           lognormal or uniform distribution \n
           or add transformation of ', priors['leaf_width', 'distn'],
                ' distribution to query.bety.priors.R',
                sep = '' ))
  }
  if(!is.null(out)){
    sink(file = paste(out,'/priors.tex',sep=""), split = FALSE)
    xtable(priors, caption="Raw table of priors")
    sink()
  }
  return(priors)
}
