query.bety.priors <- function(pft){
  con <- query.bety.con()
  ## prior traits will be used in meta-analysis for traits w/ data
  ## or to query priors for traits w/o data

  ## trstr is a list of the traits that ED can use
  trstr <- "'mort2','cuticular_cond','dark_respiration_factor','plant_min_temp','growth_resp_factor','leaf_turnover_rate','leaf_width','nonlocal_dispersal','q','root_respiration_factor','root_turnover_rate','seedling_mortality','SLA','stomatal_slope','Vm_low_temp','quantum_efficiency','f_labile','leafN','water_conductance','Vm0','r_fract','storage_turnover_rate'"
  
  ## query 1: query the prior_id s assoc. with pft
  query1 <- paste("select pfts_priors.prior_id from pfts_priors where pfts_priors.pft_id in (select pfts.id from pfts where pfts.name in ('",pft,"'));", sep = "")
  q1    <- dbSendQuery(con, query1)
  prior.id <- fetch(q1, n = -1 )$prior_id
  pr.id.str <- vecpaste(prior.id)

  ## query 2: query the variable names assoc. with priors.
  query2 <- paste("select distinct variables.name, distn, parama, paramb, n from priors join variables on priors.variable_id = variables.id where priors.id in (",pr.id.str,") and variables.name in (",trstr,");", sep = "")
  q2 <- dbSendQuery(con, query2)
  priors <- fetch ( q2, n = -1 )
  rownames(priors) <- priors$name
  priors <- priors[,-1]
  return(priors)
}
