query.bety.priors <- function(pft){
    con <- query.bety.con()
## prior traits will be used in meta-analysis for traits w/ data
## or to query priors for traits w/o data
  trstr <- "'mort2','cuticular_cond','dark_respiration_factor','plant_min_temp','growth_resp_factor','leaf_turnover_rate','leaf_width','nonlocal_dispersal','q','root_respiration_factor','root_turnover_rate','seedling_mortality','SLA','stomatal_slope','Vm_low_temp','quantum_efficiency','f_labile','leafN','water_conductance','Vm0','r_fract','storage_turnover_rate'"
  query <- paste("SELECT Priors.PriorID, PriorPFT, VarID, PriorDistn, PriorParamA, PriorParamB, PriorN from Priors, Priors_has_PFT where Priors.PriorID = Priors_has_PFT.PriorID and Priors_has_PFT.pftID in ('", pft, "') and VarID in (", trstr, ");", sep = "")
  print(query)
  q    <- dbSendQuery(con, query)
  priors <- NA
  priors <- fetch ( q, n = -1 )
  rownames(priors) <- priors$VarID
  return(priors)
}
