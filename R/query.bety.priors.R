query.bety.priors <- function(pft, prstr){
## prior traits will be used in meta-analysis for traits w/ data
## or to query priors for traits w/o data
  dvr <- dbDriver ("MySQL")
  con <- dbConnect(dvr, group  = "biofuel" )
  query <- paste("SELECT Priors.PriorID, PriorPFT, VarID, PriorDistn, PriorParamA, PriorParamB, PriorN from Priors, Priors_has_PFT where Priors.PriorID = Priors_has_PFT.PriorID and Priors_has_PFT.pftID in ('", pft, "') and VarID in (", prstr, ");", sep = "")
  q    <- dbSendQuery(con, query)
  priors <- NA
  priors <- fetch ( q, n = -1 )
  rownames(priors) <- priors$VarID
  return(priors)
}
