query.bety.parameters <- function(pft, edtraits, dvr, con,...){
  dvr <- dbDriver ("MySQL")
  con <- dbConnect(dvr, group  = "biofuel",...)
  query <- paste("select VarID from Priors where Priors.PriorID in (select Priors_has_PFT.PriorID from Priors_has_PFT where Priors_has_PFT.PFTID = '",pft,"' and VarID in (", vecpaste(edtraits),"));", sep = '')
  #query <- paste('select distinct variable_id from traits where traits.specie_id in (select specie_id from pfts_species where pft_id = (select pfts.id from pfts where pfts.name = ',pft,'));')
  query.result <- dbSendQuery(con, query)
  parms <- fetch(query.result, n = -1)[,1]
  return(parms)
}



