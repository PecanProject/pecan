query.bety.traits <- function(spstr, trvec){
  con <- query.bety.con()
  trait.data <- list()
  for (i.tr in trvec) {
    if (i.tr != 'Vcmax') {
    query <- paste("select traits.site_id, treatments.name, traits.mean, traits.statname, traits.stat, traits.n from traits left join treatments on (traits.treatment_id = treatments.id) where specie_id in (", spstr,") and variable_id in ( select id from variables where name = '",i.tr,"');", sep = "")
  } else if (i.tr == 'Vcmax') {
    query <- paste("select traits.site_id, treatments.name, traits.mean, traits.statname, traits.stat, traits.n, tdhc1.level AS 'temp', tdhc2.level AS 'canopy_layer' FROM traits LEFT JOIN treatments ON traits.treatment_id = treatment.id JOIN covariates AS tdhc1 ON (traits.id = covariates.trait_id) JOIN covariates AS tdhc2 ON (traits.id = covariates.trait_id) WHERE (SELECT true from variables WHERE tdhc1.name = 'temp' and tdhc2.TraitCovID = 'canopy_layer' and (tdhc2.TraitCovLevel >= .8 or tdhc2.TraitCovLevel IS NULL) and tdhc2.TraitCovLevel IS NOT NULL and MONTH(TraitDate) between 4 and 7 and USDASymbol in (", spstr, ") and TraitVarID = 'Vcmax'; ", sep = "")
  }
    query.result <- dbSendQuery(con, query)
    result <- fetch(query.result, n = -1)
    if (dim(result)[1] > 0) trait.data[[i.tr]] <- result 
  }
  return(trait.data)
}

 e

