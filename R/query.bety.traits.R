##* indicates lines that need to be uncommented after Vcmax query is corrected
query.bety.traits <- function(spstr, trvec){
  con <- query.bety.con()
  trait.data <- list()
  for (i.tr in trvec) {
    ##* if(i.tr != 'Vcmax'){
    query <- paste("select traits.site_id, treatments.name, traits.mean, traits.statname, traits.stat, traits.n, treatments.control from traits left join treatments on (traits.treatment_id = treatments.id) where specie_id in (", spstr,") and variable_id in ( select id from variables where name = '",i.tr,"');", sep = "")
    query.result <- dbSendQuery(con, query)
    result <- fetch(query.result, n = -1)
    ##* } elseif(i.tr == 'Vcmax') {
    ##*    qd <- paste("SELECT SiteID, TrtID, TraitMean, TraitStatName, TraitStat, TraitN, tdhc1.TraitCovLevel AS 'temp', tdhc2.TraitCovLevel AS 'canopy_layer' FROM TraitData LEFT JOIN TraitData_has_Covariate AS tdhc1 USING (TraitDataID) LEFT JOIN TraitData_has_Covariate AS tdhc2 USING (TraitDataID) WHERE tdhc1.TraitCovID = 'temp' and tdhc2.TraitCovID = 'canopy_layer' and (tdhc2.TraitCovLevel >= .8 or tdhc2.TraitCovLevel IS NULL) and tdhc2.TraitCovLevel IS NOT NULL and MONTH(TraitDate) between 4 and 7 and USDASymbol in (", spstr, ") and TraitVarID = 'Vcmax'; ", sep = "")
    ##*q    <- dbSendQuery(con, qd)
    ##*data <- NA
    ##*data <- fetch ( q, n = -1 )
    ##*data$mean <- data$mean * exp (3000 * ( 1 / 288.15 - 1 / (273.15 + data$temp)))
    ##*.l <- c("SE", "SD")
    ##*data$stat[data$statname %in% .l] <- data$stat * exp (3000 * ( 1 / 288.15 - 1 / (273.15 + data$temp)))
    ##*.s <- "MSE"
    ##*data$stat[!data$statname %in% .s] <- data$stat * exp (3000 * ( 1 / 288.15 - 1 / (273.15 + data$temp)))^2
    ##*result <- data[,1:6] #drop covariates
    ##* }

    ## labeling control treatments based on treatments.control flag
    result$name[which(result$control == 1 | is.na(result$name))] <- 0
    data <- result[,-which(colnames(result)=='control')]

    ## assign a unique sequential integer to site and trt
    ## first convert any NA's to 0, (all site_id's in database are >0)
    if(sum(is.na(data$site_id)>0)) data$site_id[is.na(data$site_id)] <- 0
    data <- transform(data,
                      site = as.integer(factor(site_id, unique(site_id))),
                      trt = as.integer(factor(name, unique(name)))
                      )

    for (i.site in unique(data$site)){
      data$trt[data$site == i.site] <- as.numeric(factor(data$trt[data$site == i.site]))-1
    }

    ## Transformation of stats to SE

    ## transform SD to SE
    sdi <- which(data$statname == "SD")
    data$stat[sdi] <- data$stat[sdi] / sqrt(data$n[sdi])
    data$statname[sdi] <- "SE"

    ## transform MSE to SE
    msei <- which (data$statname == "MSE")
    data$stat[msei] <- sqrt (data$stat[msei])
    data$statname[msei] <- "SE"

    ## need to deal with LSD
    lsdi <- which(data$statname == "LSD")
    if (sum(lsdi) > 0 ) data[lsdi,c(4,5)] <- NA
    
    ## make sure there is at least one measure of variance
    is.var <- sum(as.numeric(is.na(data$stat)))
    if(is.var == 0) log[['variance']] <- paste("no measure of variance for",name)
    ## check to make sure that all stats have been transformed to SE 
    statnames <- unique(data$statname[!is.na(data$statname)])
    if ( length(statnames) == 0) statnames <- "SE" 



    
    if (dim(result)[1] > 0) trait.data[[i.tr]] <- result
  }
  return(trait.data)
}
