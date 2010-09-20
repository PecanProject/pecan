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
    result$name[which(result$control == 1 | is.na(result$name))] <- 'control'
    data <- result[,-which(colnames(result)=='control')]

    ## assign a unique sequential integer to site and trt
    data$site_id[which(is.na(data$site_id))] <- 0
    

    if(length(unique(data$site_id)==1)){
      data$site <- 1
    } else {
      data$site <- 0
      for (i.site in unique(data$site_id)){
        data$site[data$site_id == i.site] <- max(data$site, na.rm=TRUE) +1
      }
    }
 
    data$trt <- 1
    for (ii in site.seq) {
      data$trt[which(is.na(data$TrtID))] <- 0
      ##temp set unidentified treatments to 0, change at end
      l.a <- length(unique(data$TrtID[data$site == ii]))
      if (l.a >= 2) {
        trt.i <- unique(data$TrtID[data$site == ii])
        trt.seq <- seq(trt.i)
        for (jj in trt.seq) {
          if (jj == 1) {
            data$trt[data$TrtID == trt.i[jj]] <- 1
          } else {
            data$trt[data$TrtID == trt.i[jj]]  <- max(data$trt) + 1
          }
        }
      }
    }
    data$trt[data$trt == 0] <- max(data$trt) + 1


    
    if (dim(result)[1] > 0) trait.data[[i.tr]] <- result
  }
  return(trait.data)
}
