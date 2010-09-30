##* indicates lines that need to be uncommented after Vcmax query is corrected
query.bety.traits <- function(spstr, trvec){
  con <- query.bety.con()
  trait.data <- list()

  query <- paste("select distinct variables.name from traits join variables on (traits.variable_id = variables.id) where specie_id in (", spstr,") and variable_id in (select id from variables where name in (", vecpaste(trvec),"));", sep = "")
  ## Need to write query for:
  ## first check pft_species for grass or tree 
  ##      if grass, root and shoot to calculate q
  ##      if tree, fine root and leaf to calculate q
  query.result <- dbSendQuery(con, query)
  traits.in.bety <- fetch(query.result, n = -1)

  for (i.tr in traits.in.bety$name) {

    query <- paste("select traits.site_id, treatments.name, treatments.control, sites.greenhouse, traits.mean, traits.statname, traits.stat, traits.n from traits left join treatments on  (traits.treatment_id = treatments.id) left join sites on (traits.site_id = sites.id) where specie_id in (", spstr,") and variable_id in ( select id from variables where name = '",i.tr,"');", sep = "")
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


    ## rename name column from treatment table to trt_id
    names(result)[names(result)=='name'] <- 'trt_id'
    ## labeling control treatments based on treatments.control flag
    result$trt_id[which(result$control == 1)] <- 'control'
    ## assign all unknown sites to 0
    result$site_id[is.na(result$site_id)] <- 0
    ## by default, assume obs. are from difft treatments
    result$trt_id[is.na(result$control)] <- -seq(1:sum(is.na(result$control)))
    ## remove control flag
    result <- result[,-which(names(result) == 'control')]
    ## assume not in greenhouse when is.na(greenhouse)
    result$greenhouse[is.na(result$greenhouse)] <- 0

    


    ## assign a unique sequential integer to site and trt; for trt, all controls == 0
    data <- transform(result,
                      stat = as.numeric(stat),
                      n    = as.numeric(n),
                      site_id = as.integer(factor(site_id, unique(site_id))),
                      trt_id = as.integer(factor(trt_id, unique(c('control', as.character(trt_id)))))
                      )

    ## Transformation of stats to SE

    ## transform SD to SE
    if ("SD" %in% data$statname) {
      sdi <- which(data$statname == "SD")
      data$stat[sdi] <- data$stat[sdi] / sqrt(data$n[sdi])
      data$statname[sdi] <- "SE"
    }
    ## transform MSE to SE
    if ("MSE" %in% data$statname) {
      msei <- which(data$statname == "MSE")
      data$stat[msei] <- sqrt (data$stat[msei]/data$n[msei])
      data$statname[msei] <- "SE"
    }
    ## 95%CI measured from mean to upper or lower CI
    ## SE = CI/t
    if ("95%CI" %in% data$statname) {
      cii <- which(data$statname == '95%CI')
      data$stat[cii] <- data$stat[cii]/qt(0.975,data$n[cii])
      data$statname[cii] <- "SE"
    }
    ## Fisher's Least Significant Difference (LSD)
    ## conservatively assume no within block replication
    if ("LSD" %in% data$statname) {
      lsdi <- which(data$statname == "LSD")
      data$stat[lsdi] <- data$stat[lsdi] / (qt(0.975,data$n[lsdi]) * sqrt( (2 * data$n[lsdi])))
      data$statname[lsdi] <- "SE"
    }
    ## Tukey's Honestly Significant Difference (HSD),
    ## conservatively assuming 3 groups being tested so df =2
    if ("HSD" %in% data$statname) {
      hsdi <- which(data$statname == "HSD" & data$n > 1)
      data$stat[hsdi] <- data$stat[hsdi] / (qtukey(0.975, data$n[lsdi], df = 2))
      data$statname[hsdi] <- "SE"
    }              
    ## MSD Minimum Squared Difference
    ## MSD = t_{\alpha/2, 2n-2}*SD*sqrt(2/n)
    ## SE  = MSD*n/(t*sqrt(2))
    if ("MSD" %in% data$statname) {
      msdi <- which(data$statname == "MSD")
      data$stat[msdi] <- data$stat[msdi] * data$n[msdi] / ( qt(0.975,2*data$n[lsdi]-2)*sqrt(2))
      data$statname[msdi] <- "SE"
    }
    if (!FALSE %in% c('SE','none') %in% data$statname) {
      print(paste(i.tr, ': all statistics in data set transformed to SE or removed'))
    } else {
      print(paste(i.tr, ': ERROR!!! data contains untransformed statistics'))
    }
     trait.data[[i.tr]] <- data[,-which(names(data)=='statname')]
  }
  ## check if statname other than 'SE', 'none' in trait.data
  return(trait.data)
}
