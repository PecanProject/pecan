##* indicates lines that need to be uncommented after Vcmax query is corrected
query.bety.trait.data <- function(trait, spstr){
  con <- query.bety.con()
  if(trait != 'Vcmax') {
    query <- paste("select traits.site_id, treatments.name, treatments.control, sites.greenhouse, traits.mean, traits.statname, traits.stat, traits.n from traits left join treatments on  (traits.treatment_id = treatments.id) left join sites on (traits.site_id = sites.id) where specie_id in (", spstr,") and variable_id in ( select id from variables where name = '", trait,"');", sep = "")
    query.result <- dbSendQuery(con, query)
    result <- fetch(query.result, n = -1)
  } elseif(trait == 'Vcmax') {
    query <- paste("select trt.site_id, treat.name, trt.mean, trt.statname, trt.stat, trt.n, tdhc1.level as 'temp', tdhc2.level as 'canopy_layer' from traits as trt left join covariates as tdhc1 on (tdhc1.trait_id = trt.id) left join covariates as tdhc2 on (tdhc2.trait_id = trt.id) left join treatments as treat on (trt.treatment_id = treat.id) left join variables as tdhc1_var on (tdhc1.variable_id = tdhc1_var.id) left join variables as tdhc2_var on ( tdhc2.variable_id = tdhc2_var.id ) left join species as spec on (trt.specie_id = spec.id) left join plants on (spec.plant_id = plants.id) left join variables as var on (var.id = trt.variable_id) where tdhc1_var.name = 'leafT' and ( ( tdhc2_var.name = 'canopy_layer' and tdhc2.level >= .8 )  or tdhc2.level is null)  and (month(trt.date) between 4 and 7 or trt.date is null) and species_id in(", spstr, ");")


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

  ##if no control trt, set median value to control
  if(!1 %in% result$control){
    result$control[which.min((mean(result$mean)-result$mean)^2)] <- 1
  }
  ## labeling control treatments based on treatments.control flag
  result$trt_id[which(result$control == 1)] <- 'control'
  
  ## assign all unknown sites to 0
  result$site_id[is.na(result$site_id)] <- 0
  ## by default, assume obs. are from difft treatments
  .u <- c(letters,LETTERS,-seq(52:sum(is.na(result$control))))
  result$trt_id[is.na(result$control)] <- .u[1:sum(is.na(result$control))]

  ## remove control flag
  result <- result[,-which(names(result) == 'control')]
  ## assume not in greenhouse when is.na(greenhouse)
  result$greenhouse[is.na(result$greenhouse)] <- 0

  

  ## assign a unique sequential integer to site and trt; for trt, all controls == 0
  data <- transform(result,
                    stat = as.numeric(stat),
                    n    = as.numeric(n),
                    site = as.integer(factor(site_id, unique(site_id))),
                    trt = as.integer(factor(trt_id, unique(c('control', as.character(trt_id))))),
                    Y = mean
                    )
  data$n[is.na(data$n)] <- 1
  data$ghs <- data$greenhouse #jags won't recognize 0 as an index
  
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
  if (FALSE %in% c('SE','none') %in% data$statname) {
    print(paste(trait, ': ERROR!!! data contains untransformed statistics'))
  }
  
  names(data)[names(data)=='stat'] <- 'se'
  data$stdev <- sqrt(data$n) * data$se
  data$obs.prec <- 1 / sqrt(data$stdev)
  ma.data <- data[, c('mean', 'n', 'site', 'trt', 'greenhouse', 'obs.prec')]
  names(ma.data) <- c('Y', 'n', 'site', 'trt', 'ghs', 'obs.prec')
  return(ma.data)
}
