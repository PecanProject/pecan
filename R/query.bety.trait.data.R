#' Extract trait data from BETYdb
#' @name query.bety.trait.data
#'
#' query.bety.trait.data extracts data from BETYdb for a given trait and species, converts all statistics to 
#' summary statistics, and prepares a dataframe for use in meta-analyiss.
#' For Vcmax and SLA data, only data collected between  April and July are queried, and only data collected from
#' the top of the canopy (canopy height > 0.8). For Vcmax and root_respiration_factor, data are scaled
#' converted from measurement temperature to \eqn{15^oC} (ED default) via the arrhenius equation.
#'
#' @param trait is the trait name used in BETY, stored in variables.name
#' @param spstr is the species.id integer or string of integers associated with the species
#'
#' @return dataframe ready for use in meta-analysis

##* indicates lines that need to be uncommented after Vcmax query is corrected

fetch.transformed <- function(connection, query){
  query.result <- dbSendQuery(connection, query)
  result <- pecan.transformstats(fetch(query.result, n = -1))
  return(result)
}
arrhenius.scaling <- function(mean, temp){
  return(mean / exp (3000 * ( 1 / 288.15 - 1 / (273.15 + temp))))
}

query.bety.trait.data <- function(trait, spstr,con=NULL,...){
  
  if(is.null(con)){
    con <- query.bety.con(...)
  }
 
  if(is.list(con)){
    print("query.bety.trait.data")
    print("WEB QUERY OF DATABASE NOTE IMPLEMENTED")
    return(NULL)
  } 
   
  if(trait == 'root_respiration_factor') trait <- 'root_respiration_rate'
  if(trait == 'Vm0') trait <- 'Vcmax'

  if(trait == 'Vcmax') {
    #########################   VCMAX   ############################
    query <- paste("select traits.id, traits.citation_id, traits.site_id, treatments.name, traits.date, traits.dateloc, treatments.control, sites.greenhouse, traits.mean, traits.statname, traits.stat, traits.n from traits left join treatments on  (traits.treatment_id = treatments.id) left join sites on (traits.site_id = sites.id) where specie_id in (", spstr,") and variable_id in ( select id from variables where name = '", trait,"');", sep = "")
    data <- fetch.transformed(con, query)

    ## grab covariate data
    q = dbSendQuery(con,paste("select covariates.trait_id, covariates.level,variables.name from covariates left join variables on variables.id = covariates.variable_id where trait_id in (",vecpaste(data$id),")",sep=""))
    covs = fetch(q,n=-1)
    leafT = rep(25,nrow(data))
    canopy_layer = rep(1.0,nrow(data))
    data = cbind(data,leafT,canopy_layer)
    for(j in which(covs$name %in% c("leafT","canopy_layer"))){
      data[match(covs$trait_id[j],data$id),covs$name[j]] = covs$level[j]
    }
    ## select sunleaf data
    data = data[data$canopy_layer >= 0.66,]
    
    data$mean <- arrhenius.scaling(data$mean, data$leafT)
    data$stat <- arrhenius.scaling(data$stat, data$leafT)
    result <- data[,which(!colnames(data) %in% c('leafT', 'canopy_layer','date','dateloc'))] #drop covariates

  } else if (trait == 'SLA') {

    
    #########################    SLA    ############################
    query <- paste("select trt.id, trt.citation_id, trt.site_id, treat.name, treat.control, sites.greenhouse, trt.mean, trt.statname, trt.stat, trt.n from traits as trt left join treatments as treat on (trt.treatment_id = treat.id)  left join sites on (sites.id = trt.site_id) where trt.variable_id in (select id from variables where name = 'SLA')  and specie_id in (",spstr,");", sep = "")
    data <- fetch.transformed(con, query)

    ## convert LMA to SLA
    selLMA = which(data$vname == "LMA")
    if(length(selLMA)>0){
      for(i in selLMA){
        if(is.na(data$stat[i])){
          data$mean[i] = 1/data$mean[i]
        } else {
          x = 1/rnorm(100000,data$mean[i],data$stat[i])
          data$mean[i] = mean(x)
          data$stat[i] = sd(x)
        }
      }
    }
    
    ## grab covariate data
    q = dbSendQuery(con,paste("select covariates.trait_id, covariates.level,variables.name from covariates left join variables on variables.id = covariates.variable_id where trait_id in (",vecpaste(data$id),")",sep=""))
    covs = fetch(q,n=-1)
    canopy_layer = rep(1.0,nrow(data))
    data = cbind(data,canopy_layer)
    for(j in which(covs$name %in% c("canopy_layer"))){
      data[match(covs$trait_id[j],data$id),covs$name[j]] = covs$level[j]
    }
    ## select sunleaf data
    data = data[data$canopy_layer >= 0.66,]    
    result <- data[,which(colnames(data)!='canopy_layer')]

    #convert from kg leaf / m2 to kg C / m2
    result[, c('mean','stat')] <- result[, c('mean','stat')] / 0.48 

  } else if (trait == 'leaf_turnover_rate'){

        
    #########################    LEAF TURNOVER    ############################
    query <- paste("select trt.id, trt.citation_id, variables.name as vname, trt.site_id, treat.name, treat.control, sites.greenhouse, trt.mean, trt.statname, trt.stat, trt.n from traits as trt left join treatments as treat on (trt.treatment_id = treat.id)  left join sites on (sites.id = trt.site_id) left join variables on (variables.id = trt.variable_id) where variables.name in ('leaf_turnover_rate','Leaf Longevity') and specie_id in (",spstr,");", sep = "")
    q    <- dbSendQuery(con, query)
    data <-  pecan.transformstats(fetch ( q, n = -1 ))

    ## convert LL to turnover
    selLL = which(data$vname == "Leaf Longevity")
    if(length(selLL)>0){
      for(i in selLL){
        if(is.na(data$stat[i])){
          data$mean[i] = 1/data$mean[i]
        } else {
          x = 1/rnorm(100000,data$mean[i],data$stat[i])
          data$mean[i] = mean(x)
          data$stat[i] = sd(x)
        }
      }
    }

    result <- data
    
  } else if (trait == 'root_respiration_rate') {

    #########################  ROOT RESPIRATION   ############################
    query <- paste("select trt.id, trt.citation_id, trt.site_id, treat.name, treat.control, sites.greenhouse, trt.mean, trt.statname, trt.stat, trt.n, tdhc1.level as 'temp' from traits as trt left join covariates as tdhc1 on (tdhc1.trait_id = trt.id)  left join treatments as treat on (trt.treatment_id = treat.id) left join variables as tdhc1_var on (tdhc1.variable_id = tdhc1_var.id) left join sites on (sites.id = trt.site_id)  left join species as spec on (trt.specie_id = spec.id) left join plants on (spec.plant_id = plants.id) left join variables as var on (var.id = trt.variable_id) where trt.variable_id in (select id from variables where name = 'root_respiration_rate') and specie_id in (",spstr,") and tdhc1_var.name = 'rootT';", sep = '') 
    data <- fetch.transformed(con, query)
    
    ## Scale to 15C using Arrhenius scaling,
    ## Convert root_respiration_rate to root_respiration_factor (i.e. maintenance respiration)
    ## assuming a 1:1 partitioning of growth:maintenance respiration
    data$mean <- arrhenius.scaling(data$mean, data$temp)/2
    data$stat <- arrhenius.scaling(data$stat, data$temp)/2
    result <- data[,which(!colnames(data) %in% c('temp'))] #drop covariates
    
  } else if (trait == 'c2n_leaf') {

    #########################  LEAF C:N   ############################

    query <- paste("select traits.id, traits.citation_id, variables.name as vname, traits.site_id, treatments.name, treatments.control, sites.greenhouse, traits.mean, traits.statname, traits.stat, traits.n from traits left join treatments on  (traits.treatment_id = treatments.id) left join sites on (traits.site_id = sites.id) left join variables on (traits.variable_id = variables.id) where specie_id in (", spstr,")  and variables.name in ('c2n_leaf', 'leafN');", sep = "")

    data <- fetch.transformed(con, query)
    leafNdata   <- data$name == 'leafN'
    leafNdataSE <- leafNdata & data$statname == 'SE'
    inv.se <- function(mean, stat, n) signif(sd(48/rnorm(100000, mean, stat*sqrt(n)))/sqrt(n),3)
    data$stat[leafNdataSE] <- apply(data[leafNdataSE, c('mean', 'stat', 'n')],1, function(x) inv.se(x[1],x[2],x[3]) )
    data$mean[data$vname == 'leafN'] <- 48/data$mean[data$vname == 'leafN']
    result <- data
  } else if (trait == 'q') {

    #########################  FINE ROOT ALLOCATION  ############################
    ## query Q or FRC_RC
    query <- paste("select traits.citation_id, traits.id, variables.name as vname, traits.site_id, treatments.name, treatments.control, sites.greenhouse, traits.mean, traits.statname, traits.stat, traits.n from traits left join treatments on  (traits.treatment_id = treatments.id) left join sites on (traits.site_id = sites.id) left join variables on (traits.variable_id = variables.id) where specie_id in (", spstr,")  and variables.name in ('q', 'FRC_RC');", sep = "")
    data <- fetch.transformed(con, query)

    ## query fine root biomass and leaf biomass
    query <- paste("select traits.citation_id, traits.id, variables.name as vname, traits.site_id, treatments.name, treatments.control, sites.greenhouse, traits.mean, traits.statname, traits.stat, traits.n, traits.specie_id from traits left join treatments on  (traits.treatment_id = treatments.id) left join sites on (traits.site_id = sites.id) left join variables on (traits.variable_id = variables.id) where specie_id in (", spstr,")  and variables.name in ('fine_root_biomass','leaf_biomass');", sep = "")
    data2 <- fetch.transformed(con, query)

    ## match above and below ground biomass
    ## match on citation_id, site_id, treatment_id, specie_id where different variables.name
    data3 = NULL
    if(nrow(data2) > 0){
#      pair = list(); counter = 0;
      for(cite in unique(data2$citation_id)){
        selC = which(data2$citation_id == cite)
        for(site in unique(data2$site_id[selC])){
          selS = selC[which(data2$site_id[selC] == site)]
          for(spp in unique(data2$specie_id[selS])){
            selSp = selS[which(data2$specie_id[selS] == spp)]
            for(tmt in unique(data2$name[selSp])){
              selT = selSp[which(data2$name[selSp] == tmt)]

              #ok, after all this you have a unique data -- does it constitute a root/leaf pair?
              roots  = selT[which(data2$vname[selT] == "fine_root_biomass")]
              leaves = selT[which(data2$vname[selT] == "leaf_biomass")]

              if(length(roots) == 1 & length(leaves) == 1){
                newrow = data2[roots,]; newrow$vname = 'q'
                if(is.na(data2$stat[leaves])){
                  newrow$mean = newrow$mean/data2$mean[leaves]
                  newrow$stat = newrow$stat/data2$mean[leaves]
                } else {
                  ## approximate by numerical simulation
                  if(is.na(newrow$stat)) newrow$stat = 0
                  x = rnorm(10000,newrow$mean,newrow$stat)/rnorm(10000,data2$mean[leaves],data2$stat[leaves])
                  newrow$mean = mean(x)
                  newrow$stat = sd(x)
                }
                newrow$n    = min(newrow$n,data2$n[leaves])
                if(is.null(data3)){ data3 = newrow} else {data3 = rbind(data3,newrow)}
                
              }
            }
          }
        }        
      }
      if(!is.null(data3)) data3 <-  data3[,which(colnames(data3)!="specie_id")]
    }
    
    result <- rbind(data,data3)
  }  else {
    #########################  GENERIC CASE  ############################
        query <- paste("select traits.id, traits.citation_id, traits.site_id, treatments.name, treatments.control, sites.greenhouse, traits.mean, traits.statname, traits.stat, traits.n from traits left join treatments on  (traits.treatment_id = treatments.id) left join sites on (traits.site_id = sites.id) where specie_id in (", spstr,") and variable_id in ( select id from variables where name = '", trait,"');", sep = "")
    result <- fetch.transformed(con, query)
  }

  if (trait == 'leaf_width') result <- transform(result, mean = mean/1000, stat=stat/1000) 

  ## if result is empty, stop run
  if(!exists('result')|nrow(result)==0) stop(paste('no data in database for', trait))
  
  ## rename name column from treatment table to trt_id
  names(result)[names(result)=='name'] <- 'trt_id'
  
  ## labeling control treatments based on treatments.control flag
  result$control[is.na(result$control)]     <- 1
  result$trt_id[which(result$control == 1)] <- 'control'

  error <-  function(site.i, result) paste('No control treatment set for site_id:',
                                       unique(result$site_id[site.i]),
                                       'and citation id',
                                       unique(result$citation_id[site.i]),
                                       '\nplease set control treatment for this site / citation in database\n')
  
  ## assign all unknown sites to 0
  ## TODO different site for each citation - dsl
  result$site_id[is.na(result$site_id)] <- 0

  ## assume not in greenhouse when is.na(greenhouse)
  result$greenhouse[is.na(result$greenhouse)] <- 0
  
  result$n[is.na(result$n)] <- 1
  result$n[!is.na(result$stat)] <- 2

  sites <- unique(result$site_id)
  for(ss in sites){
    site.i <- result$site == ss
    ##if only one treatment, it's control
    if(length(unique(result$trt[site.i])) == 1) result$trt_id[site.i] <- 'control'
    ##if that didn't solve the problem, then stop
    if(!'control' %in% result$trt_id[site.i]){
      stop(error(site.i, result))
    }
  }
  
  ## assign a unique sequential integer to site and trt; for trt, all controls == 0
  data <- subset(transform(result,
                    stat = as.numeric(stat),
                    n    = as.numeric(n),
                    site_id = as.integer(factor(site_id, unique(site_id))),
                    trt_id = as.integer(factor(trt_id, unique(c('control', as.character(trt_id))))),
                    mean = mean,
                    citation_id = citation_id
                    ), select = c('stat', 'n', 'site_id', 'trt_id', 'mean', 'citation_id', 'greenhouse')) 
    

  ##TODO are following assumptions okay? These seem to get commented out in revisions. Why? -dsl
  ##TODO: Looks good to me --MCD
  data$n[is.na(data$n)] <- 1 # if n=NA, n=1
  data$n[!is.na(data$stat)] <- 2 # if there is a statistic, assume n >= 2
  if(length(data$stat[data$stat <= 0.0]) > 0) {
    warning(paste('there are implausible values of SE, SE <= 0 in the data and these are set to NA from citation', unique(data$citation_id[which(data$stat >= 0.0)], ' ')))
    data$stat[data$stat <= 0.0] <- NA
  }

  ma.data <- rename.cols.forjags(data)

  return(ma.data)
}

rename.cols.forjags <- function(data) {
  data2 <-  transform(data,
                      Y        = mean,
                      se       = stat,
                      obs.prec = 1 / (sqrt(n) * stat) ^2,
                      trt      = trt_id,
                      site     = site_id,
                      cite     = citation_id,
                      ghs      = greenhouse + 1
                      )
  data3 <- subset (data2 ,  select = c('Y', 'n', 'site', 'trt', 'ghs', 'obs.prec', 'se', 'cite'))
  return(data3)
}
