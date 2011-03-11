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
#' @author David LeBauer \email{dlebauer@illinois.edu}

##* indicates lines that need to be uncommented after Vcmax query is corrected

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

  if(!trait %in% c('Vcmax','SLA','root_respiration_rate', 'c2n_leaf', 'q','leaf_turnover_rate') ) {

    #########################  GENERIC CASE  ############################
        query <- paste("select traits.id, traits.citation_id, traits.site_id, treatments.name, treatments.control, sites.greenhouse, traits.mean, traits.statname, traits.stat, traits.n from traits left join treatments on  (traits.treatment_id = treatments.id) left join sites on (traits.site_id = sites.id) where specie_id in (", spstr,") and variable_id in ( select id from variables where name = '", trait,"');", sep = "")
        
    query.result <- dbSendQuery(con, query)
    result <- pecan.transformstats(fetch(query.result, n = -1))

  } else if(trait == 'Vcmax') {

    #########################   VCMAX   ############################
    query <- paste("select traits.id, traits.citation_id, traits.site_id, treatments.name, traits.date, traits.dateloc, treatments.control, sites.greenhouse, traits.mean, traits.statname, traits.stat, traits.n from traits left join treatments on  (traits.treatment_id = treatments.id) left join sites on (traits.site_id = sites.id) where specie_id in (", spstr,") and variable_id in ( select id from variables where name = '", trait,"');", sep = "")

    q    <- dbSendQuery(con, query)
    data <- fetch ( q, n = -1 )
    data <- pecan.transformstats(data)

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
    
    data$mean <- data$mean / exp (3000 * ( 1 / 288.15 - 1 / (273.15 + data$leafT)))
    data$stat <- data$stat / exp (3000 * ( 1 / 288.15 - 1 / (273.15 + data$leafT)))
    result <- data[,-which(colnames(data) %in% c('leafT', 'canopy_layer','date','dateloc'))] #drop covariates

  } else if (trait == 'SLA') {
    
    #########################    SLA    ############################
    query <- paste("select trt.id, trt.citation_id, variables.name as vname, trt.site_id, treat.name, treat.control, sites.greenhouse, trt.mean, trt.statname, trt.stat, trt.n from traits as trt left join treatments as treat on (trt.treatment_id = treat.id)  left join sites on (sites.id = trt.site_id) left join variables on (variables.id = trt.variable_id) where variables.name in ('SLA','LMA') and specie_id in (",spstr,");", sep = "")
    q    <- dbSendQuery(con, query)
    data <-  pecan.transformstats(fetch ( q, n = -1 ))

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
    result <- data[,-which(colnames(data)=='canopy_layer')]
    
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
    q    <- dbSendQuery(con, query)
    data <- pecan.transformstats(fetch ( q, n = -1 ))
    
    ## Scale to 15C using Arrhenius scaling
    data$mean <- data$mean / exp (3000 * ( 1 / 288.15 - 1 / (273.15 + data$temp)))
    data$stat <- data$stat / exp (3000 * ( 1 / 288.15 - 1 / (273.15 + data$temp)))
    ## Convert root_respiration_rate to root_respiration_factor (i.e. maintenance respiration)
    ## assuming a 1:1 partitioning of growth:maintenance respiration
    data$mean <- data$mean/2
    data$stat <- data$stat/2
    result <- data[,-which(colnames(data) %in% c('temp'))] #drop covariates
    
  } else if (trait == 'c2n_leaf') {

    #########################  LEAF C:N   ############################

    query <- paste("select traits.id, traits.citation_id, variables.name as vname, traits.site_id, treatments.name, treatments.control, sites.greenhouse, traits.mean, traits.statname, traits.stat, traits.n from traits left join treatments on  (traits.treatment_id = treatments.id) left join sites on (traits.site_id = sites.id) left join variables on (traits.variable_id = variables.id) where specie_id in (", spstr,")  and variables.name in ('c2n_leaf', 'leafN');", sep = "")

    query.result <- dbSendQuery(con, query)
    data <- pecan.transformstats(fetch(query.result, n = -1))
    leafNdata   <- data$vname == 'leafN'
    leafNdataSE <- leafNdata & data$statname == 'SE'
    inv.se <- function(mean, stat, n) signif(sd(48/rnorm(100000, mean, stat*sqrt(n)))/sqrt(n),3)
    data$stat[leafNdataSE] <- apply(data[leafNdataSE, c('mean', 'stat', 'n')],1, function(x) inv.se(x[1],x[2],x[3]) )
    data$mean[leafNdata] <- 48/data$mean[leafNdata]
    result <- data
  } else if (trait == 'q') {

    #########################  FINE ROOT ALLOCATION  ############################
    ## query Q or FRC_RC
    query <- paste("select traits.citation_id, traits.id, variables.name as vname, traits.site_id, treatments.name, treatments.control, sites.greenhouse, traits.mean, traits.statname, traits.stat, traits.n from traits left join treatments on  (traits.treatment_id = treatments.id) left join sites on (traits.site_id = sites.id) left join variables on (traits.variable_id = variables.id) where specie_id in (", spstr,")  and variables.name in ('q', 'FRC_RC');", sep = "")
    query.result <- dbSendQuery(con, query)
    data <- pecan.transformstats(fetch(query.result, n = -1))

    ## query fine root biomass and leaf biomass
    query <- paste("select traits.citation_id, traits.id, variables.name as vname, traits.site_id, treatments.name, treatments.control, sites.greenhouse, traits.mean, traits.statname, traits.stat, traits.n, traits.specie_id from traits left join treatments on  (traits.treatment_id = treatments.id) left join sites on (traits.site_id = sites.id) left join variables on (traits.variable_id = variables.id) where specie_id in (", spstr,")  and variables.name in ('fine_root_biomass','leaf_biomass');", sep = "")
    query.result <- dbSendQuery(con, query)
    data2 <- pecan.transformstats(fetch(query.result, n = -1))

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
      if(!is.null(data3)) data3 <-  data3[,-which(colnames(data3)=="specie_id")]
    }
    
    result <- rbind(data,data3)
  }

  if (trait == 'leaf_width') result <- transform(result, mean = mean/1000, stat=stat/1000) 
  
  ## rename name column from treatment table to trt_id
  names(result)[names(result)=='name'] <- 'trt_id'
  
  ## labeling control treatments based on treatments.control flag
  result$control[is.na(result$control)]     <- 1
  result$trt_id[which(result$control == 1)] <- 'control'

## Force a control treatment at each site
  for(sitei in unique(result$site_id)) {
    i <- result$site_id == sitei 
    if(is.na(sitei)){
      i = which(is.na(result$site_id))
    }
    if(!1 %in% result$control[i]){
      warning(cat('\nWARNING: no control treatment set for site_id', sitei,
                  '\nif there is only one treatment,',
                  '\nthat treatment is set to control',
                  '\nif there is more than one treatment,',
                  '\nPECAn sets the treatment with mean closest',
                  '\nto the mean of other controls as the control',
                  '\nthis assumption may be FALSE',
                  '\nplease review data from this site\n'),
              eval = print(query.bety(paste("select url, author, year, title
                                       from citations
                                       where id in (select citation_id from
                                       traits
                                       where site_id =",sitei,");"),con=con)))
      control.mean <- ifelse(1 %in% result$control,
                             mean(result$mean[result$control == 1]),
                             mean(result$mean))
      result$control[i & which.min((control.mean - result$mean[i])^2)] <- 1
    }
  }
  
  ## assign all unknown sites to 0
  result$site_id[is.na(result$site_id)] <- 0

  ## assume not in greenhouse when is.na(greenhouse)
  result$greenhouse[is.na(result$greenhouse)] <- 0

  result$n[is.na(result$n) & !is.na(result$stat)] <- 2
  result$n[is.na(result$n)] <- 1


  ## assign a unique sequential integer to site and trt; for trt, all controls == 0
  data <- transform(result,
                    stat = as.numeric(stat),
                    n    = as.numeric(n),
                    site = as.integer(factor(site_id, unique(site_id))),
                    trt = as.integer(factor(trt_id, unique(c('control', as.character(trt_id))))),
                    Y = mean,
                    cite = citation_id
                    )
  
  sites = unique(data$site)
  for(ss in sites){
    #if only one treatment, it's control
    if(length(unique(data$trt[data$site == ss])) == 1) data$trt[data$site == ss] <- 1
#    #make sure at least one control per site
#
#    #this is redundant with what should be done above under the comment "Force a control treatment at each site"
#
  }
#  data$n[is.na(data$n)] <- 1
#  data$n[!is.na(data$stat)] <- 2
  data$ghs <- data$greenhouse #jags won't recognize 0 as an index
        
  names(data)[names(data)=='stat'] <- 'se'
  data$se[data$se <= 0.0] <- NA
  data$stdev <- sqrt(data$n) * data$se
  data$obs.prec <- 1 / data$stdev^2
  ma.data <- data[, c('mean', 'n', 'site', 'trt', 'greenhouse', 'obs.prec', 'se', 'id', 'citation_id')]
  names(ma.data) <- c('Y', 'n', 'site', 'trt', 'ghs', 'obs.prec', 'se', 'trait_id', 'citation_id')
  return(ma.data)
}
