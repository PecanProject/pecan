##' Queries data from BETY and transforms statistics to SE
##'
##' Performs query and then uses \code{pecan.transformstats} to convert miscellaneous statistical summaries
##' to SE
##' @title Fetch data and transform stats to SE
##' @param connection connection to BETYdb
##' @param query MySQL query to traits table
##' @return dataframe with trait data
##' @seealso used in \code{\link{query.bety.trait.data}}; \code{\link{pecan.transformstats}} performs transformation calculations
fetch.stats2se <- function(connection, query){
  query.result <- dbSendQuery(connection, query)
  transformed <- pecan.transformstats(fetch(query.result, n = -1))
  return(transformed)
}

##' Scale temperature dependent trait from measurement temperature to reference temperature 
##'
##' .. content for \details{} ..
##' @title 
##' @param observed.value observed value of temperature dependent trait, e.g. Vcmax, root respiration rate
##' @param old.temp temperature at which measurement was taken or previously scaled to
##' @param new.temp temperature to be scaled to, default = 25 C  
##' @return numeric value at reference temperature
arrhenius.scaling <- function(observed.value, old.temp, new.temp = 25){
  return(observed.value / exp (3000 * ( 1 / (273.15 + new.temp) - 1 / (273.15 + old.temp))))
}

rename.jags.columns <- function(data) {
  transformed <-  transform(data,
                      Y        = mean,
                      se       = stat,
                      obs.prec = 1 / (sqrt(n) * stat) ^2,
                      trt      = trt_id,
                      site     = site_id,
                      cite     = citation_id,
                      ghs      = greenhouse)
  selected <- subset (transformed, select = c('Y', 'n', 'site', 'trt', 'ghs', 'obs.prec', 'se', 'cite'))
  return(selected)
}
transform.nas <- function(data){
  #control defaults to 1
  data$control[is.na(data$control)]     <- 1
  
  #site defaults to 0
  #TODO assign different site for each citation - dsl
  data$site_id[is.na(data$site_id)] <- 0

  #greenhouse defaults to false (0)
  data$greenhouse[is.na(data$greenhouse)] <- 1
  
  #number of observations defaults to 2 for statistics, 1 otherwise
  data$n[is.na(data$n)] <- 1
  data$n[data$n ==1 & !is.na(data$stat)] <- 2

  return(data)
}
assign.controls <- function(data){
  data$trt_id[which(data$control == 1)] <- 'control'
  sites <- unique(data$site_id)
  for(ss in sites){
    site.i <- data$site == ss
    #if only one treatment, it's control
    if(length(unique(data$trt[site.i])) == 1) data$trt_id[site.i] <- 'control'
    if(!'control' %in% data$trt_id[site.i]){
      if(interactive()) browser()
      stop(  paste('No control treatment set for site_id:',
        unique(data$site_id[site.i]),
        'and citation id',
        unique(data$citation_id[site.i]),
        '\nplease set control treatment for this site / citation in database\n'))
    }
  }
  return(data)
}
drop.columns <- function(data, columns){
  return(data[,which(!colnames(data) %in% columns)])
}
query.covariates<-function(trait.ids, con = NULL, ...){
  if(is.null(con)){
    con <- query.bety.con(...)
  }
  covariate.query <- paste("select covariates.trait_id, covariates.level,variables.name",
                         "from covariates left join variables on variables.id = covariates.variable_id",
                         "where trait_id in (",vecpaste(trait.ids),")")
  q <- dbSendQuery(con, covariate.query)
  all.covs = fetch(q, n = -1)  
  return(all.covs)
}

##' Append covariate data as a column within a table
##' @name append.covariate
##'
##' \code{append.covariate} appends one or more tables of covariate data 
##' as a single column in a given table of trait data.
##' In the event a trait has several covariates across several given tables, 
##' the first table given will take precedence
##'
##' @param data trait dataframe that will be appended to.
##' @param covariate name of the covariate as it will appear in the appended column
##' @param ... one or more tables of covariate data, ordered by the precedence 
##' they will assume in the event a trait has covariates across multiple tables.
##' All tables must contain an 'id' and 'level' column, at minimum. 
append.covariate<-function(data, covariate, ...){
  merged <- data.frame()
  for(covariate.data in list(...)){
    if(length(covariate.data)>1){
      #conditional added to prevent crash when trying to transform an empty data frame
      transformed <- transform(covariate.data, id = trait_id, level = level)
      selected <- transformed[!transformed$id %in% merged$id, c('id', 'level')]
      merged <- rbind(merged, selected)
    }
  }
  colnames(merged) <- c('id', covariate)
  merged <- merge(merged, data, all = TRUE)
  return(merged)
}

##' Extract trait data from BETYdb
##' @name query.bety.trait.data
##'
##' \code{query.bety.trait.data} extracts data from BETYdb for a given trait and set of species,
##' converts all statistics to summary statistics, and prepares a dataframe for use in meta-analysis.
##' For Vcmax and SLA data, only data collected between  April and July are queried, and only data collected from the top of the canopy (canopy height > 0.66).
##' For Vcmax and root_respiration_rate, data are scaled
##' converted from measurement temperature to \eqn{25^oC} via the arrhenius equation.
##'
##' @param trait is the traiat name used in BETY, stored in variables.name
##' @param spstr is the species.id integer or string of integers associated with the species
##'  
##' @return dataframe ready for use in meta-analysis

query.bety.trait.data <- function(trait, spstr,con=NULL,...){
  if(is.null(con)){
    con <- query.bety.con(...)
  }
  
  if(is.list(con)){
    print("query.bety.trait.data")
    print("WEB QUERY OF DATABASE NOT IMPLEMENTED")
    return(NULL)
  } 
  print(trait)
 
  if(trait == 'Vcmax') {
    #########################   VCMAX   ############################
    query <- paste("select traits.id, traits.citation_id, traits.site_id, treatments.name, month(traits.date) as month, traits.dateloc, treatments.control, sites.greenhouse, traits.mean, traits.statname, traits.stat, traits.n, traits.date, traits.time, traits.cultivar_id, traits.specie_id from traits left join treatments on  (traits.treatment_id = treatments.id) left join sites on (traits.site_id = sites.id) where specie_id in (", spstr,") and variable_id in ( select id from variables where name = '", trait,"');", sep = "")
    data <- fetch.stats2se(con, query)
    all.covs <- query.covariates(data$id, con)
    
    if(length(all.covs)>0) {
      ## get temperature covariates
      data <- append.covariate(data, 'leafT', 
          all.covs[all.covs$name == 'leafT',],
          all.covs[all.covs$name == 'airT',])
      ## get canopy height covariates
      data <- append.covariate(data, 'canopy_layer',
          all.covs[all.covs$name == 'canopy_layer',])
      ## select sunleaf data
      data <- data[data$canopy_layer >= 0.66 | is.na(data$canopy_layer), ]
    }
    
    ## set default leafT to 25 if unknown
    data$leafT[is.na(data$leafT)] <-  25

    data$mean <- arrhenius.scaling(data$mean, old.temp = data$leafT)
    data$stat <- arrhenius.scaling(data$stat, old.temp = data$leafT)

    ## select only summer data for Panicum virgatum
    ##TODO fix following hack to select only summer data
    if (spstr == "'938'"){
      data <- subset(data, subset = data$month %in% c(0,5,6,7))
    }
    result <- drop.columns(data, c('leafT', 'canopy_layer','date','dateloc'))
    
  } else if (trait == 'SLA') {
    
    #########################    SLA    ############################
    query <- paste("select trt.id, trt.citation_id, trt.site_id, month(trt.date) as month, treat.name, treat.control, sites.greenhouse, variables.name as vname, trt.mean, trt.statname, trt.stat, trt.n, trt.date, trt.time, trt.cultivar_id, trt.specie_id from traits as trt left join treatments as treat on (trt.treatment_id = treat.id)  left join sites on (sites.id = trt.site_id) join variables on trt.variable_id = variables.id where variables.name in('LMA','SLA')  and specie_id in (",spstr,");", sep = "")
    data <- fetch.stats2se(con, query)

    ## convert LMA to SLA
    selLMA <- which(data$vname == "LMA")
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
    all.covs = query.covariates(data$id, con = con)

    ## get canopy height covariates
    #conditional added to prevent crash when trying to transform an empty data frame
    if(length(all.covs)>0) {  
      data <- append.covariate(data, 'canopy_layer',
          all.covs[all.covs$name == 'canopy_layer',])
      data <-  data[data$canopy_layer >= 0.66 | is.na(data$canopy_layer),]
    }
 
    ## select only summer data for Panicum virgatum
    if (spstr == "'938'"){
      data <- subset(data, subset = data$month %in% c(0,5,6,7,8,NA))
    }

    result <- drop.columns(data, 'canopy_layer')

  } else if (trait == 'leaf_turnover_rate'){
    
    #########################    LEAF TURNOVER    ############################
    query <- paste("select trt.id, trt.citation_id, variables.name as vname, trt.site_id, treat.name, treat.control, sites.greenhouse, trt.mean, trt.statname, trt.stat, trt.n, trt.date, trt.time, trt.cultivar_id, trt.specie_id from traits as trt left join treatments as treat on (trt.treatment_id = treat.id)  left join sites on (sites.id = trt.site_id) left join variables on (variables.id = trt.variable_id) where variables.name in ('leaf_turnover_rate','leaf_longevity') and specie_id in (",spstr,");", sep = "")
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
    query <- paste("select traits.id, traits.citation_id, traits.site_id, treatments.name, month(traits.date) as month, traits.dateloc, treatments.control, sites.greenhouse, traits.mean, traits.statname, traits.stat, traits.n, traits.date, traits.time, traits.cultivar_id, traits.specie_id from traits left join treatments on  (traits.treatment_id = treatments.id) left join sites on (traits.site_id = sites.id) where specie_id in (", spstr,") and variable_id in ( select id from variables where name = '", trait,"');", sep = "")
    data <- fetch.stats2se(con, query)

    all.covs = query.covariates(data$id, con = con)

    ## get temperature covariates
    data <- append.covariate(data, 'rootT', 
        all.covs[all.covs$name == 'rootT',],
        all.covs[all.covs$name == 'airT',])
    
    ## Scale to 25C using Arrhenius scaling,
    data$mean <- arrhenius.scaling(data$mean, old.temp = data$rootT, new.temp = 25)
    data$stat <- arrhenius.scaling(data$stat, old.temp = data$rootT, new.temp = 25)
    result <- drop.columns(data, c('rootT', 'date', 'dateloc'))
    
  } else if (trait == 'c2n_leaf') {

    #########################  LEAF C:N   ############################

    query <- paste("select traits.id, traits.citation_id, variables.name as vname, traits.site_id, treatments.name, treatments.control, sites.greenhouse, traits.mean, traits.statname, traits.stat, traits.n, traits.date, traits.time, traits.cultivar_id, traits.specie_id from traits left join treatments on  (traits.treatment_id = treatments.id) left join sites on (traits.site_id = sites.id) left join variables on (traits.variable_id = variables.id) where specie_id in (", spstr,")  and variables.name in ('c2n_leaf', 'leafN');", sep = "")

    data <- fetch.stats2se(con, query)
    leafNdata   <- data$name == 'leafN'
    leafNdataSE <- leafNdata & data$statname == 'SE'
    inv.se <- function(mean, stat, n) signif(sd(48/rnorm(100000, mean, stat*sqrt(n)))/sqrt(n),3)
    data$stat[leafNdataSE] <- apply(data[leafNdataSE, c('mean', 'stat', 'n')],1, function(x) inv.se(x[1],x[2],x[3]) )
    data$mean[data$vname == 'leafN'] <- 48/data$mean[data$vname == 'leafN']
    result <- data
  } else if (trait == 'fineroot2leaf') {

    #########################  FINE ROOT ALLOCATION  ############################
    ## query Q or FRC_RC
    query <- paste("select traits.citation_id, traits.id, variables.name as vname, traits.site_id, treatments.name, treatments.control, sites.greenhouse, traits.mean, traits.statname, traits.stat, traits.n, traits.date, traits.time, traits.cultivar_id, traits.specie_id from traits left join treatments on  (traits.treatment_id = treatments.id) left join sites on (traits.site_id = sites.id) left join variables on (traits.variable_id = variables.id) where specie_id in (", spstr,")  and variables.name in ('fineroot2leaf', 'FRC_RC');", sep = "")
    data <- fetch.stats2se(con, query)

    ## query fine root biomass and leaf biomass
    query <- paste("select traits.citation_id, traits.id, variables.name as vname, traits.site_id, treatments.name, treatments.control, sites.greenhouse, traits.mean, traits.statname, traits.stat, traits.n, traits.date, traits.time, traits.cultivar_id, traits.specie_id, traits.specie_id from traits left join treatments on  (traits.treatment_id = treatments.id) left join sites on (traits.site_id = sites.id) left join variables on (traits.variable_id = variables.id) where specie_id in (", spstr,")  and variables.name in ('fine_root_biomass','leaf_biomass');", sep = "")
    data2 <- fetch.stats2se(con, query)

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
                newrow = data2[roots,]; newrow$vname = 'fineroot2leaf'
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
      if(!is.null(data3)) data3 <- drop.columns(data3, "specie_id")
    }
    result <- rbind(data,data3)
  }  else {
    #########################  GENERIC CASE  ############################
        query <- paste("select traits.id, traits.citation_id, traits.site_id, treatments.name, treatments.control, sites.greenhouse, traits.mean, traits.statname, traits.stat, traits.n, traits.date, traits.time, traits.cultivar_id, traits.specie_id from traits left join treatments on  (traits.treatment_id = treatments.id) left join sites on (traits.site_id = sites.id) where specie_id in (", spstr,") and variable_id in ( select id from variables where name = '", trait,"');", sep = "")
    result <- fetch.stats2se(con, query)
  }

  ## if result is empty, stop run
  if(!exists('result') || nrow(result)==0) stop(paste('no data in database for', trait))


  
  ## rename name column from treatment table to trt_id
  names(result)[names(result)=='name'] <- 'trt_id'
  
  result <- transform.nas(result)
  result <- assign.controls(result)

  ## calculate summary statistics from experimental replicates
  result <- summarize.result(result)

  ## assign a unique sequential integer to site and trt; for trt, all controls == 0
  data <- subset(transform(result,
                           stat = as.numeric(stat),
                           n    = as.numeric(n),
                           site_id = as.integer(factor(site_id, unique(site_id))),
                           trt_id = as.integer(factor(trt_id, unique(c('control', as.character(trt_id))))),
                           greenhouse = as.integer(factor(greenhouse, unique(greenhouse))),
                           mean = mean,
                           citation_id = citation_id), 
                 select = c('stat', 'n', 'site_id', 'trt_id', 'mean', 'citation_id', 'greenhouse')) 


  if(length(data$stat[!is.na(data$stat) & data$stat <= 0.0]) > 0) {
    citationswithbadstats <- unique(data$citation_id[which(data$stat <= 0.0)])
    warning.message <- paste('there are implausible values of SE: SE <= 0 \n',
                             'for', trait, 'data from citation',citationswithbadstats,'\n',
                             'SE <=0 set to NA \n')
    warning(warning.message)
    print(data)
    data$stat[data$stat <= 0.0] <- NA
  }
  
  renamed <- rename.jags.columns(data)
  return(renamed)
}


