##' @title load_data_paleon_sda
##' @name  load_data_paleon_sda
##' @author Ann Raiho \email{araiho@nd.edu}
##' 
##' @param settings    PEcAn SDA settings object
##' 
##' @description Load data function for paleon SDA data products
##' 
##' @return obs.mean and obs.cov for sda.enkf
##' @export
##' 
load_data_paleon_sda <- function(settings){
  
  #### Types of Data needed
  ## STEPPS Fcomp
  ## ReFAB biomass
  ## HF Tree Rings (CHECK)
  ## Tree Rings from tree ring module
  ## PLS and FIA Biomass Snapshot
  ## Wish list: eddy covariance
  
  d <- settings$database$bety[c("dbname", "password", "host", "user")]
  bety <- src_postgres(host = d$host, user = d$user, password = d$password, dbname = d$dbname)
  settings$host$name <- "localhost"
  
  site <- PEcAn.DB::query.site(settings$run$site$id, bety$con)
  format_id <- settings$state.data.assimilation$data$format_id
  
  input.list <- list()
  input.id <- list()
  obvs <- list()
  
  var.names <- unlist(sapply(settings$state.data.assimilation$state.variable, 
                             function(x) {
                               x$variable.name
                             }, 
                             USE.NAMES = FALSE), 
                      use.names = FALSE)
  
  start_date <- settings$state.data.assimilation$start.date
  end_date   <- settings$state.data.assimilation$end.date
  
  obs.mean <- list()
  obs.cov <- list()
  
  start.time <- format(lubridate::ymd(start_date),settings$state.data.assimilation$forecast.time.step)
  end.time <- format(lubridate::ymd(end_date),settings$state.data.assimilation$forecast.time.step)
  obs.times <- start.time:end.time
  
  
  for(i in seq_along(format_id)){
    input.list[[i]] <- db.query(paste("SELECT * FROM inputs WHERE site_id =",site$id ,"  AND format_id = ",format_id[[i]]), bety$con)
    input.id[[i]] <- input.list[[i]]$id
    
    data.path <- PEcAn.DB::query.file.path(input.id[[i]], settings$host$name, bety$con)
    format_full <- format <- PEcAn.DB::query.format.vars(input.id = input.id[[i]], bety, format.id = NA, var.ids=NA)
    
    format$na.strings <- 'NA'
    
    # ---- LOAD INPUT DATA ---- #
    time.row <- format$time.row
    vars.used.index <- setdiff(seq_along(format$vars$variable_id), format$time.row)
    
    print(paste('Using PEcAn.benchmark::load_data.R on format_id',format_id[[i]],'-- may take a few minutes'))
    obvs[[i]] <- PEcAn.benchmark::load_data(data.path, format, start_year, end_year, site, vars.used.index, time.row)
    
    dataset <- obvs[[i]]
    variable <- var.names
    
    ### Tree Ring Data Product
    if(format_id[[i]] == '1000000040'){
      obvs[[i]] <- obvs[[i]][obvs[[i]]$model_type=='Model RW + Census',]
      obvs[[i]]$AbvGrndWood <- obvs[[i]]$AbvGrndWood * .48
      obvs[[i]]$NPP_1_C <- obvs[[i]]$NPP_1_C * .48
      arguments <- list(.(year, MCMC_iteration, site_id), .(variable))
      arguments2 <- list(.(year), .(variable))
      arguments3 <- list(.(MCMC_iteration), .(variable), .(year))
    }else{
      print('ERROR: This data format has not been added to this function (ツ)_/¯ ')
      stop()
    }
    
    ### Map species to model specific PFTs
    if(any(var.names == 'AGB.pft')){
      spp_id <- match_species_id(unique(dataset$species_id),format_name = 'usda',bety)
      spp_id <- spp_id[spp_id$input_code!='HAVI4',]
      pft_mat <- match_pft(spp_id$bety_species_id, settings$pfts, con = bety$con)
      
      x = paste0('AGB.pft.', pft_mat$pft)
      names(x) = spp_id$input_code
      dataset$pft.cat = x[dataset$species_id]
      variable <- sub('AGB.pft','AbvGrndWood',variable)
      arguments <- list(.(year, MCMC_iteration, site_id, pft.cat), .(variable))
      arguments2 <- list(.(year, pft.cat), .(variable))
      arguments3 <- list(.(MCMC_iteration), .(pft.cat, variable), .(year))
    } 
    
    print('Now, melting data')
    melt_id <- colnames(dataset)[-which(colnames(dataset) %in% variable)]
    melt.test <- reshape2::melt(dataset, id = melt_id, na.rm = TRUE)
    cast.test <- reshape2::dcast(melt.test, arguments, sum, margins = variable)
    
    melt_id <- colnames(cast.test)[-which(colnames(cast.test) %in% variable)]
    melt.next <- reshape2::melt(cast.test, id = melt_id)
    mean_mat <- reshape2::dcast(melt.next, arguments2, mean)
    
    iter_mat <- reshape2::acast(melt.next, arguments3, mean)
    cov.test <- apply(iter_mat,3,function(x){cov(x)})
   
    for(t in seq_along(obs.times)){
      obs.mean[[t]] <- mean_mat[mean_mat$year==obs.times[t], variable]
      if(any(var.names == 'AGB.pft')) names(obs.mean[[t]]) <- mean_mat[mean_mat$year==obs.times[t], 'pft.cat']
      obs.cov[[t]] <- matrix(cov.test[,which(colnames(cov.test) %in% obs.times[t])],
                             ncol = length(variable),
                             nrow = length(variable))
    } 
    
    ### Combine data if more than one type of data
    if(i > 1){
      for(t in seq_along(obs.times)){
        obs.mean[[t]] <- c(obs.mean[[t]],unlist(obs.mean[[t]]))
        obs.cov[[t]] <- magic::adiag(obs.cov[[t]],unlist(obs.cov[[t]]))
      }
    }
    
    names(obs.mean) <- paste0(obs.times,'/12/31')
    names(obs.cov) <- paste0(obs.times,'/12/31')
  }

return(list(obs.mean = obs.mean, obs.cov = obs.cov))

}
