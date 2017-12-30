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
  
  if(file.exists(file.path(settings$outdir,'sda.obs.Rdata'))){
    load(file.path(settings$outdir,'sda.obs.Rdata'))
    return(obs.list)
  }
  
  library(plyr) #need to load to use .fnc below
  
  d <- settings$database$bety[c("dbname", "password", "host", "user")]
  bety <- src_postgres(host = d$host, user = d$user, password = d$password, dbname = d$dbname)
  
  if(settings$host$name != 'localhost') PEcAn.logger::logger.severe('ERROR: Code does not support anything but settings$host$name <- localhost at this time.')
  
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
  
  obs.mean <- obs.mean.tmp <- list()
  obs.cov <- obs.cov.tmp <- list()

  obs.times <- seq(as.Date(start_date), as.Date(end_date), by = settings$state.data.assimilation$forecast.time.step)
  obs.times <- lubridate::year(obs.times)
  
  biomass2carbon <- 0.48
  
  for(i in seq_along(format_id)){
    input.list[[i]] <- db.query(paste("SELECT * FROM inputs WHERE site_id =",site$id ,"  AND format_id = ",format_id[[i]]), bety$con)
    input.id[[i]] <- input.list[[i]]$id
    
    data.path <- PEcAn.DB::query.file.path(input.id[[i]], settings$host$name, bety$con)
    format_full <- format <- PEcAn.DB::query.format.vars(input.id = input.id[[i]], bety, format.id = NA, var.ids=NA)
    
    if(TRUE){
      # hack instead of changing the units in BETY format for now
      # I don't want load_data to convert anything
      # data itself is in Mg / ha 
      format$vars[1,1] <-  format$vars[1,8] <- format$vars[1,10] <- "AbvGrndWood"
      format$vars[1,4] <- "kg C m-2"
      
      format$vars[4,1] <-  format$vars[4,8] <- format$vars[4,10] <- "GWBI"
      format$vars[4,4] <- "kg C m-2 s-1"
    }
    
    format$na.strings <- 'NA'
    time.row <- format$time.row
    time.type <- format$vars$input_units[time.row] #THIS WONT WORK IF TIMESTEP ISNT ANNUAL
    
    # ---- LOAD INPUT DATA ---- #
    PEcAn.logger::logger.info(paste('Using PEcAn.benchmark::load_data.R on format_id',format_id[[i]],'-- may take a few minutes'))
    obvs[[i]] <- PEcAn.benchmark::load_data(data.path, format, start_year = lubridate::year(start_date), end_year = lubridate::year(end_date), site)
    
    variable <- intersect(var.names,colnames(obvs[[i]]))
    
    ### Tree Ring Data Product
    if(format_id[[i]] == '1000000040'){
      obvs[[i]] <- obvs[[i]][obvs[[i]]$model_type=='Model RW + Census',]
      obvs[[i]]$AbvGrndWood <- obvs[[i]]$AbvGrndWood * biomass2carbon
      obvs[[i]]$NPP <- obvs[[i]]$NPP #* biomass2carbon #kg/m^2/s
      arguments <- list(.(year, MCMC_iteration, site_id), .(variable))
      arguments2 <- list(.(year), .(variable))
      arguments3 <- list(.(MCMC_iteration), .(variable), .(year))
    }else{
      PEcAn.logger::logger.severe('ERROR: This data format has not been added to this function (ツ)_/¯ ')
    }
    
    dataset <- obvs[[i]]
    
    ### Map species to model specific PFTs
    if(any(var.names == 'AGB.pft')){
      spp_id <- match_species_id(unique(dataset$species_id),format_name = 'usda',bety)
      pft_mat <- match_pft(spp_id$bety_species_id, settings$pfts,
                           con = bety$con, allow_missing = TRUE)
      
      x <- paste0('AGB.pft.', pft_mat$pft)
      names(x) <- spp_id$input_code
      
      PEcAn.logger::logger.info('Now, mapping data species to model PFTs')
      dataset$pft.cat <- x[dataset$species_id]
      dataset <- dataset[dataset$pft.cat!='AGB.pft.NA',]
      
      variable <- c('AbvGrndWood')
      arguments <- list(.(year, MCMC_iteration, site_id, pft.cat), .(variable))
      arguments2 <- list(.(year, pft.cat), .(variable))
      arguments3 <- list(.(MCMC_iteration), .(pft.cat, variable), .(year))
    } 
    
    PEcAn.logger::logger.info('Now, aggregating data and creating SDA input lists')
    melt_id <- colnames(dataset)[-which(colnames(dataset) %in% variable)]
    melt.test <- reshape2::melt(dataset, id = melt_id, na.rm = TRUE)
    cast.test <- reshape2::dcast(melt.test, arguments, sum, margins = variable)
    
    melt_id_next <- colnames(cast.test)[-which(colnames(cast.test) %in% variable)]
    melt.next <- reshape2::melt(cast.test, id = melt_id_next)
    mean_mat <- reshape2::dcast(melt.next, arguments2, mean)
    
    iter_mat <- reshape2::acast(melt.next, arguments3, mean)
    cov.test <- apply(iter_mat,3,function(x){cov(x)})
   
    for(t in seq_along(obs.times)){
      obs.mean.tmp[[t]] <- mean_mat[mean_mat[,time.type]==obs.times[t], -c(1)] #THIS WONT WORK IF TIMESTEP ISNT ANNUAL
      
      if(any(var.names == 'AGB.pft')){
        obs.mean.tmp[[t]] <- rep(NA, length(unique(dataset$pft.cat)))
        names(obs.mean.tmp[[t]]) <- sort(unique(dataset$pft.cat))
        for(r in seq_along(unique(dataset$pft.cat))){
          k <- mean_mat[mean_mat$year==obs.times[t] & mean_mat$pft.cat==names(obs.mean.tmp[[t]][r]), variable]
          if(any(k)){
            obs.mean.tmp[[t]][r] <- k
          }
        }
      }
      
      obs.cov.tmp[[t]] <- matrix(cov.test[,which(colnames(cov.test) %in% obs.times[t])],
                             ncol = sqrt(dim(cov.test)[1]),
                             nrow = sqrt(dim(cov.test)[1]))
      if(any(var.names == 'AGB.pft')){
        colnames(obs.cov.tmp[[t]]) <- names(iter_mat[1,,t]) 
      }
    } 
    
    ### Combine data if more than one type of data
    if(i > 1){
      for(t in seq_along(obs.times)){
        obs.mean[[t]] <- c(obs.mean[[t]],unlist(obs.mean.tmp[[t]]))
        obs.cov[[t]] <- magic::adiag(obs.cov[[t]],unlist(obs.cov.tmp[[t]]))
      }
    }else{
      obs.mean <- obs.mean.tmp
      obs.cov <- obs.cov.tmp
    }
    
    names(obs.mean) <- paste0(obs.times,'/12/31')
    names(obs.cov) <- paste0(obs.times,'/12/31')
  }
  
  obs.list <- list(obs.mean = obs.mean, obs.cov = obs.cov)
  save(obs.list,file=file.path(settings$outdir,'sda.obs.Rdata'))

return(obs.list)

}
