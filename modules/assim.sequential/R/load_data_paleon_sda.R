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
  ## STEPPS Fcomp (Work in progress)
  ## ReFAB biomass
  ## HF Tree Rings (CHECK)
  ## Tree Rings from tree ring module
  ## PLS and FIA Biomass Snapshot
  ## Wish list: eddy covariance
  #browser()
  if(file.exists(file.path(settings$outdir,'sda.obs.Rdata'))){
    load(file.path(settings$outdir,'sda.obs.Rdata'))
    return(obs.list)
  }
  
  suggests_avail <- c(
    reshape2 = requireNamespace("reshape2", quietly = TRUE),
    plyr = requireNamespace("plyr", quietly = TRUE))
  suggests_missing <- paste(
    sQuote(names(suggests_avail)[!suggests_avail], q = FALSE),
    collapse = ", ")
  if (!all(suggests_avail)) {
    PEcAn.logger::logger.error(
      "Can't find package(s)", suggests_missing,
      ", needed by `PEcAnAssimSequential::load_data_paleon_sda()`.",
      "Please install these and try again.")
  }
  
  d <- settings$database$bety
  con <- PEcAn.DB::db.open(d)
  
  if(settings$host$name != 'localhost') PEcAn.logger::logger.severe('ERROR: Code does not support anything but settings$host$name <- localhost at this time.')
  
  site <- PEcAn.DB::query.site(settings$run$site$id, con)
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
  obs.times <- formatC(lubridate::year(obs.times), width = 4, format = "d", flag = "0")
  
  biomass2carbon <- 0.48
  
  for(i in seq_along(format_id)){
    input.list[[i]] <- PEcAn.DB::db.query(paste("SELECT * FROM inputs WHERE site_id =",site$id ,"  AND format_id = ",format_id[[i]]), con)
    input.id[[i]] <- input.list[[i]]$id
    
    data.path <- PEcAn.DB::query.file.path(input.id[[i]], settings$host$name, con)
    format_full <- format <- PEcAn.DB::query.format.vars(
      bety = con,
      input.id = input.id[[i]],
      format.id = NA,
      var.ids=NA)
    

    ### Tree Ring Data Product
    if(format_id[[i]] == '1000000040'){
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
      if(!is.null(obvs[[i]]$AbvGrndWood))obvs[[i]]$AbvGrndWood <- obvs[[i]]$AbvGrndWood * biomass2carbon #* kgm2Mgha 
      if(!is.null(obvs[[i]]$GWBI)) obvs[[i]]$GWBI <- obvs[[i]]$GWBI * biomass2carbon  #* kgms2Mghayr 
      arguments <- list(
        plyr::.(year, MCMC_iteration, site_id),
        plyr::.(variable))
      arguments2 <- list(plyr::.(year), plyr::.(variable))
      arguments3 <- list(
        plyr::.(MCMC_iteration),
        plyr::.(variable),
        plyr::.(year))

      dataset <- obvs[[i]]
      
      ### Map species to model specific PFTs
      if(any(var.names == 'AGB.pft')){
        # this is the only code path that uses data.land, so we check now instead of at top of function
        if (!requireNamespace("PEcAn.data.land", quietly = TRUE)) {
          PEcAn.logger::logger.error(
            "Can't find package 'PEcAn.data.land',",
            "needed by `PEcAnAssimSequential::load_data_paleon_sda()`.",
            "Please install it and try again.")
        }
        spp_id <- PEcAn.data.land::match_species_id(unique(dataset$species_id),format_name = 'usda', con)
        pft_mat <- PEcAn.data.land::match_pft(spp_id$bety_species_id, settings$pfts,
                             con = con, allow_missing = TRUE)
        
        x <- paste0('AGB.pft.', pft_mat$pft)
        names(x) <- spp_id$input_code
        
        PEcAn.logger::logger.info('Now, mapping data species to model PFTs')
        dataset$pft.cat <- x[dataset$species_id]
        dataset <- dataset[dataset$pft.cat!='AGB.pft.NA',]
        
        variable <- c('AbvGrndWood')
        arguments <- list(
          plyr::.(year, MCMC_iteration, site_id, pft.cat),
          plyr::.(variable))
        arguments2 <- list(plyr::.(year, pft.cat), plyr::.(variable))
        arguments3 <- list(
          plyr::.(MCMC_iteration),
          plyr::.(pft.cat, variable),
          plyr::.(year))
      }

      PEcAn.logger::logger.info('Now, aggregating data and creating SDA input lists')
      melt_id <- colnames(dataset)[-which(colnames(dataset) %in% variable)]
      melt.test <- reshape2::melt(dataset, id = melt_id, na.rm = TRUE)
      cast.test <- reshape2::dcast(melt.test, arguments, sum, margins = variable)
      
      melt_id_next <- colnames(cast.test)[-which(colnames(cast.test) %in% variable)]
      melt.next <- reshape2::melt(cast.test, id = melt_id_next)
      mean_mat <- reshape2::dcast(melt.next, arguments2, mean)
      
      iter_mat <- reshape2::acast(melt.next, arguments3, mean)
      cov.test <- apply(iter_mat,3,function(x){stats::cov(x)})
      
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
    }
    
    ### Pollen Data Product (STEPPS)
    if(format_id[[i]] == '1000000058'){
      ncin <- ncdf4::nc_open(data.path)
      
      coords <- data.frame(x=site$lon,y=site$lat)
      sp::coordinates(coords) <- ~ x + y
      sp::proj4string(coords) <- sp::CRS('+proj=longlat +ellps=WGS84')
      
      ### site utm coordinates
      utm <- sp::spTransform(coords, sp::CRS("+proj=utm +zone=18N ellps=WGS84"))
      utm <- as.matrix(data.frame(utm))
      
      ### find grid cell
      site.x <- which(min(abs(ncdf4::ncvar_get(ncin, 'x') - utm[1])) == abs(ncdf4::ncvar_get(ncin, 'x') - utm[1]))
      site.y <- which(min(abs(ncdf4::ncvar_get(ncin, 'y') - utm[2])) == abs(ncdf4::ncvar_get(ncin, 'y') - utm[2]))
      years <- formatC(ncdf4::ncvar_get(ncin, 'year'), width = 4, format = "d", flag = "0")
      
      taxa <- names(ncin$var)
      if('other'%in%taxa) taxa <- taxa[-c(grep('other',taxa))]
        
      sims.keep <- array(NA,dim=c(length(taxa),length(ncin$dim$year$vals),length(ncin$dim$sample$vals)))
      for(n in seq_along(taxa)){
        taxa.start <- ncdf4::ncvar_get(ncin, taxa[n])
        
        # input is a matrix 'sims', with rows as time and columns as MCMC samples
        sims.keep[n,,] <- taxa.start[site.x,site.y,,]
      }
      
      #####
      ##### Calculating Effective Sample Size #####
      #####
      
      ESS_calc <- function(ntimes, sims){
        row.means.sims <- sims - rowMeans(sims)  # center based on mean at each time to remove baseline temporal correlation 
        # (we want to estimate effective sample size effect from correlation of the errors)
        
        # compute all pairwise covariances at different times
        covars <- NULL
        for(lag in 1:(ntimes-1)){
          covars <- c(covars, rowMeans(row.means.sims[(lag+1):ntimes, , drop = FALSE] * row.means.sims[1:(ntimes-lag), , drop = FALSE])) 
        }
        vars <- apply(row.means.sims, 1, stats::var) # pointwise post variances at each time, might not be homoscedastic
        
        # nominal sample size scaled by ratio of variance of an average
        # under independence to variance of average of correlated values
        neff <- ntimes * sum(vars) / (sum(vars) + 2 * sum(covars))
        return(neff)
      }
      
      neff.keep <- mean.keep <- list()
      pecan.pfts <- as.character(lapply(settings$pfts, function(x) x[["name"]]))
      
      for(n in taxa){
        sims.start <- ncdf4::ncvar_get(ncin,n)
        
        # input is a matrix 'sims', with rows as time and columns as MCMC samples
        sims <- sims.start[site.x,site.y,,]
        
        ntimes <- 10
      
        neff.keep[[n]] <- ESS_calc(ntimes = ntimes, sims = sims)
        mean.keep[[n]] <- rowMeans(sims)
       
      }
      
      var.inf <- 1000/mean(unlist(neff.keep))
      
      mean.mat <- as.data.frame(mean.keep)
      
      for(n in seq_len(ncol(mean.mat))){
        new.name <- pecan.pfts[grep(taxa[n],pecan.pfts,ignore.case = T)]
        if(any(nchar(new.name))){
          colnames(mean.mat)[n] <- paste0('Fcomp.',new.name)
        }
      }
      
      #####
      ##### Calculating Mean and Covariance
      #####

      obs.mean <- list()
      for(n in 1:nrow(mean.mat)){
        obs.mean[[n]]<- mean.mat[n,]
      }
      
      names(obs.mean) <- paste0(years,'/12/31')

      rownames(sims.keep) <- colnames(mean.mat)
      obs.cov <- list()
      for(n in 1:length(ncin$dim$year$vals)){
        obs.cov[[n]] <- stats::cov(t(sims.keep[,n,])) #* var.inf
      }
      
      names(obs.cov) <- paste0(years,'/12/31')
      
      #### Interpolate over all years
      which.keep <- list()
      
      for(n in obs.times){
        min.vec <- stats::na.omit(as.numeric(n) - year(as.Date(names(obs.mean))))
        which.keep[[n]] <- which(min(abs(min.vec))==abs(min.vec))
        obs.mean.tmp[[n]] <- obs.mean[[which.keep[[n]][1]]]
        obs.cov.tmp[[n]] <- obs.cov[[which.keep[[n]][1]]]
      }
      
      names(obs.mean.tmp)<-paste0(obs.times,'/12/31')
      names(obs.cov.tmp)<-paste0(obs.times,'/12/31')
      
    }
    
    ### Error Message for no data product
    if(format_id[[i]] != '1000000040' & format_id[[i]] != '1000000058'){
      PEcAn.logger::logger.severe('ERROR: This data format has not been added to this function (ツ)_/¯ ')
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
  
  obs.list <- list(obs.mean = obs.mean, obs.cov = obs.cov)
  save(obs.list,file=file.path(settings$outdir,'sda.obs.Rdata'))

return(obs.list)

}
