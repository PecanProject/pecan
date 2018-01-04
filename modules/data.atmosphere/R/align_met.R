##' Align meteorology datasets for debiasing
# -----------------------------------
# Description
# -----------------------------------
##'
##' @title align.met
##' @family debias - Debias & Align Meteorology Datasets into continuous time series
##' @author Christy Rollinson
##' @description This script aligns meteorology datasets in at temporal resolution for debiasing & 
##'              temporal downscaling.  
##'              Note: The output here is stored in memory!  
##'              Note: can probably at borrow from or adapt align_data.R in Benchmarking module, but 
##'              it's too much of a black box at the moment.
# -----------------------------------
# Notes
# -----------------------------------
##' @details 1. Assumes that both the training and source data are in *at least* daily resolution 
##'             and each dataset is in a consistent temporal resolution being read from a single file 
##'             (CF/Pecan format).  For example, CMIP5 historical/p1000 runs where radiation drivers 
##'             are in monthly resolution and temperature is in daily will need to be reconciled using
##'             one of the "met2CF" or "download" or "extract" functions
##'          2. Default file structure: Ensembles members for a given site or set of simes are housed 
##'             in a common folder with the site ID.  Right now everything is based off of Christy's 
##'             PalEON ensemble ID scheme where the site ID is a character string (e.g. HARVARD) followed
##'             the SOURCE data family (i.e. GCM) as a string and then the ensemble member ID as a number 
##'             (e.g. 001).  For example, the file path for a single daily ensemble member for PalEON is:
##'             "~/Desktop/Research/met_ensembles/data/met_ensembles/HARVARD/day/ensembles/bcc-csm1-1_004"
##'             with each year in a separate netcdf file inside of it.
##' @return 2-layered list (stored in memory) containing the training and source data that are now matched
##'         in temporal resolution have the specified number of ensemble members
##'          - dat.train (training dataset) and dat.source (source data to be downscaled or bias-corrected)
##'            are both lists that contain separate data frames for time indices and all available met 
##'            variables with ensemble members in columns
# -----------------------------------
# Parameters
# -----------------------------------
##' @param train.path - path to the dataset to be used to downscale the data
##' @param source.path - data to be bias-corrected aligned with training data (from align.met)
##' @param yrs.train - (optional) specify a specific years to be loaded for the training data; 
##'                    prevents needing to load the entire dataset.  If NULL, all available years 
##'                    will be loaded. If not null, should be a vector of numbers (so you can skip
##'                    problematic years)
##' @param yrs.source - (optional) specify a specific years to be loaded for the source data;
##'                     prevents needing to load the entire dataset.  If NULL, all available years 
##'                     will be loaded. If not null, should be a vector of numbers (so you can skip
##'                     problematic years)
##' @param n.ens  - number of ensemble members to generate and save
##' @param pair.mems - logical stating whether ensemble members should be paired in 
##'                    the case where ensembles are being read in in both the training and source data
##' @param mems.train - (optional) ensemble identifiers so that the training data is read in a specific order                    
##' @param seed - specify seed so that random draws can be reproduced
##' @param print.progress - if TRUE, prints progress bar
##' @export
# -----------------------------------
# Workflow
# -----------------------------------
# 1. Read in & format the training data
#    1.1. Determine if training data is a single series or ensemble
#       - note: assumes training data is at the temporal resolution you want to work with
#    1.2. If not already, coerce training data into the size of the desired output ensemble
# 2. Read in & format the source data to match the temporal resolution of training data
#    - Note: for now this is only going to work with a single time series & not an ensemble of source data
#    - Note: end dimensions should match that of the training data
# 3. export data (stored in memory) for input into the debiasing or temporal downscaling workflow
# 
# Returns a list called met.out with 2 levels that are matched in temporal resolution & number of ensembles
# List Layers
#  1. dat.train
#  2. dat.source
# Sublist Layers: time, met variables

#----------------------------------------------------------------------
# Begin Function
#----------------------------------------------------------------------
align.met <- function(train.path, source.path, yrs.train=NULL, yrs.source=NULL, n.ens=NULL, pair.mems = FALSE, mems.train=NULL, seed=Sys.Date(), print.progress = FALSE) {
  # Load required libraries
  library(ncdf4)
  library(lubridate)
  
  met.out <- list() # where the aligned data will be stored

  # ---------------
  # 1. Read in & organize training data
  # ---------------
  met.out[["dat.train"]] <- list()
  # 1.a. Determine if we have an ensemble in the training path or if it's a single time series
  if(length(dir(train.path, ".nc"))>0){ # we have a single time series
    n.trn = 1 # Ignore how many input ensemble members we asked for, we only actually have 1 here
    
    files.train <- dir(train.path, ".nc")
    
    yrs.file <- strsplit(files.train, "[.]")
    yrs.file <- matrix(unlist(yrs.file), ncol=length(yrs.file[[1]]), byrow=T)
    yrs.file <- as.numeric(yrs.file[,ncol(yrs.file)-1]) # Assumes year is always last thing before the file extension
    
    if(!is.null(yrs.train)){
      files.train <- files.train[which(yrs.file %in% yrs.train)]
      yrs.file <- yrs.file[which(yrs.file %in% yrs.train)]
    }
    
    # Loop through the .nc files putting everything into a list
    if(print.progress==TRUE){
      print("Processing Training Data")
      pb <- txtProgressBar(min=0, max=length(files.train), style=3)
    } 
    for(i in 1:length(files.train)){
      yr.now <- yrs.file[i]
      
      ncT <- ncdf4::nc_open(file.path(train.path, files.train[i]))
      
      # Set up the time data frame to help index
      nday <- ifelse(lubridate::leap_year(yr.now), 366, 365)
      ntime <- length(ncT$dim$time$vals)
      step.day <- nday/ntime
      step.hr  <- step.day*24
      stamps.hr <- seq(step.hr/2, by=step.hr, length.out=1/step.day) # Time stamps centered on period
      
      # Create a data frame with all the important time info
      # center the hour step
      df.time <- data.frame(Year=yr.now, DOY=rep(1:nday, each=1/step.day), Hour=rep(stamps.hr, length.out=ntime))
      df.time$Date <- strptime(paste(df.time$Year, df.time$DOY, df.time$Hour, sep="-"), format=("%Y-%j-%H"), tz="UTC")
      met.out$dat.train[["time"]] <- rbind(met.out$dat.train$time, df.time)
      
      # Extract the met info, making matrices with the appropriate number of ensemble members
      for(v in names(ncT$var)){
        df.tem <- matrix(rep(ncdf4::ncvar_get(ncT, v), n.trn), ncol=n.trn, byrow=F)

        met.out$dat.train[[v]] <- rbind(met.out$dat.train[[v]], df.tem)
      }
      
      ncdf4::nc_close(ncT)

      if(print.progress==TRUE) setTxtProgressBar(pb, i)
    } # End looping through training data files
  } else { # we have an ensemble we need to deal with
    # Figure out how many ensemble members we're working with
    ens.train <- dir(train.path)
    
    if(is.null(n.ens)) n.ens <- length(ens.train)
    if(length(ens.train)>n.ens & is.null(mems.train)) {
      train.use <- sample(1:length(ens.train), n.ens)
      ens.train <- ens.train[train.use]
    }
    if(!is.null(mems.train)){
      ens.train <- mems.train
    }

    # getting an estimate of how many files we need to process
    yrs.file <- strsplit(dir(file.path(train.path, ens.train[1])), "[.]")
    yrs.file <- matrix(unlist(yrs.file), ncol=length(yrs.file[[1]]), byrow=T)
    yrs.file <- as.numeric(yrs.file[,ncol(yrs.file)-1]) # Assumes year is always last thing before the file extension
    
    if(!is.null(yrs.train)){
      n.files <- length(yrs.file[which(yrs.file %in% yrs.train)])
    } else {
      n.files <- length(dir(file.path(train.path, ens.train[1])))
    }
    
    if(print.progress==TRUE){
      print("Processing Training Data")
      pb <- txtProgressBar(min=0, max=length(ens.train)*n.files, style=3)
      pb.ind=1
    }
    
    for(j in 1:length(ens.train)){
      files.train <- dir(file.path(train.path, ens.train[j]), ".nc")
      
      yrs.file <- strsplit(files.train, "[.]")
      yrs.file <- matrix(unlist(yrs.file), ncol=length(yrs.file[[1]]), byrow=T)
      yrs.file <- as.numeric(yrs.file[,ncol(yrs.file)-1]) # Assumes year is always last thing before the file extension
      
      if(!is.null(yrs.train)){
        files.train <- files.train[which(yrs.file %in% yrs.train)]
        yrs.file <- yrs.file[which(yrs.file %in% yrs.train)]
      }
      
      
      # Loop through the .nc files putting everything into a list
      dat.ens <- list() # Making a temporary storage bin for all the data from this ensemble member
      for(i in 1:length(files.train)){
        yr.now <- yrs.file[i]
        
        ncT <- ncdf4::nc_open(file.path(train.path, ens.train[j], files.train[i]))
        
        # Set up the time data frame to help index
        nday <- ifelse(lubridate::leap_year(yr.now), 366, 365)
        ntime <- length(ncT$dim$time$vals)
        step.day <- nday/ntime
        step.hr  <- step.day*24
        stamps.hr <- seq(step.hr/2, by=step.hr, length.out=1/step.day) # Time stamps centered on period
        
        # Create a data frame with all the important time info
        # center the hour step
        # ** Only do this with the first ensemble member so we're not being redundant
        if(j==1){
          df.time <- data.frame(Year=yr.now, DOY=rep(1:nday, each=1/step.day), Hour=rep(stamps.hr, length.out=ntime))
          df.time$Date <- strptime(paste(df.time$Year, df.time$DOY, df.time$Hour, sep="-"), format=("%Y-%j-%H"), tz="UTC")
          met.out$dat.train[["time"]] <- rbind(met.out$dat.train$time, df.time)
        }
        
        # Extract the met info, making matrices with the appropriate number of ensemble members
        for(v in names(ncT$var)){
          dat.ens[[v]] <- append(dat.ens[[v]], ncdf4::ncvar_get(ncT, v)) 
        }
        ncdf4::nc_close(ncT)

        if(print.progress==TRUE){
          setTxtProgressBar(pb, pb.ind)
          pb.ind <- pb.ind+1
        }
      } # End looping through training data files
      
      # Storing the ensemble member data in our output list
      for(v in names(dat.ens)){
        met.out$dat.train[[v]] <- cbind(met.out$dat.train[[v]], dat.ens[[v]])
      }
    } # End extracting ensemble members
    for(v in 2:length(met.out$dat.train)){
      dimnames(met.out$dat.train[[v]])[[2]] <- ens.train
    }
  } # End loading & formatting training data
  if(print.progress==TRUE) print(" ")
  # ---------------
  
  # ---------------
  # Read in & format the source data
  # ---------------
  met.out[["dat.source"]] <- list()
  if(length(dir(source.path, ".nc"))>0){ # we have a single time series
    n.src = 1 # we only have 1 time series so
    
    # Get a list of the files we'll be downscaling
    files.source <- dir(source.path, ".nc")
    
    # create a vector of the years
    yrs.file <- strsplit(files.source, "[.]")
    yrs.file <- matrix(unlist(yrs.file), ncol=length(yrs.file[[1]]), byrow=T)
    yrs.file <- as.numeric(yrs.file[,ncol(yrs.file)-1]) # Assumes year is always last thing before the file extension
    
    # Subsetting to just the years we're interested in
    if(!is.null(yrs.source)){
      files.source <- files.source[which(yrs.file %in% yrs.source)]
      yrs.file <- yrs.file[which(yrs.file %in% yrs.source)]
    }
    
    
    # Getting the day & hour timesteps from the training data
    yrs.train <- length(unique(met.out$dat.train$time$Year))
    hr.train  <- 24/length(unique(met.out$dat.train$time$Hour))
    day.train <- 1/length(unique(met.out$dat.train$time$Hour))
    # day.train <- 1/(nrow(met.out$dat.train$time)/yrs.train/365)
  
    # Loop through the .nc files putting everything into a list
    if(print.progress==TRUE){
      print("Processing Source Data")
      pb <- txtProgressBar(min=0, max=length(files.source), style=3)
    }
    
    for(i in 1:length(files.source)){
      yr.now <- yrs.file[i]
      
      ncT <- ncdf4::nc_open(file.path(source.path, files.source[i]))

      # Set up the time data frame to help index
      nday <- ifelse(leap_year(yr.now), 366, 365)
      ntime <- length(ncT$dim$time$vals)
      step.day <- nday/ntime
      step.hr  <- step.day*24
      
      # -----
      # Making time stamps to match the training data
      # For coarser time step than the training data, we'll duplicate in the loop
      # -----
      # Making what the unique time stamps should be to match the training data
      stamps.hr <- seq(hr.train/2, by=hr.train, length.out=1/day.train) 
      stamps.src <- seq(step.hr/2, by=step.hr, length.out=1/step.day) 
     
      if(step.hr < hr.train){  # Finer hour increment --> set it up to aggregate
        align = "aggregate"
        stamps.src <- rep(stamps.hr, each=24/step.hr)
      } else if(step.hr > hr.train) { # Set the flag to duplicate the data
        align = "repeat"
      } else { # things are aligned, so we're fine
        align = "aligned"
      }
      # -----
      
      # Create a data frame with all the important time info
      # center the hour step
      df.time <- data.frame(Year=yr.now, DOY=rep(1:nday, each=1/day.train), Hour=rep(stamps.hr, length.out=nday/(day.train)))
      df.time$Date <- strptime(paste(df.time$Year, df.time$DOY, df.time$Hour, sep="-"), format=("%Y-%j-%H"), tz="UTC")
      met.out$dat.source[["time"]] <- rbind(met.out$dat.source$time, df.time)
  
      src.time <- data.frame(Year=yr.now, DOY=rep(1:nday, each=1/step.day), Hour=rep(stamps.src, length.out=ntime))
      src.time$Date <- strptime(paste(src.time$Year, src.time$DOY, src.time$Hour, sep="-"), format=("%Y-%j-%H"), tz="UTC")
      
      # Extract the met info, making matrices with the appropriate number of ensemble members
      for(v in names(ncT$var)){
        dat.tem <- ncvar_get(ncT, v)
        
        if(align=="repeat"){ # if we need to coerce the time step to be repeated to match temporal resolution, do it here
          dat.tem <- rep(dat.tem, each=length(stamps.hr))
        }
        df.tem <- matrix(rep(dat.tem, n.src), ncol=n.src, byrow=F)
        
        # If we need to aggregate the data to align it, do it now to save memory
        if(align == "aggregate"){
          df.tem <- cbind(src.time, data.frame(df.tem))
  
          df.agg <- aggregate(df.tem[,(4+1:n.src)], by=df.tem[,c("Year", "DOY", "Hour")], FUN=mean)
          met.out$dat.source[[v]] <- rbind(met.out$dat.source[[v]], as.matrix(df.agg[,(3+1:n.src)]))
          
          # if workign wiht air temp, also find the max & min
          if(v=="air_temperature"){
            tmin <- aggregate(df.tem[,(4+1:n.src)], by=df.tem[,c("Year", "DOY", "Hour")], FUN=min)
            tmax <- aggregate(df.tem[,(4+1:n.src)], by=df.tem[,c("Year", "DOY", "Hour")], FUN=max)
            
            met.out$dat.source[["air_temperature_minimum"]] <- rbind(met.out$dat.source[["air_temperature_minimum"]], as.matrix(tmin[,(3+1:n.src)]))
            met.out$dat.source[["air_temperature_maximum"]] <- rbind(met.out$dat.source[["air_temperature_maximum"]], as.matrix(tmax[,(3+1:n.src)]))
          } 
        } else {
          met.out$dat.source[[v]] <- rbind(met.out$dat.source[[v]], as.matrix(df.tem, ncol=1))
        }
        
      }
      ncdf4::nc_close(ncT)
      if(print.progress==TRUE) setTxtProgressBar(pb, i)
    } # End looping through source met files
    if(print.progress==TRUE) print("")
  } else { # we have an ensemble we need to deal with
    ens.source <- dir(source.path)
    
    # If we're matching ensemble members need to use the same ones as from the training data
    if(pair.mems==TRUE){
      if(length(ens.source) < ens.train) stop("Cannot pair ensemble members. Reset pair.mems to FALSE or check your file paths")
      
      ens.source <- ens.source[train.use]
    } else {
      # Figure out whether or not we need to subsample or repeat ensemble members
      if(length(ens.source)>=n.ens){
        source.use <- sample(1:length(ens.source), n.ens)
      } else {
        source.use <- sample(1:length(ens.source), n.ens, replace = TRUE)
      }
      
      ens.source <- ens.source[source.use]
    }
    n.src = 1 # Potential to redo places where n.src is currently; this is based on out-dated code
    
    # getting an estimate of how many files we need to process
    n.files <- length(dir(file.path(source.path, ens.source[1])))
    
    if(print.progress==TRUE){
      print("Processing Source Data")
      pb <- txtProgressBar(min=0, max=length(ens.source)*n.files, style=3)
      pb.ind=1
    }
    for(j in 1:length(ens.source)){
      # Get a list of the files we'll be downscaling
      files.source <- dir(file.path(source.path, ens.source[j]), ".nc")
      
      # create a vector of the years
      yrs.file <- strsplit(files.source, "[.]")
      yrs.file <- matrix(unlist(yrs.file), ncol=length(yrs.file[[1]]), byrow=T)
      yrs.file <- as.numeric(yrs.file[,ncol(yrs.file)-1]) # Assumes year is always last thing before the file extension
      
      # Subsetting to just the years we're interested in
      if(!is.null(yrs.source)){
        files.source <- files.source[which(yrs.file %in% yrs.source)]
        yrs.file <- yrs.file[which(yrs.file %in% yrs.source)]
      }
      
      # Getting the day & hour timesteps from the training data
      day.train <- round(365/length(unique(met.out$dat.train$time$DOY)))
      hr.train  <- 24/length(unique(met.out$dat.train$time$Hour))
      
      # Loop through the .nc files putting everything into a list
      dat.ens <- list()
      for(i in 1:length(files.source)){
        yr.now <- yrs.file[i]
        
        ncT <- nc_open(file.path(source.path, ens.source[j], files.source[i]))
        
        # Set up the time data frame to help index
        nday <- ifelse(leap_year(yr.now), 366, 365)
        ntime <- length(ncT$dim$time$vals)
        step.day <- nday/ntime
        step.hr  <- step.day*24
        
        # -----
        # Making time stamps to match the training data
        # For coarser time step than the training data, we'll duplicate in the loop
        # -----
        # Making what the unique time stamps should be to match the training data
        stamps.hr <- seq(hr.train/2, by=hr.train, length.out=1/day.train) 
        stamps.src <- seq(step.hr/2, by=step.hr, length.out=1/step.day) 
        
        if(step.hr < hr.train){  # Finer hour increment --> set it up to aggregate
          align = "aggregate"
          stamps.src <- rep(stamps.hr, each=24/step.hr)
        } else if(step.hr > hr.train) { # Set the flag to duplicate the data
          align = "repeat"
        } else { # things are aligned, so we're fine
          align = "aligned"
        }
        # -----
        
        
        # Create a data frame with all the important time info
        # center the hour step
        df.time <- data.frame(Year=yr.now, DOY=rep(1:nday, each=1/day.train), Hour=rep(stamps.hr, length.out=nday/(day.train)))
        df.time$Date <- strptime(paste(df.time$Year, df.time$DOY, df.time$Hour, sep="-"), format=("%Y-%j-%H"), tz="UTC")

        # Create a data frame with all the important time info
        # center the hour step
        # ** Only do this with the first ensemble member so we're not being redundant
        if(j==1){
          met.out$dat.source[["time"]] <- rbind(met.out$dat.source$time, df.time)
        }
        
        src.time <- data.frame(Year=yr.now, DOY=rep(1:nday, each=1/step.day), Hour=rep(stamps.src, length.out=ntime))
        src.time$Date <- strptime(paste(src.time$Year, src.time$DOY, src.time$Hour, sep="-"), format=("%Y-%j-%H"), tz="UTC")
        
        # Extract the met info, making matrices with the appropriate number of ensemble members
        for(v in names(ncT$var)){
          dat.tem <- ncvar_get(ncT, v)
          
          if(align=="repeat"){ # if we need to coerce the time step to be repeated to match temporal resolution, do it here
            dat.tem <- rep(dat.tem, each=stamps.hr)
          }
          df.tem <- matrix(rep(dat.tem, n.src), ncol=1, byrow=F)
          
          # If we need to aggregate the data to align it, do it now to save memory
          if(align == "aggregate"){
            df.tem <- cbind(src.time, data.frame(df.tem))
            
            df.agg <- aggregate(df.tem[,(4+1:n.src)], by=df.tem[,c("Year", "DOY", "Hour")], FUN=mean)
            dat.ens[[v]] <- rbind(dat.ens[[v]], as.matrix(df.agg[,(3+1:n.src)]))
            
            # if working with air temp, also find the max & min
            if(v=="air_temperature"){
              tmin <- aggregate(df.tem[,(4+1:n.src)], by=df.tem[,c("Year", "DOY", "Hour")], FUN=min)
              tmax <- aggregate(df.tem[,(4+1:n.src)], by=df.tem[,c("Year", "DOY", "Hour")], FUN=max)
              
              dat.ens[["air_temperature_minimum"]] <- rbind(dat.ens[["air_temperature_minimum"]], as.matrix(tmin[,(3+1:n.src)]))
              dat.ens[["air_temperature_maximum"]] <- rbind(dat.ens[["air_temperature_maximum"]], as.matrix(tmax[,(3+1:n.src)]))
            } 
          } else {
            dat.ens[[v]] <- rbind(dat.ens[[v]], as.matrix(df.tem, ncol=1))
          }
          
        } #End variable loop
        nc_close(ncT)
        if(print.progress==TRUE){
          setTxtProgressBar(pb, pb.ind)
          pb.ind <- pb.ind+1
        }
      } # End looping through source met files
      
      # Storing the ensemble member data in our output list
      for(v in names(dat.ens)){
        met.out$dat.source[[v]] <- cbind(met.out$dat.source[[v]], dat.ens[[v]])
      }
    } # End loading & formatting source ensemble members
    
    # Storing info about the ensemble members
    for(v in 2:length(met.out$dat.source)){
      dimnames(met.out$dat.source[[v]])[[2]] <- ens.source
    }
    
  } # End loading & formatting source data
  if(print.progress==TRUE) print("")
  # ---------------
  
  
  return(met.out)
}
  