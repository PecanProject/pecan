#### need to create a graph funciton here to call with the args of start time 

forecast.graphs <- function(args){ 
  start_date <- tryCatch(as.POSIXct(args[1]), error = function(e) {NULL} )
  if (is.null(start_date)) {
    in_wid <- as.integer(args[1])
  }
  dbparms = list()
  dbparms$dbname = "bety"
  dbparms$host = "128.197.168.114"
  dbparms$user = "bety"
  dbparms$password = "bety"
  #Connection code copied and pasted from met.process
  bety <- dplyr::src_postgres(dbname   = dbparms$dbname, 
                              host     = dbparms$host, 
                              user     = dbparms$user, 
                              password = dbparms$password)
  con <- bety$con #Connection to the database.  dplyr returns a list.
  # Identify the workflow with the proper information
  if (!is.null(start_date)) {
    workflows <- PEcAn.DB::db.query(paste0("SELECT * FROM workflows WHERE start_date='", format(start_date, "%Y-%m-%d %H:%M:%S"), 
                                           "' ORDER BY id"), con)
  } else {
    workflows <- PEcAn.DB::db.query(paste0("SELECT * FROM workflows WHERE id='", in_wid, "'"), con)
  }
  print(workflows)
  
  workflows <- workflows[which(workflows$site_id == args[3]),]
  
  if (nrow(workflows) > 1) {
    workflow <- workflows[1,]
  } else {
    workflow <- workflows
  }
  
  
  print(paste0("Using workflow ", workflow$id))
  wid <- workflow$id
  outdir <- args[4]
  pecan_out_dir <- paste0(outdir, "PEcAn_", wid, "/out");
  pecan_out_dirs <- list.dirs(path = pecan_out_dir)
  if (is.na(pecan_out_dirs[1])) {
    print(paste0(pecan_out_dirs, " does not exist."))
  }
  
  
  #neemat <- matrix(1:64, nrow=1, ncol=64) # Proxy row, will be deleted later.
  #qlemat <- matrix(1:64, nrow=1, ncol=64)# Proxy row, will be deleted later.

  neemat <- vector()
  qlemat <- vector()
  soilmoist <- vector()
  time <- vector()
  
   num_results <- 0;
  for (i in 2:length(pecan_out_dirs)) {
    #datafile <- file.path(pecan_out_dirs[i], format(workflow$start_date, "%Y.nc"))
    datafiles <- list.files(pecan_out_dirs[i]) 
    datafiles <- datafiles[grep("*.nc$", datafiles)]
  
  if (length(datafiles) == 0) {
      print(paste0("File ", pecan_out_dirs[i], " does not exist."))
      next
    }
  
  if(length(datafiles) == 1){
    
    file = paste0(pecan_out_dirs[i],'/', datafiles[1])
    
    num_results <- num_results + 1
    
    #open netcdf file
    ncptr <- ncdf4::nc_open(file);
    
    # Attach data to matricies
    nee <- ncdf4::ncvar_get(ncptr, "NEE")
    if(i == 2){ neemat <- nee} else{neemat <- cbind(neemat,nee)}
    
    qle <- ncdf4::ncvar_get(ncptr, "Qle")
    if(i == 2){ qlemat <- qle} else{qlemat <- cbind(qlemat,qle)}
    
    soil <- ncdf4::ncvar_get(ncptr, "SoilMoistFrac")
    if(i == 2){ soilmoist <- soil} else{soilmoist <- cbind(soilmoist,soil)}
    
    sec <- ncptr$dim$time$vals
    origin <- strsplit(ncptr$dim$time$units, " ")[[1]][3]
   
    # Close netcdf file
    ncdf4::nc_close(ncptr)
  }

  if(length(datafiles) > 1){ 
    
    
    file = paste0(pecan_out_dirs[i],'/', datafiles[1])
    file2 = paste0(pecan_out_dirs[i],'/', datafiles[2])
    
    num_results <- num_results + 1

    #open netcdf file
    ncptr1 <- ncdf4::nc_open(file);
    ncptr2 <- ncdf4::nc_open(file2);
    # Attach data to matricies
    nee1 <- ncdf4::ncvar_get(ncptr1, "NEE")
    nee2 <- ncdf4::ncvar_get(ncptr2, "NEE")
    nee <- c(nee1, nee2)
    if(i == 2){ neemat <- nee} else{neemat <- cbind(neemat,nee)}
    
    qle1 <- ncdf4::ncvar_get(ncptr1, "Qle")
    qle2 <- ncdf4::ncvar_get(ncptr2, "Qle")
    qle <- c(qle1, qle2)
  
    if(i == 2){ qlemat <- qle} else{qlemat <- cbind(qlemat,qle)}
    
    soil1 <- ncdf4::ncvar_get(ncptr1, "SoilMoistFrac")
    soil2 <- ncdf4::ncvar_get(ncptr2, "SoilMoistFrac")
    soil <- c(soil1, soil2)
    if(i == 2){ soilmoist <- soil} else{soilmoist <- cbind(soilmoist,soil)}
  
    
    sec <- c(ncptr1$dim$time$vals,  ncptr2$dim$time$vals+ last(ncptr1$dim$time$vals)) 
    origin <- strsplit(ncptr1$dim$time$units, " ")[[1]][3]
  
    
    # Close netcdf file
    ncdf4::nc_close(ncptr1)
    ncdf4::nc_close(ncptr2)

    }
    
  }
   
  if (num_results == 0) {
    print("No results found.")
    quit("no")
  } else {
    print(paste0(num_results, " results found."))
  }

  # Time
  time <- seq(1, length.out= length(sec))
 
  
  # Caluclate means
  neemins <- NULL
  neemaxes <- NULL
  quantiles <- apply(neemat,1,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
  neelower95 <- quantiles[1,]
  neemeans <- quantiles[2,]
  neeupper95 <- quantiles[3,]
  needf <- data.frame(time = time, Lower = neelower95, Predicted = neemeans, Upper = neeupper95)
  needf$date <- as.Date(sec, origin = origin)
  #$needf$Time <- c(6,12,18, rep(c(0,6,12,18),length.out = (length(needf$date) - 3)))
  needf$start_date <- rep(start_date, each = length(sec))
  needf$Time <- round(abs(sec - floor(sec)) * 24)
    
    
    
    
  quantiles <- apply(qlemat,1,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
  qlelower95 <- quantiles[1,]
  qlemeans <- quantiles[2,]
  qleupper95 <- quantiles[3,]
  qledf <- data.frame(time = time, Lower = qlelower95, Predicted = qlemeans, Upper = qleupper95)
  qledf$date <-  as.Date(sec, origin = origin)
  #qledf$Time <- c(6,12,18, rep(c(0,6,12,18),length.out = (length(qledf$date) - 3)))
  qledf$start_date <- rep(start_date, each = length(sec))
  qledf$Time <- round(abs(sec - floor(sec)) * 24)
  
  
  #soil moisture
  soilmins <- NULL
  soilmaxes <- NULL
  quantiles <- apply(soilmoist,1,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
  soillower95 <- quantiles[1,]
  soilmeans <- quantiles[2,]
  soilupper95 <- quantiles[3,]
  soildf <- data.frame(time = time, Lower = soillower95, Predicted = soilmeans, Upper = soilupper95)
  soildf$date <- as.Date(sec, origin = origin)
  #$needf$Time <- c(6,12,18, rep(c(0,6,12,18),length.out = (length(needf$date) - 3)))
  soildf$start_date <- rep(start_date, each = length(sec))
  soildf$Time <- round(abs(sec - floor(sec)) * 24)
  
  
  
  if(args[2] == "NEE"){
  return(needf)}
  if(args[2]== "LE"){
    return(qledf)}
  else(return(soildf))
  
  PEcAn.DB::db.close(con)
} 



