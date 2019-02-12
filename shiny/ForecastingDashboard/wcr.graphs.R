#### need to create a graph funciton here to call with the args of start time 

wcr.graphs <- function(args){ 
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
    workflow <- workflows[nrow(workflows),]
  } else {
    workflow <- workflows
  }
  
  
  print(paste0("Using workflow ", workflow$id))
  wid <- workflow$id
  pecan_out_dir <- paste0("/fs/data3/kzarada/output/PEcAn_", wid, "/out");
  pecan_out_dirs <- list.dirs(path = pecan_out_dir)
  if (is.na(pecan_out_dirs[1])) {
    print(paste0(pecan_out_dirs, " does not exist."))
  }
  neemat <- matrix(1:64, nrow=1, ncol=64) # Proxy row, will be deleted later.
  qlemat <- matrix(1:64, nrow=1, ncol=64) # Proxy row, will be deleted later.
  num_results <- 0;
  for (i in 2:length(pecan_out_dirs)) {
    datafile <- file.path(pecan_out_dirs[i], format(workflow$start_date, "%Y.nc"))
    if (!file.exists(datafile)) {
      print(paste0("File ", datafile, " does not exist."))
      next
    }
    
    num_results <- num_results + 1
    
    #open netcdf file
    ncptr <- ncdf4::nc_open(datafile);
    
    # Attach data to matricies
    nee <- ncdf4::ncvar_get(ncptr, "NEE")
    neemat <- rbind(neemat, nee)
    
    qle <- ncdf4::ncvar_get(ncptr, "Qle")
    qlemat <- rbind(qlemat, qle)
    
    # Close netcdf file
    ncdf4::nc_close(ncptr)
  }
  if (num_results == 0) {
    print("No results found.")
    quit("no")
  } else {
    print(paste0(num_results, " results found."))
  }
  # Strip away proxy rows
  neemat <- neemat[-1,]
  qlemat <- qlemat[-1,]
  # Time
  time <- seq(6, 6 * ncol(neemat), by=6)
  # Caluclate means
  neemins <- NULL
  neemaxes <- NULL
  quantiles <- apply(neemat,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
  neelower95 <- quantiles[1,]
  neemeans <- quantiles[2,]
  neeupper95 <- quantiles[3,]
  needf <- data.frame(time = time, Lower = neelower95, Predicted = neemeans, Upper = neeupper95)
  needf$date <- c(rep(as.Date(start_date), 3), rep(seq(as.Date(start_date) + lubridate::days(1), as.Date(workflow$end_date)-lubridate::days(1), by="days"), each = 4), as.Date(workflow$end_date))
  needf$Time <- c(6,12,18, rep(c(0,6,12,18),length.out = (length(needf$date) - 3)))
  needf$start_date <- rep(start_date, each = 64)
  
  quantiles <- apply(qlemat,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
  qlelower95 <- quantiles[1,]
  qlemeans <- quantiles[2,]
  qleupper95 <- quantiles[3,]
  qledf <- data.frame(time = time, Lower = qlelower95, Predicted = qlemeans, Upper = qleupper95)
  qledf$date <-  c(rep(as.Date(start_date), 3), rep(seq(as.Date(start_date) + lubridate::days(1), as.Date(workflow$end_date)-lubridate::days(1), by="days"), each = 4), as.Date(workflow$end_date))
  qledf$Time <- c(6,12,18, rep(c(0,6,12,18),length.out = (length(qledf$date) - 3)))
  qledf$start_date <- rep(start_date, each = 64)
  
  if(args[2] == "NEE"){
  return(needf)}
  else(return(qledf))
} 



