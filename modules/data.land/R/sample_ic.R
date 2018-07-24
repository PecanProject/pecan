##' @name sample_ic
##' @title sample_ic
##' @export
##' @author Istem Fer
sample_ic <- function(in.path, in.name, start_date, ensemble,
                      end_date, source = input_veg$source, overwrite = FALSE, ...){
  
  
  #--------------------------------------------------------------------------------------------------#
  # Read
  rds_file <- file.path(in.path, in.name)
  veg_info <- readRDS(rds_file) 
  
  #--------------------------------------------------------------------------------------------------#
  # Prepare for sampling
  obs <- as.data.frame(veg_info[[2]], stringsAsFactors = FALSE)
  
  year <- lubridate::year(start_date)
  
  # subset samples for the year 
  samples <- obs[obs$year == year, ]
  
  # remove rows with NAs (we don't want DBH to be NA but do we want to allow missing taxa?)
  samples <- samples[complete.cases(samples), ]
  
  # if there are subplots, sample within each subplot instead of pooling all together
  if(!is.null(samples$Subplot)){
    n.subplot <- length(unique(samples$Subplot))
  }else{
    n.subplot <- 1
    samples$Subplot <- 1
  }
  
  #--------------------------------------------------------------------------------------------------#
  # Sampling ensemble
  
  ic.list <- list()
  for(icn in seq_len(n.ensemble)){
    
    sub.list <- list()
    for(np in seq_len(n.subplot)){
      samples_sub  <- samples[samples$Subplot == np,] 
      samp_ind    <- tapply(seq_along(samples_sub$Tree_number), samples_sub$Tree_number, sample, 1)
      sub_samp    <- samples_sub[samp_ind,]
      
      sub.list[[np]]  <- sub_samp
    }
    
    ic.list[[icn]] <- <- do.call("rbind", sub.list)
    
  }
  
  
  veg_info[[2]] <- ic.list
  
  #--------------------------------------------------------------------------------------------------#
  # Write?
  
  # Build results dataframe for convert.input
  results <- data.frame(file = out$filepath, 
                        host = c(PEcAn.remote::fqdn()), 
                        mimetype = out$mimetype, 
                        formatname = out$formatname, 
                        startdate = start_date, 
                        enddate = end_date, 
                        dbfile.name = out$filename, 
                        stringsAsFactors = FALSE)
  
  ### return for convert.inputs
  return(invisible(results))
  
  
} # sample_ic
