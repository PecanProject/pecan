##' @name sample_ic
##' @title sample_ic
##' 
##' @param in.path path to folder of the file to be sampled
##' @param in.name file name of the file to be sampled
##' @param start_date date in "YYYY-MM-DD" format
##' @param end_date date in "YYYY-MM-DD" format
##' @param outfolder dir path, whete to write the file
##' @param n.ensemble integer, ensemble member number
##' @param machine_host localhost name, e.g. "pecan2.bu.edu"
##' @param source string to appear in file names, e.g. "PalEON"
##' @param bin_var variable you would like to sample by, DEFAULT is DBH
##' @param bin_size bin size for sampling, DEFAULT is 10
##' @param ... Other inputs
##' 
##' @export
##' @author Istem Fer
sample_ic <- function(in.path, in.name, start_date, end_date, outfolder,
                      n.ensemble, machine_host, source, bin_var = "DBH", bin_size = 10, ...){
  
  
  #--------------------------------------------------------------------------------------------------#
  # Read
  rds_file <- file.path(in.path, in.name)
  veg_info <- readRDS(rds_file) 
  
  
  #--------------------------------------------------------------------------------------------------#
  # Prepare for sampling
  # NOTE: This function might call different functions in the future, e.g. : sample_cohort, sample_pool, or both
  # Then, the rest of the script would change, this is cohort-based only
  
  # 1st sublist is either NULL or has metadata (e.g. age, area), in the future we might want to sample over that too
  obs <- as.data.frame(veg_info[[2]], stringsAsFactors = FALSE)
  
  year <- lubridate::year(start_date)
  
  # subset samples for the year 
  samples <- obs[obs$year == year, ]
  
  # remove rows with NAs (we don't want DBH to be NA but do we want to allow missing taxa?)
  samples <- samples[complete.cases(samples), ]
  
  # if there are subplots, sample within each subplot instead of pooling all together, maybe pass down a flag if we want to pool anyway
  if(!is.null(samples$Subplot)){
    n.subplot <- length(unique(samples$Subplot))
    subplot.n <- unique(samples$Subplot)
  }else{
    n.subplot <- 1
    samples$Subplot <- 1
    subplot.n <- 1
  }
  sppfilename <- rep(NA, n.ensemble)
  #--------------------------------------------------------------------------------------------------#
  # Sampling 
  #loop over ensemble members
  for (ens in 1:n.ensemble) {
  
  sub.list <- list()
  veg_ens <- veg_info
  for(np in seq_len(n.subplot)){
    samples_sub <- samples[samples$Subplot == subplot.n[np],] 
    
    ############################################
    #  
    #  samples_sub is in this format (there are other columns):
    # 
    #   DBH  MCMC_iteration ... Tree_number ...
    #    36               1 ...           1 ...
    #  16.1               1 ...           2 ...
    #    24               1 ...           3 ...
    #   ...             ... ...         ... ...
    #  36.5               2 ...           1 ...
    #  16.2               2 ...           2 ...
    #   ...             ... ...         ... ...
    #  35.9            1000 ...           1 ...
    #    16            1000 ...           2 ...
    #   ...             ... ...         ... ...
    #   6.8            1000 ...         170 ...   
    #
    #  we can use Tree_number as the index for tapply and sample 1 from MCMC samples
    if (!is.null(samples_sub$Tree_number)) {
      samp_ind    <- tapply(seq_along(samples_sub$Tree_number), samples_sub$Tree_number, sample, 1)
      
    } else {
      #don't have MCMC samples, instead re-sample trees stratified by size
      #not every dataset will call DBH DBH, and 10 should be a variable. Add parameter for bin_size and bin_var with defaults set to DBH and 10 
      size <- ceiling(samples_sub[,which(colnames(samples_sub)==bin_var)]/bin_size)
      samp_ind <- unlist(tapply(seq_along(size), size, function(x){sample(x, length(x), replace = TRUE)}, simplify = TRUE))
    }
    sub_samp    <- samples_sub[samp_ind,]
    
    sub.list[[np]]  <- sub_samp
  }
  
    veg_ens[[2]] <- do.call("rbind", sub.list)
  
  
  
  #--------------------------------------------------------------------------------------------------#
  # Write vegettion data as rds, return results to convert.input
  
  # write with ensemble number
  sppfilename[ens] <- write_veg(outfolder, start_date, veg_info = veg_ens, paste0(source, "_ens", ens))
  }
  # Build results dataframe for convert.input
  results <- data.frame(file = sppfilename, 
                        host = machine_host, 
                        mimetype = "application/rds", 
                        formatname = "spp.info", 
                        startdate = start_date, 
                        enddate = end_date, 
                        dbfile.name = basename(sppfilename), 
                        stringsAsFactors = FALSE)
  
  ### return for convert.inputs
  return(invisible(results))  
  
  
} # sample_ic
