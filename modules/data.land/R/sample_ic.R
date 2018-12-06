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
##' 
##' @export
##' @author Istem Fer
sample_ic <- function(in.path, in.name, start_date, end_date, outfolder,
                      n.ensemble, machine_host, source, ...){
  
  
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
  }else{
    n.subplot <- 1
    samples$Subplot <- 1
  }
  
  #--------------------------------------------------------------------------------------------------#
  # Sampling 
  
  sub.list <- list()
  for(np in seq_len(n.subplot)){
    samples_sub <- samples[samples$Subplot == np,] 
    
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
    samp_ind    <- tapply(seq_along(samples_sub$Tree_number), samples_sub$Tree_number, sample, 1)
    sub_samp    <- samples_sub[samp_ind,]
      
    sub.list[[np]]  <- sub_samp
  }
    
  veg_info[[2]] <- do.call("rbind", sub.list)
  
  #--------------------------------------------------------------------------------------------------#
  # Write vegettion data as rds, return results to convert.input
  
  # write with ensemble number
  sppfilename <- write_veg(outfolder, start_date, veg_info = veg_info, paste0(source, "_ens", n.ensemble))
  
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
