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
##' @param bin_herb_soil if we want to use bin size for both herb and soil sampling
##' @param ... Other inputs
##' 
##' @export
##' @author Istem Fer
sample_ic <- function(in.path, in.name, start_date, end_date, outfolder,
                      n.ensemble, machine_host, source, bin_var = "DBH", bin_size = 10, bin_herb_soil = TRUE, ...){
  
  
  #--------------------------------------------------------------------------------------------------#
  # Read
  rds_file <- file.path(in.path, in.name)
  veg_info <- readRDS(rds_file) 
  
  
  #--------------------------------------------------------------------------------------------------#
  # Prepare for sampling
  # NOTE: This function might call different functions in the future, e.g. : sample_cohort, sample_pool, or both
  # Then, the rest of the script would change, this is cohort-based only
    
    #--------------------------------------------------------------------------------------------------#
    # Sampling 
    #loop over ensemble members
    sppfilename <- rep(NA, n.ensemble)
    for (ens in 1:n.ensemble) {
      
      veg_ens <- veg_info
      
      #sample DBH
      if("DBH" %in% bin_var){
        bin_Var <- "DBH"
        obs <- as.data.frame(veg_info[[2]], stringsAsFactors = FALSE)
        
        year.start <- lubridate::year(start_date)
        year.end <- lubridate::year(end_date)
        
        # subset samples for the year 
        samples <- obs[obs$year >= year.start & obs$year <= year.end, ]
        
        # remove rows with NAs (we don't want DBH to be NA but do we want to allow missing taxa?)
        #samples <- samples[complete.cases(samples), ]
        samples <- samples[!is.na(samples[bin_Var]), ]
  
        # if there are subplots, sample within each subplot instead of pooling all together, maybe pass down a flag if we want to pool anyway
        if(!is.null(samples$Subplot)){
          n.subplot <- length(unique(samples$Subplot))
          subplot.n <- unique(samples$Subplot)
        }else{
          n.subplot <- 1
          samples$Subplot <- 1
          subplot.n <- 1
        }
        sub.list <- list()
        
        for(np in seq_len(n.subplot)){
          samples_sub <- samples[samples$Subplot == subplot.n[np],] 
          
          #  we can use Tree_number as the index for tapply and sample 1 from MCMC samples
          if (!is.null(samples_sub$Tree_number)) {
            samp_ind    <- tapply(seq_along(samples_sub$Tree_number), samples_sub$Tree_number, sample, 1)
            
          } else {
            #don't have MCMC samples, instead re-sample trees stratified by size
            #not every dataset will call DBH DBH, and 10 should be a variable. Add parameter for bin_size and bin_var with defaults set to DBH and 10 
            size <- ceiling(samples_sub[,which(colnames(samples_sub)==bin_Var)]/bin_size)
            samp_ind <- unlist(tapply(seq_along(size), size, function(x){sample(x, length(x), replace = TRUE)}, simplify = TRUE))
          }
          sub_samp    <- samples_sub[samp_ind,]
          
          sub.list[[np]]  <- sub_samp
        }
        veg_ens[[2]] <- do.call("rbind", sub.list)
      }
      
      #sample Herb
      if("dryMass" %in% bin_var){
        bin_Var <- "dryMass"
        obs <- as.data.frame(veg_info[[1]], stringsAsFactors = FALSE)
        
        year.start <- lubridate::year(start_date)
        year.end <- lubridate::year(end_date)
        
        # subset samples for the year 
        samples <- obs[obs$year >= year.start & obs$year <= year.end, ]
        
        # remove rows with NAs (we don't want DBH to be NA but do we want to allow missing taxa?)
        #samples <- samples[complete.cases(samples), ]
        samples <- samples[!is.na(samples[bin_Var]), ]
        
        # if there are subplots, sample within each subplot instead of pooling all together, maybe pass down a flag if we want to pool anyway
        if(!is.null(samples$plot)){
          n.plot <- length(unique(samples$plot))
          plot.n <- unique(samples$plot)
        }else{
          n.plot <- 1
          samples$plot <- 1
          plot.n <- 1
        }
        
        #if we are using bin_size to sample
        if(bin_herb_soil){
          if(n.plot>bin_size){
            plot_bin_size <- bin_size
          }else{
            plot_bin_size <- ceiling(n.plot/5)
          }
        }else{
          plot_bin_size <- 1
        }
        
        sub.list <- list()
        sample_plots <- sample(plot.n, plot_bin_size)
        for(np in 1:length(sample_plots)){
          samples_sub <- samples[samples$plot == sample_plots[np],] 
          sub.list[[np]]  <- samples_sub
        }
        veg_ens[[1]] <- do.call("rbind", sub.list)
      }
      
      #sample Soil Carbon
      if("SoilCarbon" %in% bin_var){
        bin_Var <- "SoilCarbon"
        obs <- as.data.frame(veg_info[[3]], stringsAsFactors = FALSE)
        
        year.start <- lubridate::year(start_date)
        year.end <- lubridate::year(end_date)
        
        # subset samples for the year 
        samples <- obs[obs$year >= year.start & obs$year <= year.end, ]
        
        # remove rows with NAs (we don't want DBH to be NA but do we want to allow missing taxa?)
        #samples <- samples[complete.cases(samples), ]
        samples <- samples[!is.na(samples[bin_Var]), ]
        
        # if there are subplots, sample within each subplot instead of pooling all together, maybe pass down a flag if we want to pool anyway
        if(!is.null(samples$plot)){
          n.plot <- length(unique(samples$plot))
          plot.n <- unique(samples$plot)
        }else{
          n.plot <- 1
          samples$plot <- 1
          plot.n <- 1
        }
        
        #if we are using bin_size to sample
        if(bin_herb_soil){
          if(n.plot>bin_size){
            plot_bin_size <- bin_size
          }else{
            plot_bin_size <- ceiling(n.plot/5)
          }
        }else{
          plot_bin_size <- 1
        }
        
        sub.list <- list()
        sample_plots <- sample(plot.n, plot_bin_size)
        for(np in 1:length(sample_plots)){
          samples_sub <- samples[samples$plot == sample_plots[np],] 
          sub.list[[np]]  <- samples_sub
        }
        veg_ens[[3]] <- do.call("rbind", sub.list)
      }
      #--------------------------------------------------------------------------------------------------#
      # Write vegetation data as rds, return results to convert_input
      
      # write with ensemble number
      sppfilename[ens] <- write_veg(outfolder, start_date, veg_info = veg_ens, paste0(source, "_ens", ens))
      
  }


  # Build results dataframe for convert_input
  results <- data.frame(file = sppfilename, 
                        host = machine_host, 
                        mimetype = "application/rds", 
                        formatname = "spp.info", 
                        startdate = start_date, 
                        enddate = end_date, 
                        dbfile.name = basename(sppfilename), 
                        stringsAsFactors = FALSE)
  
  ### return for convert_inputs
  return(invisible(results))  
  
  
} # sample_ic