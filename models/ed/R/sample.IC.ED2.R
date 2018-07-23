##' Samples intial conditions for ED2
##' 
##' NOTE: almost all of the fcn args can be extracted from pecan settings list, just wnated to be explicit
##' 
##' @param samples a dataframe with columns: tree id, spp info, Measurement_Year, DBH, plot id (if applicable), could come from load_data
##' @param format_name how the spp info provided: usda, fia, latin_name, custom
##' @param start_date settings$run$start.date
##' @param pfts settings$pfts
##' @param ne number of ensembles requested
##' @param latitude settings$run$site$lat
##' @param longitude settings$run$site$lon
##' @param source the source of the samples, will appear as prefix in the IC filenames e.g. "HF_lyford.PalEON"
##' @param metadata any known metadata: age, area (to calculate density) etc.
##' @param outfolder settings$rundir
##' @param host_info host info, e.g. settings$host
##' @param inputs_path dir path to inputs, e.g. dirname(settings$run$inputs$css$path)
##' 
##' @return IC 
##' 
##' @author Istem Fer
##' @export
##' 
sample.IC.ED2 <- function(samples, format_name = "usda", start_date, pfts, ne, 
                          latitude, longitude, source, metadata, outfolder, host_info, inputs_path) {
  

  
  # developing code
  colnames(samples) <- c("tree", "Measurement_Year", "iter", "DBH", "plot")

  year <- lubridate::year(start_date)-1 # year before the initial run
  
  # subset samples for the year, if coming from load_data won't need this
  samples <- samples[samples$Measurement_Year == year, ]
  
  # remove rows with NAs (we don't want DBH to be NA but do we want to allow missing taxa?)
  samples <- samples[complete.cases(samples), ]

  # dummy taxa until I get from Andria
  samples$usda <- samples$plot
  spp <- c("ACRU", "QURU", "BEAL2", "FAGR", "TSCA", "BELE", "PIST")
  spp_codes <- rep(spp, length.out= length(unique(samples$tree)))
  for(i in 1:length(unique(samples$tree))){
    ind <- samples$tree == unique(samples$tree)[i]
    samples$usda[ind] <- spp_codes[i]
  }
  
  # match spp info with species names 
  # the matching can go into the ic-loop below, depending which is easier to handle sample size vs ensemble size
  # for sample sizes of 10^5 this is still fast without sampling
  spp_match <- match_species_id(input_codes = samples[[format_name]], format_name = format_name)
  
  # match pfts
  pft_match   <- match_pft(bety_species_id = spp_match$bety_species_id, pfts = pfts) 
  samples$pft <- pft_match$pft
  
  # how many pathces we want? should "n.patches" be a function argument?
  if(!is.null(samples$plot)){
    n.patches <- length(unique(samples$plot))
  }else{
    n.patches <- 1
  }
  
  ic.list <- list()
  for(icn in seq_len(ne)){
    
    veg_info <- list()
    
    # prepare info for css/pss/site
    css_info    <- list()
    pss_info    <- list()
    # site_info <- list()
    
    # css
    patch.list <- list()
    for(np in seq_len(n.patches)){
      patch_sub  <- samples[samples$plot == np,] # these colnames might need generalization
      samp_ind   <- tapply(seq_along(patch_sub$tree), patch_sub$tree, sample, 1)
      patch_samp <- patch_sub[samp_ind,]
      
      patch_samp$patch  <- np
      patch_samp$year   <- year
      patch_samp$cohort <- 1:nrow(patch_samp) # EACH TREE ITS OWN COHORT?
      if(!is.null(metadata$area)){
        patch_samp$n <- nrow(patch_samp)/metadata$area
      }
      patch.list[[np]]  <- patch_samp
    }
    
    css_info <- do.call("rbind", patch.list)
    
    
    # pss
    pss_info$n.patch <- seq_len(n.patches)
    pss_info$time    <- rep(year, n.patches) 

    # site, do nothing
    # unless we want something different than the default in veg2model.ED2
    # then veg2model.ED2 also needs tweaking
    
    veg_info[[1]]       <- pss_info
    veg_info[[2]]       <- css_info
    # veg_info[["site"]]    <- site_info 
    
    
    new_site <- list()
    new_site[["latitude"]]  <- latitude
    new_site[["longitude"]] <- longitude
    
    ic.list[[icn]] <- veg2model.ED2(outfolder, veg_info, start_date, new_site, paste0(source, "_ens", icn))
    localfiles     <- paste0(sub("(.*)[.].*", "\\1", ic.list[[icn]]$filepath), c(".css", ".pss", ".site"))
    remotefiles    <- paste0(inputs_path, "/", sub("(.*)[.].*", "\\1", ic.list[[icn]]$filename), c(".css", ".pss", ".site"))
    # copy to remote and delete local
    for(fls in seq_along(localfiles)){
      PEcAn.remote::remote.copy.to(host_info, localfiles[fls], remotefiles[fls])
      file.remove(localfiles[fls])
    }
    
    ic.list[[icn]] <- data.frame(css = remotefiles[1], pss = remotefiles[2], site = remotefiles[3], stringsAsFactors = FALSE)
    
  }
  
  ic.list <- do.call("rbind", ic.list)
  
  return(ic.list)
} # sample.IC.ED2
