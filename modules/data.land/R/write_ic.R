##' @name write_ic
##' @title write_ic
##' 
##' @param in.path file path to rds file with IC data
##' @param in.name file name of IC data
##' @param start_date YYYY-MM-DD
##' @param end_date YYYY-MM-DD
##' @param outfolder Location to store function outputs
##' @param model BETY model ID
##' @param new_site Site info including lat, lon, and BETT site ID
##' @param pfts list settings$pfts. 
##' @param source Data source as saved in the BETY db
##' @param overwrite DEfault is FALSE. Option to overwrite existing files. 
##' @param n.ensemble number of ensemble members
##' @param ... Additional parameters
##' @param host.inputargs host info taken from settings object 
##'
##' @export
##' 
##' @author Istem Fer
write_ic <- function(in.path, in.name, start_date, end_date, 
                     outfolder, model, new_site, pfts,
                     source = input_veg$source, overwrite = FALSE, n.ensemble, host.inputargs, ...){
  
  
  #--------------------------------------------------------------------------------------------------#
  # Read
  rds_file <- file.path(in.path, in.name)
  veg_info <- readRDS(rds_file) 
  
  #--------------------------------------------------------------------------------------------------#
  # Match PFTs
  #revisit later need to fix species matching first
  obs <- as.data.frame(veg_info[[2]], stringsAsFactors = FALSE)
  # NOTE : match_pft may return NAs for unmatched dead trees
  pft.info <- PEcAn.data.land::match_pft(bety_species_id = obs$bety_species_id, pfts = pfts, model = model, con = NULL)

  # merge with other stuff
  obs$pft <- pft.info$pft
  
  veg_info[[2]] <- obs
  
  #--------------------------------------------------------------------------------------------------#
  # veg2model
  # Set model-specific functions
  pkg <- paste0("PEcAn.", model$type)
  do.call("library", list(pkg))
  fcnx <- paste("veg2model.", model$type, sep = "")
  if (!exists(fcnx)) {
    PEcAn.logger::logger.severe(paste(fcnx, "does not exist."))
  }else{
    fcn <- match.fun(fcnx)
  }
  # Cohort2Pool -------------------------------------------------------------
  # read in registration xml for pool specific information
  register.xml <- system.file(paste0("register.", model$type, ".xml"), package = paste0("PEcAn.", model$type))
  if(file.exists(register.xml)){
    register     <- XML::xmlToList(XML::xmlParse(register.xml))
    
  }else{
    PEcAn.logger::logger.warn("No model register file found")
  }
  #check if register,model.xml includes "POOL"
  if (register$initcond == "POOL") {
    poolinfo <- cohort2pool(dat = veg_info, allom_param = NULL, dbh_name = "DBH")
    siteid <- as.numeric(new_site$id)
    out <- fcn(outfolder, poolinfo, siteid, ens = n.ensemble)
    
  } else{
    out <- fcn(outfolder, veg_info, start_date, new_site, source, ens = n.ensemble)
    
  }
  # Build results dataframe for convert_input
  results <- data.frame(file = out$file, 
                        host = host.inputargs$name, 
                        mimetype = out$mimetype, 
                        formatname = out$formatname, 
                        startdate = start_date, 
                        enddate = end_date, 
                        dbfile.name = out$dbfile.name, 
                        stringsAsFactors = FALSE)
  
  
  ### return for convert_inputs
  return(invisible(results))
  
  
} # write_ic