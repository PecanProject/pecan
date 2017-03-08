.put.veg.module <- function(raw.id = raw.id,
                           dir = dir,
                           input_veg = input, 
                           machine = machine, 
                           start_date = start_date, end_date = end_date,
                           str_ns = str_ns, bety = bety, 
                           lat = new.site$lat, lon = new.site$lon,
                           host = host, localdb = settings$database$dbfiles,
                           overwrite = overwrite$getveg){
  
  #--------------------------------------------------------------------------------------------------#
  # Read
  
  #--------------------------------------------------------------------------------------------------#
  # Match PFTs
  
  pft.info <- match_pft(obs$bety_species_id, pfts, con)
  
  ### merge with other stuff
  obs <- cbind(obs, pft.info[c("bety_pft_id", "pft")])
  
  ### write
  file_name <- paste0("pftmatch.", start_year, "_", end_year, ".txt")
  
  # where to read file from in veg2model
  inputinfo$path <- file.path(file_path, file_name)
  
  
  #--------------------------------------------------------------------------------------------------#
  # convert.inputs : write model specific IC files
  
  ## Set model-specific functions
  pkg <- paste0("PEcAn.", model)
  do.call("library", list(pkg))
  fcn <- paste("veg2model.", model, sep = "")
  if (!exists(fcn)) {
    logger.severe(paste(fcn, "does not exist."))
  }
  
  con <- bety$con
  
  raw.id <- convert.input(input.id = NA,
                          outfolder = outfolder, 
                          formatname = "spp.info", 
                          mimetype = "text/plain",
                          site.id = site.id, 
                          start_date = start_date, end_date = end_date, 
                          pkg = pkg, fcn = fcn, 
                          con = con, host = host, browndog = NULL, 
                          write = TRUE, 
                          overwrite = overwrite, 
                          # fcn specific args 
                          temp_file)
  
}