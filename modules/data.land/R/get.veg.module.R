.get.veg.module <- function(input_veg, 
                            outfolder, tmpfolder,
                            start_date, end_date,
                            bety, dbparms,
                            lat, lon, site,
                            host, overwrite, machine){

  #--------------------------------------------------------------------------------------------------#
  # Extract/load data : this step requires DB connections can't be handled by convert.inputs
  
  # which sources require load_data
  load_sources <- c("GapMacro", "NASA_FFT_Plot_Inventory", "NASA_TE_FIA")
  
  if(input_veg$source == "FIA"){
    
    veg_info <- extract_FIA(lon, lat, start_date, gridres = 0.075, dbparms)
    
  }else if(input_veg$source %in% load_sources){
    
    if(!is.null(input_veg$source.id)){
      source.id <- input_veg$source.id
    }else{
      logger.error("Must specify input source.id")
    }
    
    # query data.path from source id [input id in BETY]
    query      <- paste0("SELECT * FROM dbfiles where container_id = ", source.id)
    input_file <- db.query(query, con = con)
    data_path  <- file.path(input_file[["file_path"]], input_file[["file_name"]])
    
    # query format info
    format <- query.format.vars(bety = bety, input.id = source.id)
    
    veg_info <- load_data(data.path = data_path, format, site)
                     
  }
  
  #--------------------------------------------------------------------------------------------------#
  # Match species : this step requires DB connections can't be handled by convert.inputs
  
  usda_sources <- c("GapMacro", "NASA_FFT_Plot_Inventory", "NASA_TE_FIA")
  
  # decide which code format to use while matching species
  # should we retrieve it from settings or assign a code format per source type?
  # or allow both?
  if(input_veg$source %in% usda_sources){
    format.name = 'usda'
  }else if(input_veg$source %in% c("FIA")){
    format.name = 'fia'      
  }else if(!is.null(input_veg$match.format)){
    format.name = input_veg$match.format
  }else{
    logger.error("Can't match code to species. No valid format found.")
  }
  
  # decide which column has the codes
  # this block may or may not be merged with the block above
  if(format.name == 'usda'){
    code.col = "species_USDA_symbol"
  }else if(format.name == 'fia'){
    code.col = "spcd"
  }else if(format.name == 'latin_name'){
    code.col = "latin_name"
  }
  
  obs <- veg_info[[2]]
  
  # match code to species ID
  spp.info <- match_species_id(input_codes = obs[[code.col]], format_name = format.name, bety = bety)
  
  # merge with data
  tmp <- spp.info[ , colnames(spp.info) != "input_code"]
  
  veg_info[[2]] <- cbind(obs, tmp)

  
  ### IF: A hack to be able to use convert.input ###
    now       <- format(Sys.time(), "%Y%m%d%H%M%OS3")
    temp_file_local <- file.path(tmpfolder, paste0(now,".Rdata")) # to be deleted below
    temp_file       <- file.path(outfolder, paste0(now,".Rdata")) # to be deleted in write_veg
    save(veg_info, file = temp_file_local)
    remote.copy.to(host, temp_file_local, temp_file)
    file.remove(temp_file_local)
  ### IF: A hack to be able to use convert.input ###


  #--------------------------------------------------------------------------------------------------#
  # convert.inputs
  
  pkg <- "PEcAn.data.land"
  fcn <- "write_veg"
  con <- bety$con
  
  raw.id <- convert.input(input.id = NA,
                          outfolder = outfolder, 
                          formatname = "spp.info", 
                          mimetype = "text/plain",
                          site.id = site$id, 
                          start_date = start_date, end_date = end_date, 
                          pkg = pkg, fcn = fcn, 
                          con = con, host = host, browndog = NULL, 
                          write = TRUE, 
                          overwrite = overwrite, 
                          # fcn specific args 
                          temp_file = temp_file)
  
  
  return(raw.id)
  
}