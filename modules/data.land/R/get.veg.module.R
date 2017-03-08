.get.veg.module <- function(dir = dir,
                            input_veg = input,
                            machine = machine, 
                            start_date = start_date, end_date = end_date,
                            str_ns = str_ns, bety = bety, dbparms = dbparms,
                            lat, lon,
                            host = host, localdb,
                            overwrite = overwrite$getveg,
                            site = site){
  
  outfolder <- file.path(dir, paste0(input_veg$source, "_site_", str_ns))

  #--------------------------------------------------------------------------------------------------#
  # Extract/load data : this step requires DB connections can't be handled by convert.inputs
  
  # which sources require load_data
  load_sources <- c("GapMacro", "NASA_FFT_Plot_Inventory", "NASA_TE_FIA")
  
  if(input_veg$source == "FIA"){
    
    veg_info <- extract_FIA(lon, lat, start_date, gridres = 0.075, dbparms)
    
  }else if(input_veg$source %in% load_sources){
    
    if(!is.null(input_veg$source.id){
      source.id <- input_veg$source.id
    }else{
      logger.error("Must specify input source.id")
    }
    
    # query data.path from source id [input id in BETY]
    query <- paste0("SELECT * FROM dbfiles where container_id = ", source.id)
    data.path <- file.path(db.query(query, con = con)[["file_path"]], db.query(query, con = con)[["file_name"]])
    
    # query format info
    format.vars <- query.format.vars(bety = bety, input.id = source.id)
    
    veg_info <- load_data(data.path, format, site = runinfo$site)
                     
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
  
  # IF: A hack to be able to use convert.input
  tmpfolder <- file.path(localdb, paste0(input_veg$source, "_site_", str_ns))
  dir.create(tmpfolder, showWarnings = F, recursive = T)
  
  now <- format(Sys.time(), "%Y%m%d%H%M%OS3")
  temp_file <- file.path(tmpfolder, paste0(now,".Rdata")) # to be deleted
  save(veg_info, file = temp_file)


  #--------------------------------------------------------------------------------------------------#
  # convert.inputs
  
  pkg <- "PEcAn.data.land"
  fcn <- "write_veg"
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
  
  
  return(raw.id)
  
}