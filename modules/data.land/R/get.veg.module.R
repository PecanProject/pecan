.get.veg.module <- function(dir = dir,
                            input_veg = input,
                            machine = machine, 
                            start_date = start_date, end_date = end_date,
                            str_ns = str_ns, con = con, dbparms = dbparms,
                            lat, lon,
                            host = host, 
                            overwrite = overwrite$getveg,
                            site = site){
  
  outfolder <- file.path(dir, paste0(input_veg$source, "_site_", str_ns))
  
  # which sources require load_data
  load_sources <- c("GapMacro", "NASA_FFT_Plot_Inventory", "NASA_TE_FIA")
  

  # determine function and function arguments
  if(input_veg$source == "FIA"){
    fcn <- "get_FIA"
    source.id <- NULL

    
  }else if(input_veg$source %in% load_sources){
    fcn <- "get_data"
    
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


                     
  }
  pkg <- "PEcAn.data.land"

  
  
  obs <- extract.FIA(inputinfo, lat, lon, year = start_year, dbparms)
  obs <- load_data(data.path, format, site = runinfo$site)

  get_data(input_veg, outfolder, ....)
  
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
                          # convert.input args end, fcn args begin
                          # I can't pass list as extra fcn arguments to convert.input
                          # need break and rebuild dbparms
                          lat = lat, lon = lon, source.id = source.id,
                          bety_usr = dbparms$bety$user, bety_pwd = dbparms$bety$password, 
                          bety_hst = dbparms$bety$host, bety_dbn = dbparms$bety$dbname, 
                          fia_usr = dbparms$fia$user, fia_pwd = dbparms$fia$password, 
                          fia_hst = dbparms$fia$host, fia_dbn = dbparms$fia$dbname)
  
  
  return(raw.id)
  
}