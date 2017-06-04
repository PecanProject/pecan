.get.veg.module <- function(input_veg, 
                            outfolder,
                            start_date, end_date,
                            dbparms,
                            new_site, 
                            host, machine_host,
                            overwrite){

  #--------------------------------------------------------------------------------------------------#
  # Extract/load data : this step requires DB connections 
  # can be passed to convert.inputs now because process IC locally
  
  lat       <- new_site$lat
  lon       <- new_site$lon
  site_id   <- new_site$id
  site_name <- new_site$name   
  
  ## Prepare to call convert.inputs
  pkg  <- "PEcAn.data.land"
  bety <- dplyr::src_postgres(dbname   = dbparms$bety$dbname, 
                              host     = dbparms$bety$host, 
                              user     = dbparms$bety$user, 
                              password = dbparms$bety$password)
  con  <- bety$con
  
  if(input_veg$source == "FIA"){
    
    fcn <- "extract_FIA"
    
    getveg.id <- convert.input(input.id = NA,
                               outfolder = outfolder, 
                               formatname = "spp.info", 
                               mimetype = "application/rds",
                               site.id = site_id, 
                               start_date = start_date, end_date = end_date, 
                               pkg = pkg, fcn = fcn, 
                               con = con, host = host, browndog = NULL, 
                               write = TRUE, 
                               overwrite = overwrite, 
                               # fcn specific args 
                               lon = lon, lat = lat, 
                               site_name = site_name,
                               gridres = 0.075, dbparms = dbparms,
                               machine_host = machine_host,
                               source = input_veg$source)
    
    return(getveg.id)
    
  }else{
    
    fcn <- "load_veg"
    
    if(!is.null(input_veg$source.id)){
      source.id <- input_veg$source.id
    }else{
      logger.error("Must specify input source.id")
    }
    
    getveg.id <- convert.input(input.id = NA,
                               outfolder = outfolder, 
                               formatname = "spp.info", 
                               mimetype = "application/rds",
                               site.id = site_id, 
                               start_date = start_date, end_date = end_date, 
                               pkg = pkg, fcn = fcn, 
                               con = con, host = host, browndog = NULL, 
                               write = TRUE, 
                               overwrite = overwrite, 
                               # fcn specific args 
                               new_site,
                               source_id = source.id, 
                               format_name = input_veg$match.format,
                               dbparms = dbparms,
                               machine_host = machine_host,
                               source = input_veg$source)
    
    return(getveg.id)
    
  }


} # get.veg.module



