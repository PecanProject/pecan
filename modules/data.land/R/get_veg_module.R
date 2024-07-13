##' Load/extract + match species module
##'
##' @param input_veg list, this is a sublist of settings$run$inputs that has info about source, id, metadata of the requested IC file
##' @param outfolder path to where the processed files will be written
##' @param start_date date in "YYYY-MM-DD" format, in case of source==FIA it's the settings$run$start.date, otherwise start_date of the IC file in DB
##' @param end_date date in "YYYY-MM-DD" format, in case of source==FIA it's the settings$run$end.date, otherwise end_date of the IC file in DB
##' @param dbparms list, settings$database info reqired for opening a connection to DB
##' @param new_site data frame, id/lat/lon/name info about the site
##' @param host list, host info as in settings$host, host$name forced to be "localhost" upstream
##' @param machine_host local machine hostname, e.g. "pecan2.bu.edu"
##' @param overwrite logical flag for convert_input
##' 
##' @export
##'
##' @author Istem Fer
get_veg_module <- function(input_veg,
                           outfolder,
                           start_date, end_date,
                           dbparms,
                           new_site,
                           host, machine_host,
                           overwrite){
  
  #--------------------------------------------------------------------------------------------------#
  # Extract/load data : this step requires DB connections
  # can be passed to convert_inputs now because process IC locally

  lat       <- new_site$lat
  lon       <- new_site$lon
  site_id   <- new_site$id
  site_name <- new_site$name
  ## Prepare to call convert_inputs
  pkg  <- "PEcAn.data.land"
  con <- PEcAn.DB::db.open(dbparms$bety)
  on.exit(PEcAn.DB::db.close(con), add = TRUE)
  
  # this check might change depending on what other sources that requires querying its own DB we will have
  if(input_veg$source == "FIA" | input_veg$source == "NEON_veg"){ 
    
    fcn <- "extract_veg"

    getveg.id <- PEcAn.DB::convert_input(
      input.id = NA,
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
      new_site = new_site,
      gridres = input_veg$gridres, dbparms = dbparms,
      machine_host = machine_host, input_veg = input,
      source = input_veg$source)
  
    
    return(getveg.id)
    
  }else{
    
    fcn <- "load_veg"
    if(!is.null(input_veg$id)){
      source.id <- input_veg$id
    }else{
      PEcAn.logger::logger.error("Must specify input id")
    }
    getveg.id <- PEcAn.DB::convert_input(
      input.id = NA,
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
      new_site = new_site,
      source_id = source.id,
      format_name = input_veg$match.format,
      dbparms = dbparms,
      machine_host = machine_host,
      source = input_veg$source,
      ##  any metadata passed via settings to be used in the IC files (in veg2model)
      ##  if different than defaults, e.g.:
      ##
      ##  <metadata>
      ##   <trk>2</trk>
      ##   <age>70</age>
      ##  </metadata>
      ##
      icmeta = input_veg$metadata)

    return(getveg.id)
    
  }
  
  
} # get.veg.module
