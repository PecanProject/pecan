#' @name download.raw.met.module
#' @title download.raw.met.module
#' 
#' @return A list of data frames is returned containing information about the data file that can be used to locate it later.  Each
#' data frame contains information about one file.
#'
#' @param dir directory to write outputs to
#' @param met source included in input_met
#' @param register register.xml, provided by met.process
#' @param machine machine associated with hostname, provided by met.process
#' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
#' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
#' @param str_ns substitute for site_id if not provided, provided by met.process
#' @param con database connection based on dbparms in met.process
#' @param input_met Which data source to process
#' @param site.id site id
#' @param lat.in site latitude, provided by met.process
#' @param lon.in site longitude, provided by met.process
#' @param host host info from settings file
#' @param site site info from settings file
#' @param username database username
#' @param overwrite whether to force download.raw.met.module to proceed
#' @param dbparms database settings from settings file
#' @param Ens.Flag default set to FALSE 
#'
#' 
#' @export
#'
#'

.download.raw.met.module <-
  function(dir,
           met,
           register,
           machine,
           start_date,
           end_date,
           str_ns,
           con,
           input_met,
           site.id,
           lat.in,
           lon.in,
           host,
           site,
           username,
           overwrite = FALSE,
           dbparms,
           Ens.Flag=FALSE) {
    
  outfolder <- file.path(dir,paste0(met, "_site_", str_ns))
  
  pkg <- "PEcAn.data.atmosphere"
  fcn <- paste0("download.", met)
  
  #Some data products can be forecasts instead of real time data.  Others can be ensembles of data instead of a single source.  Some can be both.
  #Not all of the registration.xml files for each data source contains a <forecast> or <ensemble> tag; therefore, we must check for their 
  #existence first.
  forecast = FALSE
  ensemble = FALSE
  if (!is.null(register$forecast)) {
    forecast = as.logical(register$forecast)
  }
  if (!is.null(register$ensemble) && !is.na(as.integer(register$ensemble)) && as.integer(register$ensemble) > 1) {
    ensemble = as.integer(register$ensemble) #No ensembles is given by FALSE, while the presence of ensembles is given by the number of ensembles.
    ifelse(is.na(ensemble), FALSE, ensemble) #If ensemble happens to be a character vector or something it can't convert, as.integer will evaluate to NA.
  }
  
  if (register$scale == "regional") {
    raw.id <- PEcAn.DB::convert_input(
      input.id = NA,
      outfolder = outfolder,
      formatname = register$format$name,
      mimetype = register$format$mimetype,
      site.id = site.id,
      start_date = start_date,
      end_date = end_date,
      pkg = pkg,
      fcn = fcn,
      con = con,
      host = host,
      write = TRUE,
      overwrite = overwrite,
      site_id = site.id,
      lat.in = lat.in,
      lon.in = lon.in,
      model = input_met$model,
      scenario = input_met$scenario,
      ensemble_member = input_met$ensemble_member,
      method = input_met$method,
      pattern = met,
      dbparms=dbparms,
      ensemble = ensemble
    )
    
  } else if (register$scale == "site") {
    # Site-level met
    raw.id <- PEcAn.DB::convert_input(
      input.id = NA,
      outfolder = outfolder,
      formatname = register$format$name,
      mimetype = register$format$mimetype,
      site.id = site.id,
      start_date = start_date,
      end_date = end_date,
      pkg = pkg,
      fcn = fcn,
      con = con,
      host = host,
      write = TRUE,
      overwrite = overwrite,
      forecast = forecast,
      ensemble = ensemble,
      sitename = site$name,
      username = username,
      lat.in = lat.in,
      lon.in = lon.in,
      pattern = met, 
      site_id = site.id,
      product = input_met$product
    )
    
  } else {
    PEcAn.logger::logger.severe("Unknown register$scale")
  }
  
  return(raw.id)
} # .download.raw.met.module
