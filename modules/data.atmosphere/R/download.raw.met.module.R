.download.raw.met.module <- function(dir, met, register, machine, start_date, end_date, str_ns,
                                     con, input_met, site.id, lat.in, lon.in, host, site, username, overwrite = FALSE) {
  
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
    raw.id <- PEcAn.utils::convert.input(input.id = NA,
                            outfolder = outfolder, 
                            formatname = register$format$name, 
                            mimetype = register$format$mimetype,
                            site.id = site.id, 
                            start_date = start_date, end_date = end_date, 
                            pkg = pkg, fcn = fcn, 
                            con = con, host = host, browndog = NULL,
                            write = TRUE, overwrite = overwrite, 
                            site_id = site.id, 
                            lat.in = lat.in, lon.in = lon.in, 
                            model = input_met$model, 
                            scenario = input_met$scenario, 
                            ensemble_member = input_met$ensemble_member,
                            pattern = met)
    
  } else if (register$scale == "site") {
    # Site-level met
    raw.id <- PEcAn.utils::convert.input(input.id = NA,
                            outfolder = outfolder, 
                            formatname = register$format$name, 
                            mimetype = register$format$mimetype,
                            site.id = site.id, 
                            start_date = start_date, end_date = end_date, 
                            pkg = pkg, 
                            fcn = fcn, 
                            con = con, host = host, browndog = NULL, 
                            write = TRUE, overwrite = overwrite, 
                            forecast = forecast,
                            ensemble = ensemble,
                            sitename = site$name, 
                            username = username,
                            lat.in = lat.in,
                            lon.in = lon.in,
                            pattern = met)
    
  } else {
    PEcAn.logger::logger.severe("Unknown register$scale")
  }
  
  return(raw.id)
} # .download.raw.met.module
