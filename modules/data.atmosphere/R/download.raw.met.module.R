.download.raw.met.module <- function(dir, met, register, machine, start_date, end_date, str_ns,
                                     con, input_met, site.id, lat.in, lon.in, host, site, username, overwrite = FALSE) {
  
  outfolder <- file.path(dir,paste0(met, "_site_", str_ns))
  
  pkg <- "PEcAn.data.atmosphere"
  fcn <- paste0("download.", met)
  
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
                            sitename = site$name, 
                            username = username)
    
  } else {
    PEcAn.logger::logger.severe("Unknown register$scale")
  }
  
  return(raw.id)
} # .download.raw.met.module
