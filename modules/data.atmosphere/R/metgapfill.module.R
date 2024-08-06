.metgapfill.module <- function(cf.id, register, dir, met, str_ns, site, new.site, con, 
                               start_date, end_date, host, overwrite = FALSE, ensemble_name = NULL) {
  PEcAn.logger::logger.info("Gapfilling")
  
  input.id   <- cf.id[1]
  outfolder  <- file.path(dir, paste0(met, "_CF_gapfill_site_", str_ns))
  pkg        <- "PEcAn.data.atmosphere"
  fcn        <- "metgapfill"
  # fcn <- register$gapfill
  formatname <- "CF Meteorology"
  mimetype   <- "application/x-netcdf"
  lst        <- site.lst(site.id=site$id, con=con)
  
  if (!is.null(register$forecast)) {
    forecast <- isTRUE(as.logical(register$forecast))
  } else {
    forecast <- FALSE
  }
  
  # met products requiring special gapfilling functions (incompatable with metgapfill)
  # Overrides default value of "fcn"
  if (met %in% c("NOAA_GEFS")) {
    fcn <- "metgapfill.NOAA_GEFS"
  }
  
  ready.id <- PEcAn.DB::convert_input(input.id = input.id,
                            outfolder = outfolder, 
                            formatname = formatname, 
                            mimetype =  mimetype, 
                            site.id = site$id, 
                            start_date = start_date, end_date = end_date, 
                            pkg = pkg, fcn = fcn, con = con, host = host,
                            write = TRUE, 
                            lst = lst, 
                            overwrite = overwrite,
                            exact.dates = FALSE,
                            forecast = forecast,
                            pattern = met,
                            ensemble = !is.null(register$ensemble) && as.logical(register$ensemble),
                            ensemble_name = ensemble_name)
  
  print(ready.id)
  
  PEcAn.logger::logger.info("Finished Gapfilling Met")
  
  return(ready.id)
} # .metgapfill.module
