.extract.nc.module <- function(cf.id, register, dir, met, str_ns, site, new.site, con, 
                               start_date, end_date, host, overwrite = FALSE) {
  logger.info("Site Extraction")
  
  input.id <- cf.id[1]
  outfolder <- ifelse(host$name == "localhost", 
                      file.path(dir, paste0(met, "_CF_site_", str_ns)), 
                      file.path(host$dbfiles, paste0(met, "_CF_site_", str_ns)))
  pkg        <- "PEcAn.data.atmosphere"
  fcn        <- "extract.nc"
  formatname <- "CF Meteorology"
  mimetype   <- "application/x-netcdf"
  
  ready.id <- convert.input(input.id, 
                            outfolder, 
                            formatname, 
                            mimetype, 
                            site.id = site$id, 
                            start_date, end_date,
                            pkg, 
                            fcn, 
                            con = con, host = host, browndog = NULL, 
                            write = TRUE, 
                            slat = new.site$lat, slon = new.site$lon,
                            newsite = new.site$id, 
                            overwrite = overwrite)
  
  logger.info("Finished Extracting Met")
  
  return(ready.id)
} # .extract.nc.module
