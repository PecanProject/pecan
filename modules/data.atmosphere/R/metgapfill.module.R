.metgapfill.module <- function(cf.id, register, dir, met, str_ns, site, new.site, con, 
                               start_date, end_date, host, overwrite = FALSE) {
  PEcAn.logger::logger.info("Gapfilling")  # Does NOT take place on browndog!
  
  input.id   <- cf.id[1]
  outfolder  <- file.path(dir, paste0(met, "_CF_gapfill_site_", str_ns))
  pkg        <- "PEcAn.data.atmosphere"
  fcn        <- "metgapfill"
  # fcn <- register$gapfill
  formatname <- "CF Meteorology"
  mimetype   <- "application/x-netcdf"
  lst        <- site.lst(site.id=site$id, con=con)
  
  ready.id <- PEcAn.utils::convert.input(input.id = input.id, 
                            outfolder = outfolder, 
                            formatname = formatname, 
                            mimetype =  mimetype, 
                            site.id = site$id, 
                            start_date = start_date, end_date = end_date, 
                            pkg = pkg, fcn = fcn, con = con, host = host, browndog = NULL,
                            write = TRUE, 
                            lst = lst, 
                            overwrite = overwrite,
                            exact.dates = FALSE)
  
  print(ready.id) # Not a debugging statement
  
  PEcAn.logger::logger.info("Finished Gapfilling Met")
  
  return(ready.id)
} # .metgapfill.module
