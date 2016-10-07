.metgapfill.module <- function(cf.id, register, dir, met, str_ns, site, new.site, con, start_date, end_date, host, overwrite=FALSE) {
  logger.info("Gapfilling") # Does NOT take place on browndog!
  
  input.id   <- cf.id[1]
  outfolder  <- file.path(dir,paste0(met,"_CF_gapfill_site_",str_ns))
  pkg        <- "PEcAn.data.atmosphere"
  fcn        <- "metgapfill"
  #       fcn        <- register$gapfill
  formatname <- 'CF Meteorology'
  mimetype   <- 'application/x-netcdf'
  lst        <- site.lst(site,con)
  
  ready.id   <- convert.input(
    input.id, outfolder, formatname, mimetype, site.id=site$id,
    start_date, end_date, pkg, fcn, con=con, host=host, browndog=NULL,
    write=TRUE, lst=lst, overwrite=overwrite)
  
  print(ready.id)
  
  logger.info("Finished Gapfilling Met")
  
  return(ready.id)
}