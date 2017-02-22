.met2cf.module <- function(raw.id, register, met, str_ns, dir, machine, site.id, lat, lon, start_date, end_date, 
                           con, host, overwrite = FALSE, format.vars) {
  
  logger.info("Begin change to CF Standards")
  
  input.id   <- raw.id$input.id[1]
  pkg        <- "PEcAn.data.atmosphere"
  formatname <- "CF Meteorology"
  mimetype   <- "application/x-netcdf"
  format.id  <- 33
  
  if (register$scale == "regional") {
    input_name <- paste0(met, "_CF")
    outfolder <- file.path(dir, input_name)
    
    fcn1 <- paste0("met2CF.", met)
    mimename <- register$format$mimetype
    mimename <- substr(mimename, regexpr("/", mimename) + 1, nchar(mimename))
    mimename <- substr(mimename, regexpr("-", mimename) + 1, nchar(mimename))
    fcn2 <- paste0("met2CF.", mimename)
    if (exists(fcn1)) {
      fcn <- fcn1
    } else if (exists(fcn2)) {
      fcn <- fcn2
    } else {
      logger.error("met2CF function ", fcn1, " or ", fcn2, " don't exist")
    }
    
    cf0.id <- convert.input(input.id = input.id, 
                            outfolder = outfolder, 
                            formatname = formatname,
                            mimetype = mimetype, 
                            site.id = site.id, start_date = start_date, end_date = end_date, 
                            pkg = pkg, fcn = fcn, con = con, host = host, browndog = NULL, 
                            write = TRUE, 
                            format.vars = format.vars, 
                            overwrite = overwrite,
                            exact.dates = FALSE)
    
    input_name <- paste0(met, "_CF_Permute")
    fcn <- "permute.nc"
    outfolder <- file.path(dir, input_name)
    
    cf.id <- convert.input(input.id = cf0.id$input.id, 
                           outfolder = outfolder, 
                           formatname = formatname, 
                           mimetype = mimetype, 
                           site.id = site.id, 
                           start_date = start_date, end_date =  end_date, 
                           pkg = pkg, fcn = fcn, con = con, host = host, browndog = NULL,
                           write = TRUE, 
                           overwrite = overwrite, exact.dates = FALSE)
    
  } else if (register$scale == "site") {
    input_name <- paste0(met, "_CF_site_", str_ns)
    outfolder <- file.path(dir, input_name)
    
    fcn1 <- paste0("met2CF.", met)
    mimename <- register$format$mimetype
    mimename <- substr(mimename, regexpr("/", mimename) + 1, nchar(mimename))
    mimename <- substr(mimename, regexpr("-", mimename) + 1, nchar(mimename))
    fcn2 <- paste0("met2CF.", mimename)
    if (exists(fcn1)) {
      fcn <- fcn1
      cf.id <- convert.input(input.id = input.id,
                             outfolder = outfolder,
                             formatname = formatname,
                             mimetype = mimetype, 
                             site.id = site.id,
                             start_date = start_date, end_date = end_date, 
                             pkg = pkg, fcn = fcn, con = con, host = host, browndog = NULL,
                             write = TRUE,
                             lat = lat, lon = lon, 
                             overwrite = overwrite,
                             exact.dates = FALSE)
    } else if (exists(fcn2)) {
      fcn <- fcn2
      format <- query.format.vars(input.id, con)
      cf.id <- convert.input(input.id = input.id,
                             outfolder = outfolder,
                             formatname = formatname,
                             mimetype = mimetype, 
                             site.id = site.id, 
                             start_date = start_date, end_date = end_date,
                             pkg = pkg, fcn = fcn, con = con, host = host, browndog = NULL, 
                             write = TRUE, 
                             lat = lat, lon = lon, 
                             format.vars = format.vars, 
                             overwrite = overwrite, exact.dates = FALSE)
    } else {
      logger.error("met2CF function ", fcn1, " or ", fcn2, " doesn't exists")
    }
  }
  
  logger.info("Finished change to CF Standards")
  return(cf.id)
} # .met2cf.module
