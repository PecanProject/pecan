.met2cf.module <- function(raw.id, register, met, dir, machine, start_date, end_date, con, overwrite=FALSE, format.vars) {
  logger.info("Begin change to CF Standards")
  
  input.id  <-  raw.id$input.id[1]
  pkg       <- "PEcAn.data.atmosphere"
  formatname <- 'CF Meteorology'
  mimetype <- 'application/x-netcdf'
  format.id <- 33
  
  if(register$scale=="regional"){
    input_name <- paste0(met,"_CF")
    outfolder  <- file.path(dir, input_name)
    
    print("start CHECK change to CF standards")
    check = db.query(
      paste0("SELECT i.start_date, i.end_date, d.file_path, d.container_id, d.id ", 
             "from dbfiles as d join inputs as i on i.id = d.container_id where i.site_id=",
             register$siteid, " and d.container_type = 'Input' and i.format_id=", format.id, 
             " and d.machine_id =", machine$id, " and i.name = '", input_name, 
             "' and (i.start_date <= DATE '", as.POSIXlt(start_date, tz = "GMT"), 
             "') and (DATE '", as.POSIXlt(end_date, tz = "GMT"), "' <= i.end_date)" ),con)
    print(check, digits=10)
    print("end CHECK")
    
    if(!overwrite && length(check) > 0) {
      cf0.id <- list(input.id=check$container_id, dbfile.id=check$id)
      logger.info("Skipping met2CF because files are already available.")
    } else {
      fcn1 <- paste0("met2CF.", met)
      mimename <- register$format$mimetype
      mimename <- substr(mimename, regexpr('/', mimename) + 1, nchar(mimename))
      mimename <- substr(mimename, regexpr('-',mimename) + 1, nchar(mimename))
      fcn2 <- paste0("met2CF.", mimename)
      if(exists(fcn1)) {
        fcn <- fcn1
      } else if(exists(fcn2)) {
        fcn <- fcn2
      } else {
        logger.error("met2CF function ", fcn1, " or ", fcn2, " don't exist")
      }
      
      cf0.id <- convert.input(input.id, outfolder, formatname, mimetype, site.id=site$id,
                              start_date, end_date, pkg, fcn, con=con, host=host, browndog=NULL, write=TRUE,
                              format.vars=format.vars, overwrite=overwrite)
    }
    
    input_name <- paste0(met,"_CF_Permute")
    fcn       <-  "permute.nc"
    outfolder  <- file.path(dir, input_name)
    
    print("start CHECK permute nc")
    check = db.query(
      paste0("SELECT i.start_date, i.end_date, d.file_path, d.container_id, d.id  ",
             "from dbfiles as d join inputs as i on i.id = d.container_id where i.site_id =", 
             register$siteid, " and d.container_type = 'Input' and i.format_id=", format.id, 
             " and d.machine_id =", machine$id, " and i.name = '", input_name, 
             "' and (i.start_date <= DATE '" ,as.POSIXlt(start_date, tz = "GMT"), 
             "') and (DATE '", as.POSIXlt(end_date, tz = "GMT"), "' <= i.end_date)" ), con)
    print(check, digits=10)
    print("end CHECK")
    
    if(!overwrite && length(check) > 0) {
      cf.id <- list(input.id=check$container_id, dbfile.id=check$id)
      logger.info("Skipping permute.nc because files are already available.")
    } else {
      # Just a draft of what would happen - doesn't include using the cluster so it would be SLOW. 
      # Hasn't been tested.
      cf.id <- convert.input(cf0.id$input.id, outfolder, formatname, mimetype, site.id=site$id, 
                             start_date, end_date, pkg, fcn, con=con, host=host, browndog=NULL,
                             write=TRUE, overwrite=overwrite)
    }
  } else if(register$scale=="site") {
    input_name <- paste0(met,"_CF_site_",str_ns)
    outfolder  <- file.path(dir,input_name)
    
    print("start CHECK change to CF standards")
    check = db.query(
      paste0("SELECT i.start_date, i.end_date, d.file_path, d.container_id, d.id  ", 
             "from dbfiles as d join inputs as i on i.id = d.container_id where i.site_id =",
             new.site$id, " and d.container_type = 'Input' and i.format_id=", 33, 
             " and d.machine_id =", machine$id, " and i.name = '", input_name,
             "' and (i.start_date <= DATE '", as.POSIXlt(start_date, tz = "GMT"),
             "') and (DATE '", as.POSIXlt(end_date, tz = "GMT"), "' <= i.end_date)" ), con)
    print(check, digits=10)
    print("end CHECK")
    
    if(!overwrite && length(check)>0) {
      cf.id <- list(input.id=check$container_id, dbfile.id=check$id)
      logger.info("Skipping met2CF because files are already available.")
    } else {
      fcn1 <- paste0("met2CF.",met)
      mimename <- register$format$mimetype
      mimename <- substr(mimename, regexpr('/', mimename) + 1, nchar(mimename))
      mimename <- substr(mimename, regexpr('-', mimename) + 1, nchar(mimename))
      fcn2 <- paste0("met2CF.", mimename)
      if(exists(fcn1)) {
        fcn <- fcn1
        cf.id <- convert.input(
          input.id, outfolder, formatname, mimetype, site.id=site$id, 
          start_date, end_date, pkg, fcn, con=con, host=host, browndog=NULL, 
          write=TRUE, site$lat, site$lon, overwrite=overwrite)
      } else if(exists(fcn2)) {
        fcn <- fcn2
        format <- query.format.vars(input.id,con)
        cf.id <- convert.input(
          input.id, outfolder, formatname, mimetype, site.id=site$id, 
          start_date, end_date, pkg, fcn, con=con, host=host, browndog=NULL, 
          write=TRUE, site$lat, site$lon, format.vars=format.vars, 
          overwrite=overwrite)
      } else {
        logger.error("met2CF function ", fcn1, " or ", fcn2," doesn't exists")
      }
    }
  }
  
  logger.info("Finished change to CF Standards")
  return(cf.id)
}