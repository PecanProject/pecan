.download.raw.met.module <- function(dir, met, register, machine, start_date, end_date, con, new.site=NULL, host, overwrite=FALSE) {
  outfolder  <- file.path(dir, met)
  pkg        <- "PEcAn.data.atmosphere"
  fcn        <- paste0("download.",met)
  
  if(register$scale=="regional") { 
    # Right now this only means NARR but will need to be generalized once we have more 
    # regional met products
    
    # fcn.args <- list(outfolder=outfolder, start_date=start_date, end_date=end_date)
    
    
    raw.data.site.id <- ifelse(met %in% c("CRUNCEP", "GFDL"), new.site$id, register$siteid)

    raw.id <- convert.input(
      input.id=NA, outfolder=outfolder, formatname=register$format$name, mimetype=register$format$mimetype, 
      site.id=raw.data.site.id, start_date=start_date, end_date=end_date, 
      pkg=pkg, fcn=fcn, con=con, host=host, browndog=NULL, write=TRUE, overwrite=overwrite, 
      site_id=new.site$id, lat.in=new.site$lat, lon.in=new.site$lon,
      model=input_met$model, scenario=input_met$scenario, ensemble_member=input_met$ensemble_member)

  } else if(register$scale=="site") { # Site-level met
    
    print("start CHECK download raw met")
    check = db.query(
      paste0("SELECT i.start_date, i.end_date, d.file_path, d.container_id, d.id ",
             "from dbfiles as d join inputs as i on i.id = d.container_id where i.site_id =",
             site$id, " and d.container_type = 'Input' and i.format_id=", register$format$id, 
             " and d.machine_id =", machine$id,
             " and (i.start_date <= DATE '", as.POSIXlt(start_date, tz = "GMT"), 
             "') and (DATE '", as.POSIXlt(end_date, tz = "GMT"),"' <= i.end_date)" ), con)
    print(check, digits=10)
    print("end CHECK")
    
    if(length(check) > 0) {
      raw.id <- list(input.id=check$container_id, dbfile.id=check$id)
      logger.info("Skipping raw met download because files are already available.")
    } else {
      outfolder = paste0(outfolder,"_site_",str_ns)
      fcn.args <- list(
        sitename=site$name, outfolder=outfolder, 
        start_date=start_date, end_date=end_date, 
        overwrite=overwrite, username=username)
      
      cmdFcn = paste0(pkg, "::", fcn, "(", listToArgString(fcn.args), ")")
      
      new.files <- remote.execute.R(
        script=cmdFcn, host=host, user=NA, verbose=TRUE, R="R")
      
      ## insert database record
      raw.id <- dbfile.input.insert(in.path=dirname(new.files$file[1]),
                                    in.prefix=new.files$dbfile.name[1],
                                    siteid = site$id,
                                    startdate = start_date,
                                    enddate = end_date,
                                    mimetype=new.files$mimetype[1],
                                    formatname=new.files$formatname[1],
                                    parentid=NA,
                                    con = con,
                                    hostname = host$name)
    }
  }
  return(raw.id)
}
