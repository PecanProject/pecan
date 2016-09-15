.download.raw.met.module <- function(dir, met, register, machine, start_date, end_date, con, new.site=NULL, host) {
  outfolder  <- file.path(dir, met)
  pkg        <- "PEcAn.data.atmosphere"
  fcn        <- paste0("download.",met)
  
  if(register$scale=="regional") { 
    # Right now this only means NARR but will need to be generalized once we have more 
    # regional met products
 
    print("start CHECK download raw met")
    existing.dbfile <- dbfile.input.check(
      siteid=register$siteid, mimetype=register$format$mimetype, formatname=register$format$name, 
      parentid=NA, con=con, hostname=machine$hostname, ignore.dates=TRUE)
    print(existing.dbfile, digits=10)
    print("end CHECK")
    
    if(nrow(existing.dbfile) > 0) {
      if(nrow(existing.dbfile) > 1) {
        print(existing.dbfile)
        logger.warn("Multiple existing inputs found. Using last.")
        existing.dbfile <- existing.dbfile[nrow(existing.dbfile),]
      }
      
      existing.input <- db.query(paste0("SELECT * FROM inputs WHERE id=", existing.dbfile[['container_id']]), con)    
      # Convert dates to Date objects and strip all time zones (DB values are timezone-free)
      start_date <- force_tz(as_date(start_date), 'GMT')
      end_date <- force_tz(as_date(end_date), 'GMT')
      existing.input$start_date <- force_tz(as_date(existing.input$start_date), 'GMT')
      existing.input$end_date <- force_tz(as_date(existing.input$end_date), 'GMT')
      
      if(!overwrite && (start_date >= existing.input$start_date) && (end_date <= existing.input$end_date)) {
        # There's an existing input that spans desired start/end dates. Use that one. 
        logger.info("Skipping raw met download because files are already available.")
        return(list(input.id=existing.input$id, dbfile.id=existing.dbfile$id))
      } else if(overwrite) {
        # start and end dates stay the same. 
      # collect files to flag for deletion
      } else {
        # Start/end dates need to be updated so that the input spans a continuous timeframe

        start_date <- min(start_date, existing.input$start_date)
        end_date <- max(end_date, existing.input$end_date)
        logger.info(paste0("Changed start/end dates to '", start_date, "'/'", end_date, "' ",
                    "so that existing input can be updated while maintaining continuous time span."))
        
        # There might be existing files for some years (but not all; checked that above)
        # fcn should be smart enough not overwrite the existing ones, 
        # and hopefully won't waste time working on them either
        # At the end, if convert.inputs was successful we'll need to update its start/end dates
        # We don't know the dbfile path/prefix until after fcn runs, so unfortunately can't check 
        # that the new dbfile record won't conflict with existing ones. 
      }
    } else {
      # No existing record found. Should be good to go?
    }

 
    fcn.args <- list(outfolder=outfolder, start_date=start_date, end_date=end_date)
    
    raw.data.site.id <- register$siteid
    if(met == "CRUNCEP") {
      ## this is a hack for regional products that go direct to site-level extraction. 
      ## Needs generalization (mcd)
      fcn.args$site_id <- new.site$id
      fcn.args$lat.in  <- new.site$lat
      fcn.args$lon.in  <- new.site$lon
      stage$met2cf = FALSE
      stage$standardize = FALSE
      raw.data.site.id <- new.site$id
    }
    
    if (met == "GFDL") {
      fcn.args$site_id         <- new.site$id
      fcn.args$lat.in          <- new.site$lat
      fcn.args$lon.in          <- new.site$lon
      fcn.args$model           <- input_met$model
      fcn.args$scenario        <- input_met$scenario
      fcn.args$ensemble_member <- input_met$ensemble_member
      stage$met2cf = FALSE
      stage$standardize = FALSE
      raw.data.site.id         <- new.site$id
    }

    cmdFcn  = paste0(pkg, "::", fcn, "(", listToArgString(fcn.args), ")")
    new.files <- remote.execute.R(cmdFcn, host, verbose=TRUE)

        
    if(exists("existing.input") && nrow(existing.input) > 0) {
      db.query(paste0(
        "UPDATE inputs SET start_date='", start_date, "', end_date='", end_date, "', ", 
        "updated_at=NOW() WHERE id=", existing.input$id), con)
    }

    raw.id <- dbfile.input.insert(in.path = dirname(new.files$file[1]),
                                  in.prefix = new.files$dbfile.name[1],
                                  siteid = raw.data.site.id,
                                  startdate = start_date,
                                  enddate = end_date,
                                  mimetype=new.files$mimetype[1],
                                  formatname=new.files$formatname[1],
                                  parentid = NA,
                                  con = con,
                                  hostname = host$name)
  
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
