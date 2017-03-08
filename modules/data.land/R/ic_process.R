##' @name ic_process
##' @title ic_process
##' @export
##'
##' @param settings pecan settings list
##' @param dbfiles where to write files
##' @param overwrite whether to force ic_process to proceed
##' @author Istem Fer
ic_process <- function(settings, input, dir, overwrite = FALSE){
  
  
  #--------------------------------------------------------------------------------------------------#
  # Extract info from settings and setup
  site       <- settings$run$site
  start_date <- settings$run$start.date
  end_date   <- settings$run$end.date
  model      <- settings$model$type
  host       <- settings$host
  dbparms    <- settings$database
  
  
  # If overwrite is a plain boolean, fill in defaults for each module
  if (!is.list(overwrite)) {
    if (overwrite) {
      overwrite <- list(getveg = TRUE,  putveg = TRUE)
    } else {
      overwrite <- list(getveg = FALSE, putveg = FALSE)
    }
  } else {
    if (is.null(overwrite$getveg)) {
      overwrite$getveg <- FALSE
    }
    if (is.null(overwrite$putveg)) {
      overwrite$putveg <- FALSE
    }
  }
  
  # set up bety connection
  bety <- dplyr::src_postgres(dbname   = dbparms$bety$dbname, 
                              host     = dbparms$bety$host, 
                              user     = dbparms$bety$user, 
                              password = dbparms$bety$password)
  
  con <- bety$con
  on.exit(db.close(con))
  
  # set up host information
  machine.host <- ifelse(host == "localhost" || host$name == "localhost", fqdn(), host$name)
  machine <- db.query(paste0("SELECT * from machines where hostname = '", machine.host, "'"), con)
  
  
  # setup site database number, lat, lon and name and copy for format.vars if new input
  new.site <- data.frame(id = as.numeric(site$id), 
                         lat = PEcAn.data.atmosphere::db.site.lat.lon(site$id, con = con)$lat, 
                         lon = PEcAn.data.atmosphere::db.site.lat.lon(site$id, con = con)$lon)
  str_ns <- paste0(new.site$id %/% 1e+09, "-", new.site$id %% 1e+09)
  
  outfolder <- file.path(dir, paste0(input_veg$source, "_site_", str_ns))
  # hack
  remote.execute.cmd(host, "mkdir", c("-p", outfolder))
  tmpfolder <- file.path(localdb, paste0(input_veg$source, "_site_", str_ns)) 
  dir.create(tmpfolder, showWarnings = F, recursive = T)
  
  raw.id <- NULL
  #--------------------------------------------------------------------------------------------------#
  # Load/extract + match species module
  
  if (is.null(input$id)) {

    raw.id <- .get.veg.module(input_veg = input, 
                              outfolder = outfolder, tmpfolder = tmpfolder,
                              start_date = start_date, end_date = end_date,
                              bety = bety, dbparms = dbparms,
                              lat = new.site$lat, lon = new.site$lon, site = site,
                              host = host, overwrite = overwrite$getveg, machine = machine)

  }
  

  #--------------------------------------------------------------------------------------------------#
  # Match species to PFTs + veg2model module
  if (!is.null(raw.id)) {
    
    ready.id <- .put.veg.module(raw.id = raw.id, bety = bety, 
                                input_veg = input, pfts = settings$pfts,
                                outfolder = outfolder, tmpfolder = tmpfolder,
                                dir = dir, machine = machine, model = model,
                                start_date = start_date, end_date = end_date,
                                lat = new.site$lat, lon = new.site$lon, site_id = new.site$id,
                                host = host, overwrite = overwrite$putveg)
    
  }

  #--------------------------------------------------------------------------------------------------#
  # Fill settings
  
  # use ready.id
  # model.file <- db.query(paste("SELECT * from dbfiles where container_id =", ic.id), con)
  # fill settings
  
  return(settings)
} # ic_process