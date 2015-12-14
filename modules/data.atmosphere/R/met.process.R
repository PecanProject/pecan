##' @name met.process
##' @title met.process
##' @export
##'
##' @param site Site info from settings file
##' @param input currently "NARR" or "Ameriflux"
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param model model_type name
##' @param host Host info from settings file
##' @param dbparms  database settings from settings file
##' @param dir  directory to write outputs to
##'
##' @author Elizabeth Cowdery, Michael Dietze
met.process <- function(site, input_met, start_date, end_date, model, host, dbparms, dir, browndog=NULL){
  require(RPostgreSQL)
  require(XML)

  #setup connection and host information
  con      <- db.open(dbparms)
  username <- ""
  machine.host <- ifelse(host$name == "localhost", fqdn(),host$name)
  machine = db.query(paste0("SELECT * from machines where hostname = '",machine.host,"'"),con)

  #get met source and potentially determine where to start in the process
  met <- ifelse(is.null(input_met$source), logger.error("Must specify met source"),input_met$source)

  # special case Brown Dog
  if (!is.null(browndog)) {
    result <- browndog.met(browndog, met, site, start_date, end_date, model, dir)

    if (is.data.frame(result)) {
      dbfile.input.insert(in.path= dirname(result$file),
                          in.prefix = result$dbfile.name,
                          siteid = site$id,
                          startdate = start_date,
                          enddate = end_date,
                          mimetype= result$mimetype,
                          formatname= result$formatname,
                          parentid= NA,
                          con = con,
                          hostname = result$host)
      db.close(con)
      invisible(return(result$file))
    }
  }


  #read in registration xml for met specific information
  register.xml <- system.file(paste0("registration/register.", met, ".xml"), package = "PEcAn.data.atmosphere")
  register <- read.register(register.xml, con)

  # first attempt at function that designates where to start met.process
  if(is.null(input_met$id)){
    stage <- list(download.raw = TRUE, met2cf = TRUE, standardize = TRUE, met2model = TRUE)
  }else{
    stage <- met.process.stage(input_met$id,register$format$id,con)
    # Is there a situation in which the input ID could be given but not the file path?
    # I'm assuming not right now
    assign(stage$id.name,list(
      inputid = input_met$id,
      dbfileid = db.query(paste0("SELECT id from dbfiles where file_name = '", basename(input_met$path) ,"' AND file_path = '", dirname(input_met$path) ,"'"),con)[[1]]
    ))
  }

  #setup additional browndog arguments
  if(!is.null(browndog)){browndog$inputtype <- register$format$inputtype}

  #setup site database number, lat, lon and name
  new.site <- data.frame(id = as.numeric(site$id), lat = db.site.lat.lon(site$id,con=con)$lat, lon = db.site.lat.lon(site$id,con=con)$lon)
  str_ns    <- paste0(new.site$id %/% 1000000000, "-", new.site$id %% 1000000000)

  #--------------------------------------------------------------------------------------------------#
  # Download raw met from the internet

  if(stage$download.raw==TRUE){
  outfolder  <- file.path(dir,met)
  pkg        <- "PEcAn.data.atmosphere"
  fcn        <- paste0("download.",met)

  if(register$scale=="regional"){ #Right now this only means NARR but will need to be generalized once we have more regional met products

    print("start CHECK")
    check = db.query(
      paste0("SELECT i.start_date, i.end_date, d.file_path, d.container_id, d.id  from dbfiles as d join inputs as i on i.id = d.container_id where i.site_id =",register$siteid,
             " and d.container_type = 'Input' and i.format_id=",register$format$id, " and d.machine_id =",machine$id,
             " and (i.start_date <= DATE '",as.POSIXlt(start_date, tz = "GMT"),"') and (DATE '", as.POSIXlt(end_date, tz = "GMT"),"' <= i.end_date)" ),con)
    print("end CHECK")
    options(digits=10)
    print(check)
    if(length(check)>0){
      raw.id <- list(input.id=check$container_id, dbfile.id=check$id)
    }else{

      args <- list(outfolder, start_date, end_date)
      if(met %in% "CRUNCEP") {
        ## this is a hack for regional products that go direct to site-level extraction. Needs generalization (mcd)
        args <- c(args, new.site$id, new.site$lat, new.site$lon) 
        stage$met2cf = FALSE
        stage$standardize = FALSE
      }
      cmdFcn  = paste0(pkg,"::",fcn,"(",paste0("'",args,"'",collapse=","),")")
      new.files <- remote.execute.R(cmdFcn,host$name,user=NA, verbose=TRUE)

      raw.id <- dbfile.input.insert(in.path=dirname(new.files$file[1]),
                                    in.prefix=new.files$dbfile.name[1],
                                    siteid = site$id,
                                    startdate = start_date,
                                    enddate = end_date,
                                    mimetype=new.files$mimetype[1],
                                    formatname=new.files$formatname[1],
                                    parentid = NA,
                                    con = con,
                                    hostname = host$name)
      if(met %in% "CRUNCEP"){ready.id = raw.id}
    }

  }else if(register$scale=="site") { # Site-level met

    print("start CHECK")
    check = db.query(
      paste0("SELECT i.start_date, i.end_date, d.file_path, d.container_id, d.id  from dbfiles as d join inputs as i on i.id = d.container_id where i.site_id =",site$id,
             " and d.container_type = 'Input' and i.format_id=",register$format$id, " and d.machine_id =",machine$id,
             " and (i.start_date <= DATE '",as.POSIXlt(start_date, tz = "GMT"),"') and (DATE '", as.POSIXlt(end_date, tz = "GMT"),"' <= i.end_date)" ),con)
    print("end CHECK")
    options(digits=10)
    print(check)
    if(length(check)>0){
      raw.id <- list(input.id=check$container_id, dbfile.id=check$id)
    }else{

      outfolder = paste0(outfolder,"_site_",str_ns)
      args <- list(site$name, outfolder, start_date, end_date)

      cmdFcn  = paste0(pkg,"::",fcn,"(",paste0("'",args,"'",collapse=","),")")
      new.files <- remote.execute.R(script=cmdFcn,host=host$name,user=NA,verbose=TRUE,R="R")

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
  }

  #--------------------------------------------------------------------------------------------------#
  # Change to  CF Standards

  if(stage$met2cf == TRUE){
  logger.info("Begin change to CF Standards")

  input.id  <-  raw.id[1]
  pkg       <- "PEcAn.data.atmosphere"
  formatname <- 'CF Meteorology'
  mimetype <- 'application/x-netcdf'
  format.id <- 33


  if(register$scale=="regional"){

    input_name <- paste0(met,"_CF")
    outfolder  <- file.path(dir,input_name)

    print("start CHECK")
    check = db.query(
      paste0("SELECT i.start_date, i.end_date, d.file_path, d.container_id, d.id  from dbfiles as d join inputs as i on i.id = d.container_id where i.site_id =",register$siteid,
             " and d.container_type = 'Input' and i.format_id=",format.id, " and d.machine_id =",machine$id, " and i.name = '", input_name,
             "' and (i.start_date <= DATE '",as.POSIXlt(start_date, tz = "GMT"),"') and (DATE '", as.POSIXlt(end_date, tz = "GMT"),"' <= i.end_date)" ),con)
    print("end CHECK")
    options(digits=10)
    print(check)
    if(length(check)>0){
      cf0.id <- list(input.id=check$container_id, dbfile.id=check$id)
    }else{

      fcn1 <- paste0("met2CF.",met)
      fcn2 <- paste0("met2CF.",register$format$mimetype)
      if(exists(fcn1)){
        fcn <- fcn1
      }else if(exists(fcn2)){
        fcn <- fcn2
      }else{logger.error("met2CF function doesn't exists")}

      cf0.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,fcn,
                              username,con=con,hostname=host$name,browndog=NULL,write=TRUE)
    }

    input_name <- paste0(met,"_CF_Permute")
    fcn       <-  "permute.nc"
    outfolder  <- file.path(dir,input_name)

    print("start CHECK")
    check = db.query(
      paste0("SELECT i.start_date, i.end_date, d.file_path, d.container_id, d.id  from dbfiles as d join inputs as i on i.id = d.container_id where i.site_id =",register$siteid,
             " and d.container_type = 'Input' and i.format_id=",format.id, " and d.machine_id =",machine$id, " and i.name = '", input_name,
             "' and (i.start_date <= DATE '",as.POSIXlt(start_date, tz = "GMT"),"') and (DATE '", as.POSIXlt(end_date, tz = "GMT"),"' <= i.end_date)" ),con)
    print("end CHECK")
    options(digits=10)
    print(check)
    if(length(check)>0){
      cf.id <- list(input.id=check$container_id, dbfile.id=check$id)
    }else{
      # Just a draft of what would happen - doesn't include using the cluster so it would be SLOW. Hasn't been tested.
      cf.id <- convert.input(cf0.id, outfolder2,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,permute.nc,
                             username,con=con,hostname=host$name,browndog=NULL,write=TRUE)
    }

  }else if(register$scale=="site"){

    input_name <- paste0(met,"_CF_site_",str_ns)
    outfolder  <- file.path(dir,input_name)

    print("start CHECK")
    check = db.query(
      paste0("SELECT i.start_date, i.end_date, d.file_path, d.container_id, d.id  from dbfiles as d join inputs as i on i.id = d.container_id where i.site_id =",new.site$id,
             " and d.container_type = 'Input' and i.format_id=",33, " and d.machine_id =",machine$id, " and i.name = '", input_name,
             "' and (i.start_date <= DATE '",as.POSIXlt(start_date, tz = "GMT"),"') and (DATE '", as.POSIXlt(end_date, tz = "GMT"),"' <= i.end_date)" ),con)
    print("end CHECK")
    options(digits=10)
    print(check)
    if(length(check)>0){
      cf.id <- list(input.id=check$container_id, dbfile.id=check$id)
    }else{
      fcn1 <- paste0("met2CF.",met)
      fcn2 <- paste0("met2CF.",register$format$mimetype)
      if(exists(fcn1)){
        fcn <- fcn1
        cf.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,fcn,
                               username,con=con,hostname=host$name,browndog=NULL,write=TRUE,site$lat,site$lon) 
      }else if(exists(fcn2)){
        fcn <- fcn2
        format <- query.format(input.id,con)
        cf.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,fcn,
                               username,con=con,hostname=host$name,browndog=NULL,write=TRUE,site$lat,site$lon,format) 
      }else{logger.error("met2CF function doesn't exists")}
    }  
  }

  logger.info("Finished change to CF Standards")
  }

  #--------------------------------------------------------------------------------------------------#
  # Change to Site Level - Standardized Met (i.e. ready for conversion to model specific format)

  if(stage$standardize == TRUE){
  logger.info("Begin Standardize Met")

  if(register$scale=="regional"){ #### Site extraction

    logger.info("Site Extraction")

    input.id   <- cf.id[1]
    outfolder  <- file.path(dir,paste0(met,"_CF_site_",str_ns))
    pkg        <- "PEcAn.data.atmosphere"
    fcn        <- "extract.nc"
    formatname <- 'CF Meteorology'
    mimetype   <- 'application/x-netcdf'

    ready.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,fcn,
                              username,con=con,hostname=host$name,browndog=NULL,write=TRUE,
                              slat=new.site$lat,slon=new.site$lon,newsite=new.site$id)

  }else if(register$scale=="site"){ ##### Site Level Processing
#     if(!is.null(register$gapfill)){
      logger.info("Gapfilling") # Does NOT take place on browndog!
      
      input.id   <- cf.id[1]
      outfolder  <- file.path(dir,paste0(met,"_CF_gapfill_site_",str_ns))
      pkg        <- "PEcAn.data.atmosphere"
      fcn        <- "metgapfill"
#       fcn        <- register$gapfill
      formatname <- 'CF Meteorology'
      mimetype   <- 'application/x-netcdf'
      lst        <- site.lst(site,con)
      
      ready.id   <- convert.input(input.id,outfolder,formatname,mimetype,site.id=site$id
                                  ,start_date,end_date,pkg,fcn,username,con=con,
                                  hostname=host$name,browndog=NULL,write=TRUE,lst=lst)
#     }else{
#       ready.id<-cf.id[1]
#     }
#     
  }
  logger.info("Finished Standardize Met")
  }

  #--------------------------------------------------------------------------------------------------#
  # Prepare for Model

  if(stage$met2model == TRUE){
  logger.info("Begin Model Specific Conversion")

  # Determine output format name and mimetype
  model_info <- db.query(paste0("SELECT f.name, f.id, mt.type_string from modeltypes as m",
                                " join modeltypes_formats as mf on m.id = mf.modeltype_id",
                                " join formats as f on mf.format_id = f.id",
                                " join mimetypes as mt on f.mimetype_id = mt.id",
                                " where m.name = '", model, "' AND mf.tag='met'"),con)
  formatname <- model_info[1]
  mimetype   <- model_info[3]

  print("# Convert to model format")

  input.id  <- ready.id$input.id[1]
  outfolder <- file.path(dir,paste0(met,"_",model,"_site_",str_ns))
  pkg       <- paste0("PEcAn.",model)
  fcn       <- paste0("met2model.",model)
  lst       <- site.lst(site,con)

  model.id  <- convert.input(input.id,outfolder,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,fcn,
                             username,con=con,hostname=host$name,browndog,write=TRUE,lst=lst,lat=new.site$lat,lon=new.site$lon)
  }

  logger.info(paste("Finished Model Specific Conversion",model.id[1]))

  model.file <- db.query(paste("SELECT * from dbfiles where id =",model.id[[2]]),con)[["file_name"]]

  db.close(con)
  return(file.path(outfolder, model.file))

}

#################################################################################################################################

##' @name db.site.lat.lon
##' @title db.site.lat.lon
##' @export
##' @param site.id
##' @param con
##' @author Betsy Cowdery
##'
db.site.lat.lon <- function(site.id,con){
  site <- db.query(paste("SELECT id, ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id =",site.id),con)
  if(nrow(site)==0){logger.error("Site not found"); return(NULL)}
  if(!(is.na(site$lat)) && !(is.na(site$lat))){
    return(list(lat = site$lat, lon = site$lon))
  }
}

#################################################################################################################################


##' @name browndog.met
##' @description Use browndog to get the met data for a specific model
##' @title get met data from browndog
##' @export
##' @param browndog, list with url, username and password to connect to browndog
##' @param source, the source of the met data, currently only NARR an Ameriflux is supported
##' @param site, site information should have id, lat, lon and name (ameriflux id)
##' @param start_date, start date for result
##' @param end_date, end date for result
##' @param model, model to convert the met data to
##' @param dir, folder where results are stored (in subfolder)
##
##' @author Rob Kooper
browndog.met <- function(browndog, source, site, start_date, end_date, model, dir) {
  folder <- tempfile("BD-", dir)
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)

  if (source == "Ameriflux") {
    sitename <- sub(".*\\((.+)\\)", "\\1", site$name)
  } else if (source == "NARR") {
    sitename <- gsub("[\\s/()]", "-", site$name, perl=TRUE)
  } else {
    logger.warn("Could not process source", source)
    invisible(return(NA))
  }

  # this logic should live somewhere else, maybe the registry?
  if (model == "SIPNET") {
    formatname <- "clim"
    outputfile <- file.path(folder, "sipnet.clim")
    results <- data.frame(file=outputfile,
                          host = fqdn(),
                          mimetype ='text/csv',
                          formatname = 'Sipnet.climna' ,
                          startdate = start_date ,
                          enddate = end_date,
                          dbfile.name = basename(outputfile),
                          stringsAsFactors = FALSE)
  } else if (model == "ED2") {
    formatname <- "ed.zip"
    outputfile <- file.path(folder, "ed.zip")
    results <- data.frame(file=file.path(folder, "ED_MET_DRIVER_HEADER"),
                          host = fqdn(),
                          mimetype ='text/plain',
                          formatname = 'ed.met_driver_header files format' ,
                          startdate = start_date ,
                          enddate = end_date,
                          dbfile.name = "ED_MET_DRIVER_HEADER",
                          stringsAsFactors = FALSE)
  } else if (model == "DALEC") {
    formatname <- "dalec"
    outputfile <- file.path(folder, "dalec.dat")
    results <- data.frame(file=outputfile,
                          host = fqdn(),
                          mimetype ='text/plain',
                          formatname = 'DALEC meteorology' ,
                          startdate = start_date ,
                          enddate = end_date,
                          dbfile.name = basename(outputfile),
                          stringsAsFactors = FALSE)
  } else if (model == "LINKAGES") {
    formatname <- "linkages"
    outputfile <- file.path(folder, "climate.txt")
    results <- data.frame(file=outputfile,
                          host = fqdn(),
                          mimetype ='text/plain',
                          formatname = 'LINKAGES meteorology' ,
                          startdate = start_date ,
                          enddate = end_date,
                          dbfile.name = basename(outputfile),
                          stringsAsFactors = FALSE)
  } else {
    logger.warn("Could not process model", model)
    invisible(return(NA))
  }

  xmldata <- paste0("<input>",
                    "<type>", source, "</type>",
                    "<site>", sitename, "</site>",
                    "<lat>", site$lat, "</lat>",
                    "<lon>", site$lon, "</lon>",
                    "<start_date>", start_date, "</start_date>",
                    "<end_date>", end_date, "</end_date>",
                    "</input>")

  userpass <- paste(browndog$username, browndog$password, sep=":")
  curloptions <- list(userpwd=userpass, httpauth=1L, followlocation=TRUE)
  result <- postForm(paste0(browndog$url, formatname, "/"),
                     "fileData"=fileUpload("pecan.xml", xmldata, "text/xml"),
                     .opts=curloptions)
  url <- gsub('.*<a.*>(.*)</a>.*', '\\1', result)
  downloadedfile <- download.url(url, outputfile, 600, curloptions)

  # fix returned data
  if (model == "ED2") {
    unzip(downloadedfile, exdir=folder)
    # fix ED_MET_DRIVER_HEADER
    x <- readLines(results$file)
    x[3] <- folder
    writeLines(x, results$file)
  } else {
    results$file <- downloadedfile
    results$dbfile.name <- basename(downloadedfile)
  }

  invisible(return(results))
}

#################################################################################################################################

##' @name site_from_tag
##' @title site_from_tag
##' @export
##' @param sitename
##' @param tag
##' @author Betsy Cowdery
##'
##' Function to find the site code for a specific tag
##' Example:
##'   sitename = "Rhinelander Aspen FACE Experiment (FACE-RHIN)"
##'   tag = "FACE"
##'   site_from_tag(sitename,tag) = "RHIN"
##' Requires that site names be set up specifically with (tag-sitecode) - this may change


site_from_tag <- function(sitename,tag){
  temp <- regmatches(sitename,gregexpr("(?<=\\().*?(?=\\))", sitename, perl=TRUE))[[1]]
  pref <- paste0(tag,"-")
  site <- unlist(strsplit(temp[grepl(pref,temp)], pref))[2]
  return(site)
}
