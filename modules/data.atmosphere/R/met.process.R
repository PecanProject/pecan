##' @name met.process
##' @title met.process
##' @export
##'
##' @param site Site info from settings file
##' @param input_met Which data source to process. 
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param model model_type name
##' @param host Host info from settings file
##' @param dbparms  database settings from settings file
##' @param dir  directory to write outputs to
##' @param spin spin-up settings passed to model-specific met2model. List containing nyear (number of years of spin-up), nsample (first n years to cycle), and resample (TRUE/FALSE)
##' @param overwrite Whether to force met.process to proceed.
##' 
##'        `overwrite` may be a list with individual components corresponding to 
##'        `download`, `met2cf`, `standardize`, and `met2model`. If it is instead a simple boolean,
##'        the default behavior for `overwrite=FALSE` is to overwrite nothing, as you might expect.
##'        Note however that the default behavior for `overwrite=TRUE` is to overwrite everything
##'        *except* raw met downloads. I.e., it corresponds to:
##'
##'        list(download = FALSE, met2cf = TRUE, standardize = TRUE,  met2model = TRUE)
##'
##' @author Elizabeth Cowdery, Michael Dietze, Ankur Desai, James Simkins, Ryan Kelly
met.process <- function(site, input_met, start_date, end_date, model,
                        host = "localhost", dbparms, dir, browndog = NULL, spin=NULL,
                        overwrite = FALSE) {
  # get met source and potentially determine where to start in the process
  if(is.null(input_met$source)){
    if(is.null(input_met$id)){
      PEcAn.logger::logger.warn("met.process only has a path provided, assuming path is model driver and skipping processing")
      
      # Additional layer of list depth added for consistancy with other return statements.
      temp_path = input_met$path
      input_met$path <- list()
      input_met$path$path1 <- temp_path
      return(input_met)
    }else {
     PEcAn.logger::logger.warn("No met source specified")
      if(!is.null(input_met$id) & !is.null(input_met$path)){
       PEcAn.logger::logger.warn("Assuming source CFmet")
        met <- input_met$source <- "CFmet" ## this case is normally hit when the use provides an existing file that has already been
        ## downloaded, processed, and just needs conversion to model-specific format.
        ## setting a 'safe' (global) default
      } else {
       PEcAn.logger::logger.error("Cannot process met without source information")
      }  
    }
  } else {
    met <-input_met$source
  }
  
  # If overwrite is a plain boolean, fill in defaults for each stage
  if (!is.list(overwrite)) {
    if (overwrite) {
      # Default for overwrite==TRUE is to overwrite everything but download
      overwrite <- list(download = FALSE, met2cf = TRUE, standardize = TRUE,  met2model = TRUE)
    } else {
      overwrite <- list(download = FALSE, met2cf = FALSE, standardize = FALSE, met2model = FALSE)
    }
  } else {
    if (is.null(overwrite$download)) {
      overwrite$download <- FALSE
    }
    if (is.null(overwrite$met2cf)) {
      overwrite$met2cf <- FALSE
    }
    if (is.null(overwrite$standardize)) {
      overwrite$standardize <- FALSE
    }
    if (is.null(overwrite$met2model)) {
      overwrite$met2model <- FALSE
    }
  }
  overwrite.check <- unlist(overwrite)
  for (i in seq_along(overwrite.check)) {
    if (i < length(overwrite.check) && 
        overwrite.check[i] == TRUE && 
        !all(overwrite.check[(i + 1):length(overwrite.check)])) {
      print(overwrite)
     PEcAn.logger::logger.error(paste0("If overwriting any stage of met.process, ", "all subsequent stages need to be overwritten too. Please correct."))
    }
  }
  
  # set up connection and host information
  bety <- dplyr::src_postgres(dbname   = dbparms$dbname, 
                       host     = dbparms$host, 
                       user     = dbparms$user, 
                       password = dbparms$password)
  
  con <- bety$con
  on.exit(PEcAn.DB::db.close(con))
  username <- ifelse(is.null(input_met$username), "pecan", input_met$username)
  machine.host <- ifelse(host == "localhost" || host$name == "localhost", PEcAn.remote::fqdn(), host$name)
  machine <- PEcAn.DB::db.query(paste0("SELECT * from machines where hostname = '", machine.host, "'"), con)

  # special case Brown Dog
  if (!is.null(browndog)) {
    result <- browndog.met(browndog, met, site, start_date, end_date, model, dir, username, con)
    
    if (is.data.frame(result)) {
      dbentry = PEcAn.DB::dbfile.input.insert(in.path = dirname(result$file),
                          in.prefix = result$dbfile.name,
                          siteid = site$id, 
                          startdate = start_date, enddate = end_date,
                          mimetype = result$mimetype,
                          formatname = result$formatname, 
                          parentid = NA, 
                          con = con, hostname = result$host)
      
      # Additonal list depth added for consistancy with other return statements.
      input_met$path <- list()
      input_met$path$path1 <- result$file
      input_met$id <- list()
      input_met$id$id1 <- dbentry$input.id
      
      return(invisible(input_met))
    }
  }
  
  # read in registration xml for met specific information
  register.xml <- system.file(paste0("registration/register.", met, ".xml"), package = "PEcAn.data.atmosphere")
  register     <- read.register(register.xml, con)
  
  # first attempt at function that designates where to start met.process
  if (is.null(input_met$id)) {
    stage <- list(download.raw = TRUE, met2cf = TRUE, standardize = TRUE, met2model = TRUE)
    format.vars <- PEcAn.DB::query.format.vars(bety = bety, format.id = register$format$id)  # query variable info from format id
  } else {
    stage <- met.process.stage(input.id=input_met$id, raw.id=register$format$id, con)
    format.vars <- PEcAn.DB::query.format.vars(bety = bety, input.id = input_met$id)  # query DB to get format variable information if available
    # Is there a situation in which the input ID could be given but not the file path? 
    # I'm assuming not right now
    assign(stage$id.name, list(inputid = input_met$id,
                               dbfileid = PEcAn.DB::dbfile.check("Input",input_met$id,hostname = machine.host,con=con)$id))
  }
  print(stage)
  
  if(is.null(model)){
    stage$model <- FALSE
  }
  
  # setup additional browndog arguments
  if (!is.null(browndog)) {
    browndog$inputtype <- register$format$inputtype
  }
  
  # setup site database number, lat, lon and name and copy for format.vars if new input
  new.site <- data.frame(id = as.numeric(site$id), 
                         lat = db.site.lat.lon(site$id, con = con)$lat, 
                         lon = db.site.lat.lon(site$id, con = con)$lon)
  str_ns <- paste0(new.site$id %/% 1e+09, "-", new.site$id %% 1e+09)
  
  if (is.null(format.vars$lat)) {
    format.vars$lat <- new.site$lat
  }
  if (is.null(format.vars$lon)) {
    format.vars$lon <- new.site$lon
  }
  if (is.null(format.vars$site)) {
    format.vars$site <- new.site$id
  }
  
  #--------------------------------------------------------------------------------------------------#
  # Download raw met
  if (stage$download.raw) {
    raw.data.site.id <- ifelse(is.null(register$siteid), new.site$id, register$siteid)
    
    raw.id <- .download.raw.met.module(dir = dir,
                                       met = met, 
                                       register = register, 
                                       machine = machine, 
                                       start_date = start_date, end_date = end_date,
                                       str_ns =str_ns, con = con, 
                                       input_met = input_met, 
                                       site.id = raw.data.site.id, 
                                       lat.in = new.site$lat, lon.in = new.site$lon, 
                                       host = host, 
                                       overwrite = overwrite$download,
                                       site = site, username = username)
    if (met %in% c("CRUNCEP", "GFDL")) {
      ready.id <- raw.id
      # input_met$id overwrites ready.id below, needs to be populated here
      input_met$id <- raw.id
      stage$met2cf <- FALSE
      stage$standardize <- FALSE
    } else if (met %in% c("NOAA_GEFS")) { # Can sometimes have missing values, so the gapfilling step is required.
      cf.id <- raw.id
      input_met$id <-raw.id
      stage$met2cf <- FALSE
    }
  }
  
  #--------------------------------------------------------------------------------------------------#
  # Change to CF Standards
  if (stage$met2cf) {
    new.site.id <- ifelse(met %in% c("NARR"), register$siteid, site$id)
    
    cf.id <- .met2cf.module(raw.id = raw.id, 
                            register = register,
                            met = met, 
                            str_ns = str_ns, 
                            dir = dir, 
                            machine = machine, 
                            site.id = new.site.id, 
                            lat = new.site$lat, lon = new.site$lon, 
                            start_date = start_date, end_date = end_date, 
                            con = con, host = host, 
                            overwrite = overwrite$met2cf, 
                            format.vars = format.vars,
                            bety = bety)
  } else {
    cf.id = input_met$id
  }
  
  #--------------------------------------------------------------------------------------------------#
  # Change to Site Level - Standardized Met (i.e. ready for conversion to model specific format)
  if (stage$standardize) {
    standardize_result = list()
    
    for (i in 1:length(cf.id[[1]])) {
      if (register$scale == "regional") {
        #### Site extraction
        standardize_result[[i]] <- .extract.nc.module(cf.id = list(input.id = cf.id$input.id[i], dbfile.id = cf.id$dbfile.id[i]), 
                                       register = register, 
                                       dir = dir, 
                                       met = met, 
                                       str_ns = str_ns, 
                                       site = site, new.site = new.site, 
                                       con = con, 
                                       start_date = start_date, end_date = end_date, 
                                       host = host, 
                                       overwrite = overwrite$standardize)
                                       # Expand to support ensemble names in the future
      } else if (register$scale == "site") {
        ##### Site Level Processing
        standardize_result[[i]] <- .metgapfill.module(cf.id = list(input.id = cf.id$input.id[i], dbfile.id = cf.id$dbfile.id[i]), 
                                       register = register,
                                       dir = dir,
                                       met = met, 
                                       str_ns = str_ns, 
                                       site = site, new.site = new.site, 
                                       con = con, 
                                       start_date = start_date, end_date = end_date,
                                       host = host, 
                                       overwrite = overwrite$standardize,
                                       ensemble_name = i)
      }
      
    } # End for loop
    ready.id = list(input.id = NULL, dbfile.id = NULL)
    
    for (i in 1:length(standardize_result)) {
      ready.id$input.id <- c(ready.id$input.id, standardize_result[[i]]$input.id)
      ready.id$dbfile.id <- c(ready.id$dbfile.id, standardize_result[[i]]$dbfile.id)
    }
    
  } else {
    ready.id = input_met$id
  }
  
  #--------------------------------------------------------------------------------------------------#
  # Prepare for Model
  if (stage$met2model) {
    
    ## Get Model Registration
    reg.model.xml <- system.file(paste0("register.", model, ".xml"), package = paste0("PEcAn.",model))
    reg.model <- XML::xmlToList(XML::xmlParse(reg.model.xml))
    
    met2model.result = list()
    for (i in 1:length(ready.id[[1]])) {
      met2model.result[[i]] <- .met2model.module(ready.id = list(input.id = ready.id$input.id[i], dbfile.id = ready.id$dbfile.id[i]), 
                                    model = model, 
                                    con = con,
                                    host = host, 
                                    dir = dir, 
                                    met = met, 
                                    str_ns = str_ns,
                                    site = site, 
                                    start_date = start_date, end_date = end_date, 
                                    browndog = browndog, 
                                    new.site = new.site,
                                    overwrite = overwrite$met2model,
                                    exact.dates = reg.model$exact.dates,
                                    spin = spin,
                                    register = register,
                                    ensemble_name = i)
    }
    
    model.id = list()
    model.file.info = list()
    model.file = list()
    
    for (i in 1:length(met2model.result)) {
      model.id[[i]]  <- met2model.result[[i]]$model.id
      model.file.info[[i]] <- PEcAn.DB::db.query(paste0("SELECT * from dbfiles where id = ", model.id[[i]]$dbfile.id), con)
      model.file[[i]] <- file.path(model.file.info[[i]]$file_path, model.file.info[[i]]$file_name)
    }
    
    
    
    # met.process now returns the entire $met portion of settings, updated with parellel lists containing
    # the model-specific data files and their input ids.
    
    input_met$id <- list()
    input_met$path <- list()
    
    for (i in 1:length(model.id)) {
      input_met$id[[paste0("id", i)]] <- model.id[[i]]$input.id
      input_met$path[[as.character(paste0("path", i))]] <- model.file[[i]]
    }
    
    
  } else {
    # Because current ensemble data cannot reach this else statement, it only supports single source data.
    PEcAn.logger::logger.info("ready.id",ready.id,machine.host)
    model.id  <- PEcAn.DB::dbfile.check("Input", ready.id, con, hostname=machine.host)
    if(!(is.null(model.id)|length(model.id)==0)) {
      model.id$dbfile.id  <- model.id$id 
      model.file.info <- PEcAn.DB::db.query(paste0("SELECT * from dbfiles where id = ", model.id$dbfile.id), con)
      model.file <- file.path(model.file.info$file_path, model.file.info$file_name)
    } else {
      PEcAn.logger::logger.severe("Missing model id.")
    }
    
    input_met$path <- list() # for consistancy with the code in the if to this else.
    input_met$path$path1 <- model.file
    input_met$id <- model.id$container_id # This is the input id, whereas $id is the dbfile id.
    PEcAn.logger::logger.info("model.file = ",model.file,input_met)
  }
    
  return(input_met) # Returns an updated $met entry for the settings object.
} # met.process

################################################################################################################################# 

##' @name db.site.lat.lon
##' @title db.site.lat.lon
##' @export
##' @param site.id
##' @param con
##' @author Betsy Cowdery
db.site.lat.lon <- function(site.id, con) {
  site <- PEcAn.DB::db.query(paste("SELECT id, ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id =", 
                         site.id), con)
  if (nrow(site) == 0) {
   PEcAn.logger::logger.error("Site not found")
    return(NULL)
  }
  if (!(is.na(site$lat)) && !(is.na(site$lat))) {
    return(list(lat = site$lat, lon = site$lon))
  } else {
   PEcAn.logger::logger.severe("We should not be here!")
  }
} # db.site.lat.lon

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
##' @param username, used when downloading data from Ameriflux like sites
##' @param con, database connection
## 
##' @author Rob Kooper
browndog.met <- function(browndog, source, site, start_date, end_date, model, dir, username, con) {
  folder <- tempfile("BD-", dir)
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)
  
  if (source == "Ameriflux") {
    sitename <- sub(".*\\((.+)\\)", "\\1", site$name)
  } else if (source == "NARR") {
    sitename <- gsub("[\\s/()]", "-", site$name, perl = TRUE)
  } else {
   PEcAn.logger::logger.warn("Could not process source", source)
    return(invisible(NA))
  }
  
  # this logic should live somewhere else, maybe the registry?
  if (model == "SIPNET") {
    formatname <- "clim"
    outputfile <- file.path(folder, "sipnet.clim")
    results <- data.frame(file = outputfile, 
                          host = PEcAn.remote::fqdn(), 
                          mimetype = "text/csv",
                          formatname = "Sipnet.climna", 
                          startdate = start_date, enddate = end_date, 
                          dbfile.name = basename(outputfile), 
                          stringsAsFactors = FALSE)
  } else if (model == "ED2") {
    formatname <- "ed.zip"
    outputfile <- file.path(folder, "ed.zip")
    results <- data.frame(file = file.path(folder, "ED_MET_DRIVER_HEADER"), 
                          host = PEcAn.remote::fqdn(), 
                          mimetype = "text/plain", 
                          formatname = "ed.met_driver_header files format",
                          startdate = start_date, enddate = end_date, 
                          dbfile.name = "ED_MET_DRIVER_HEADER", 
                          stringsAsFactors = FALSE)
  } else if (model == "DALEC") {
    formatname <- "dalec"
    outputfile <- file.path(folder, "dalec.dat")
    results <- data.frame(file = outputfile, 
                          host = PEcAn.remote::fqdn(), 
                          mimetype = "text/plain", 
                          formatname = "DALEC meteorology", 
                          startdate = start_date, enddate = end_date, 
                          dbfile.name = basename(outputfile), 
                          stringsAsFactors = FALSE)
  } else if (model == "LINKAGES") {
    formatname <- "linkages"
    outputfile <- file.path(folder, "climate.txt")
    results <- data.frame(file = outputfile, 
                          host = PEcAn.remote::fqdn(), 
                          mimetype = "text/plain", 
                          formatname = "LINKAGES meteorology", 
                          startdate = start_date, enddate = end_date,
                          dbfile.name = basename(outputfile), 
                          stringsAsFactors = FALSE)
  } else if (model == "BIOCRO") {
    metinfo <- PEcAn.DB::db.query(paste0("select mimetypes.type_string, formats.name from mimetypes, formats, modeltypes, modeltypes_formats",
                               " where modeltype_id=modeltypes.id and format_id=formats.id and formats.mimetype_id=mimetypes.id",
                               " and tag='met' and modeltypes.name='", model, "'"), con)

    formatname <- tolower(paste0("met.", model))
    outputfile <- file.path(folder, "browndog_generated.out") # TODO is there a better name?
    results <- data.frame(file = outputfile, 
                          host = PEcAn.remote::fqdn(), 
                          mimetype = metinfo$type_string,
                          formatname = metinfo$name,
                          startdate = start_date, enddate = end_date,
                          dbfile.name = basename(outputfile), 
                          stringsAsFactors = FALSE)
  } else {
    PEcAn.logger::logger.warn("Could not process model", model)
    return(invisible(NA))
  }
  
  xmldata <- paste0("<input>", 
                    "<type>", source, "</type>", 
                    "<site>", sitename, "</site>",
                    "<lat>", site$lat, "</lat>", 
                    "<lon>", site$lon, "</lon>",
                    "<start_date>", start_date, "</start_date>", 
                    "<end_date>", end_date, "</end_date>", 
                    "<username>", username, "</username>",
                    "<model>", model, "</model>", 
                    "</input>")
  
  userpass <- paste(browndog$username, browndog$password, sep = ":")
  curloptions <- list(userpwd = userpass, httpauth = 1L, followlocation = TRUE)
  result <- RCurl::postForm(paste0(browndog$url, formatname, "/"),
                     fileData = RCurl::fileUpload("pecan.xml", xmldata, "text/xml"), .opts = curloptions)
  url <- gsub(".*<a.*>(.*)</a>.*", "\\1", result)
  PEcAn.logger::logger.info("browndog download url :", url)
  downloadedfile <- PEcAn.utils::download.url(url, outputfile, 600, curloptions)
  
  # fix returned data
  if (model == "ED2") {
    utils::unzip(downloadedfile, exdir = folder)
    # fix ED_MET_DRIVER_HEADER
    x <- readLines(results$file)
    x[3] <- ifelse(grepl("/$", folder), folder, paste0(folder, "/"))
    writeLines(x, results$file)
  } else {
    results$file <- downloadedfile
    results$dbfile.name <- basename(downloadedfile)
  }
  
  return(invisible(results))
} # browndog.met

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
##'   sitename = 'Rhinelander Aspen FACE Experiment (FACE-RHIN)'
##'   tag = 'FACE'
##'   site_from_tag(sitename,tag) = 'RHIN'
##' Requires that site names be set up specifically with (tag-sitecode) - this may change
site_from_tag <- function(sitename, tag) {
  temp <- regmatches(sitename, gregexpr("(?<=\\().*?(?=\\))", sitename, perl = TRUE))[[1]]
  pref <- paste0(tag, "-")
  return(unlist(strsplit(temp[grepl(pref, temp)], pref))[2])
} # site_from_tag
