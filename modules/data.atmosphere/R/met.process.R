##' met.process
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
##' 
##'        `overwrite` may be a list with individual components corresponding to 
##'        `download`, `met2cf`, `standardize`, and `met2model`. If it is instead a simple boolean,
##'        the default behavior for `overwrite=FALSE` is to overwrite nothing, as you might expect.
##'        Note however that the default behavior for `overwrite=TRUE` is to overwrite everything
##'        *except* raw met downloads. I.e., it corresponds to:
##'
##'        list(download = FALSE, met2cf = TRUE, standardize = TRUE,  met2model = TRUE)
##'  List of `url`, `username`, `password`
##' @importFrom rlang .data .env
##' @export
##' @author Elizabeth Cowdery, Michael Dietze, Ankur Desai, James Simkins, Ryan Kelly
met.process <- function(site, input_met, start_date, end_date, model,
                        host = "localhost", dbparms, dir, spin=NULL,
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
      PEcAn.logger::logger.debug(overwrite)
      PEcAn.logger::logger.error(
        "If overwriting any stage of met.process, ",
        "all subsequent stages need to be overwritten too. Please correct.")
    }
  }
  
  # set up connection and host information
  con <- PEcAn.DB::db.open(dbparms)

  on.exit(PEcAn.DB::db.close(con), add = TRUE)
  username <- ifelse(is.null(input_met$username), "pecan", input_met$username)
  machine.host <- ifelse(host == "localhost" || host$name == "localhost", PEcAn.remote::fqdn(), host$name)
  machine <- PEcAn.DB::db.query(paste0("SELECT * from machines where hostname = '", machine.host, "'"), con)


  # read in registration xml for met specific information
  register.xml <- system.file(paste0("registration/register.", met, ".xml"), package = "PEcAn.data.atmosphere")
  register     <- read.register(register.xml, con)
  # first attempt at function that designates where to start met.process
  if (is.null(input_met$id)) {
    stage <- list(download.raw = TRUE, met2cf = TRUE, standardize = TRUE, met2model = TRUE)
    format.vars <- PEcAn.DB::query.format.vars(bety = con, format.id = register$format$id)  # query variable info from format id
  } else {
    stage <- met.process.stage(input.id=input_met$id, raw.id=register$format$id, con)
    format.vars <- PEcAn.DB::query.format.vars(bety = con, input.id = input_met$id)  # query DB to get format variable information if available
    # Is there a situation in which the input ID could be given but not the file path? 
    # I'm assuming not right now
    assign(stage$id.name,
           list(
             input.id = input_met$id,
             dbfile.id = PEcAn.DB::dbfile.check("Input", input_met$id, hostname = machine.host, con =
                                                 con)$id
           ))
  }
  #--- If the met source is local then there is no need for download
  if (!is.null(register$Local)){
    if (as.logical(register$Local)) {
      stage$download.raw <- FALSE
      stage$local <- TRUE
    }
  }else{
    stage$local <- FALSE
  }
  
  PEcAn.logger::logger.debug(stage)
  
  if(is.null(model)){
    stage$model <- FALSE
  }
  
  
  # setup site database number, lat, lon and name and copy for format.vars if new input
  latlon <- PEcAn.DB::query.site(site$id, con = con)[c("lat", "lon")] 
  new.site <- data.frame(id = as.numeric(site$id), 
                         lat = latlon$lat, 
                         lon = latlon$lon)
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
  # Or met source is either downloadable or it's local .
  # Download raw met
  if (stage$download.raw) {
    raw.data.site.id <- ifelse(is.null(register$siteid), new.site$id, register$siteid)
    str_ns_download <- ifelse(is.null(register$siteid), str_ns, register$siteid)

    raw.id <- .download.raw.met.module(
      dir = dir,
      met = met,
      register = register,
      machine = machine,
      start_date = start_date,
      end_date = end_date,
      str_ns = str_ns_download,
      con = con,
      input_met = input_met,
      site.id = raw.data.site.id,
      lat.in = new.site$lat,
      lon.in = new.site$lon,
      host = host,
      overwrite = overwrite$download,
      site = site,
      username = username,
      dbparms=dbparms
    )
    
    if (met %in% c("CRUNCEP", "GFDL", "NOAA_GEFS", "MERRA")) {
      ready.id <- raw.id
      # input_met$id overwrites ready.id below, needs to be populated here
      input_met$id <- raw.id
      stage$met2cf <- FALSE
      stage$standardize <- FALSE
    } 
  }else if (stage$local){ # In parallel to download met module this needs to check if the files are already downloaded or not 

    db.file <- PEcAn.DB::dbfile.input.check(
      siteid=new.site$id %>% as.character(),
      startdate = start_date %>% as.Date,
      enddate = end_date  %>% as.Date,
      parentid = NA,
      mimetype="application/x-netcdf",
      formatname="CF Meteorology",
      con,
      hostname = PEcAn.remote::fqdn(),
      exact.dates = TRUE,
#      pattern = met,
      return.all=TRUE
    ) 
    # If we already had the met downloaded for this site  
    if (nrow(db.file) >0 ){
      cf.id <- raw.id <- db.file
    }else{ 
      # I did this bc dbfile.input.check does not cover the between two time periods situation
      mimetypeid <- PEcAn.DB::get.id(table = "mimetypes", colnames = "type_string", 
                           values = "application/x-netcdf", con = con)

      formatid <- PEcAn.DB::get.id(table = "formats", colnames = c("mimetype_id", "name"),
                         values = c(mimetypeid, "CF Meteorology"), con = con)
      
      machine.id <- PEcAn.DB::get.id(table = "machines", "hostname", PEcAn.remote::fqdn(), con)
      # Finding the tiles.
      raw.tiles <- dplyr::tbl(con, "inputs") %>%
        dplyr::filter(
          .data$site_id == register$ParentSite,
          .data$start_date <= .env$start_date,
          .data$end_date >= .env$end_date,
          .data$format_id == formatid
        ) %>%
        dplyr::filter(grepl(met, "name")) %>%
        dplyr::inner_join(dplyr::tbl(con, "dbfiles"), by = c('id' = 'container_id')) %>%
        dplyr::filter(.data$machine_id == machine.id) %>%
        dplyr::collect()
      
      cf.id <- raw.id <- list(input.id = raw.tiles$id.x, dbfile.id = raw.tiles$id.y)
    }
    
    stage$met2cf <- FALSE 
    stage$standardize <- TRUE
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
                            bety = con)
  } else {
   if (! met %in% c("ERA5", "FieldObservatory")) cf.id = input_met$id
  }

  #--------------------------------------------------------------------------------------------------#
  # Change to Site Level - Standardized Met (i.e. ready for conversion to model specific format)
  if (stage$standardize) {
    id_stdized <- list()
    ready.id <- list(input.id = NULL, dbfile.id = NULL)

    for (i in seq_along(cf.id[[1]])) {

      if (register$scale == "regional") {
        #### Site extraction
        id_stdized <- .extract.nc.module(cf.id = list(input.id = cf.id$container_id[i],
                                                                   dbfile.id = cf.id$id[i]), 
                                       register = register, 
                                       dir = dir, 
                                       met = met, 
                                       str_ns = str_ns, 
                                       site = site,
                                       new.site = new.site, 
                                       con = con, 
                                       start_date = start_date,
                                       end_date = end_date, 
                                       host = host, 
                                       overwrite = overwrite$standardize)
                                       # Expand to support ensemble names in the future
      } else if (register$scale == "site") {
        ##### Site Level Processing
        id_stdized <- .metgapfill.module(cf.id = list(input.id = cf.id$input.id[i], dbfile.id = cf.id$dbfile.id[i]), 
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
      } else {
        # No action taken. These ids will be dropped from ready.id
        id_stdized <- NULL
      }

      ready.id$input.id <- c(ready.id$input.id, id_stdized$input.id)
      ready.id$dbfile.id <- c(ready.id$dbfile.id, id_stdized$dbfile.id)
      
    } # End for loop
    
  } else {
    ready.id <- input_met$id
  }

  #--------------------------------------------------------------------------------------------------#
  # Prepare for Model
  if (stage$met2model) {
    
    ## Get Model Registration
    reg.model.xml <- system.file(paste0("register.", model, ".xml"), package = paste0("PEcAn.",model))
    reg.model <- XML::xmlToList(XML::xmlParse(reg.model.xml))
    
      met2model.result = list()
    for (i in seq_along(ready.id[[1]])) {
      met2model.result[[i]] <- .met2model.module(ready.id = list(input.id = ready.id$input.id[i], dbfile.id = ready.id$dbfile.id[i]), 
                                    model = model, 
                                    con = con,
                                    host = host, 
                                    dir = dir, 
                                    met = met, 
                                    str_ns = str_ns,
                                    site = site, 
                                    start_date = start_date,
                                    end_date = end_date, 
                                    new.site = new.site,
                                    overwrite = overwrite$met2model,
                                    exact.dates = reg.model$exact.dates,
                                    spin = spin,
                                    register = register,
                                    ensemble_name = i)
      }

    model.id <- list()
    model.file.info <- list()
    model.file <- list()

    for (i in seq_along(met2model.result)) {
      model.id[[i]]  <- met2model.result[[i]]$model.id
      model.file.info[[i]] <- PEcAn.DB::db.query(paste0("SELECT * from dbfiles where id = ", model.id[[i]]$dbfile.id), con)
      model.file[[i]] <- file.path(model.file.info[[i]]$file_path, model.file.info[[i]]$file_name)
    }
    
  
    
    # met.process now returns the entire $met portion of settings, updated with parellel lists containing
    # the model-specific data files and their input ids.
    
    input_met$id <- list()
    input_met$path <- list()

    for (i in seq_along(model.id)) {
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

##' Function to find the site code for a specific tag
##'
##' Example:
##'   sitename = 'Rhinelander Aspen FACE Experiment (FACE-RHIN)'
##'   tag = 'FACE'
##'   site_from_tag(sitename,tag) = 'RHIN'
##' Requires that site names be set up specifically with (tag-sitecode) - this may change
##'
##' @param sitename full name of site
##' @param tag abbreviated name of site
##' @author Betsy Cowdery
##'
##' @export
site_from_tag <- function(sitename, tag) {
  temp <- regmatches(sitename, gregexpr("(?<=\\().*?(?=\\))", sitename, perl = TRUE))[[1]]
  pref <- paste0(tag, "-")
  return(unlist(strsplit(temp[grepl(pref, temp)], pref))[2])
} # site_from_tag
