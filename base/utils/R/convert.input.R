##' Convert input by applying fcn and insert new record into database
##'
##'
##' @name convert.input
##' @title convert.input
##' @export
##' @author Betsy Cowdery, Michael Dietze, Ankur Desai, Tony Gardella, Luke Dramko

convert.input <- function(input.id, outfolder, formatname, mimetype, site.id, start_date, 
                          end_date, pkg, fcn, con = con, host, browndog, write = TRUE, 
                          format.vars, overwrite = FALSE, exact.dates = FALSE, 
                          allow.conflicting.dates = TRUE, insert.new.file = FALSE, pattern = NULL,
                          forecast = FALSE, ensemble = FALSE,...) {
  input.args <- list(...)
  
  ### Debugging
  print("Entered convert.inputs")
  
  PEcAn.logger::logger.debug(paste("Convert.Inputs", fcn, input.id, host$name, outfolder, formatname, 
                     mimetype, site.id, start_date, end_date))
  
  # TODO see issue #18
  Rbinary <- ifelse(!exists("settings") || is.null(settings$host$Rbinary),"R",settings$host$Rbinary)
  
  n <- nchar(outfolder)
  if (substr(outfolder, n, n) != "/") {
    outfolder <- paste0(outfolder, "/")
  }
  
  outname <- utils::tail(unlist(strsplit(outfolder, "/")), n = 1)
  
  PEcAn.logger::logger.info(paste("start CHECK Convert.Inputs", fcn, input.id, host$name, outfolder, 
                    formatname, mimetype, site.id, start_date, end_date, forecast, ensemble))
  
  
  ##----------------------------------------------------------------------------------------------------------------##  
  # Forecast data sets require their own set of database checking operations, making the following else if and else irrelevant
  # for such data.  Forecast data are different if their start date (and if they are part of an ensemble, their ensemble id) are 
  # different.
  if (forecast) {
    #if the data is an ensemble, ensemble will be set equal to the number of ensemble members.
    #However, if the data is not an ensemble, ensemble will be equal to FALSE.  In order to treat ensemble and 
    #non-ensemble members together in one piece of code, we set ensemble=1 if it's FALSE.
    if (!is.integer(ensemble)) {ensemble = as.integer(1) }
    
    ### Just to check if this works...
    print(sprintf("ensemble = %d", ensemble))
    
    ### More deugging print statements
    print("$##$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$##$")
    print(sprintf("sideid = %s", site.id))
    print(sprintf("mimetype = %s", mimetype))
    print(sprintf("formatname = %s", formatname))
    print(sprintf("startdate = %s", start_date))
    print(sprintf("enddate = %s", end_date))
    
    # Convert dates to Date objects and strip all time zones
    # (DB values are timezone-free)
    start_date <- lubridate::force_tz(lubridate::as_datetime(start_date), "UTC")
    end_date   <- lubridate::force_tz(lubridate::as_datetime(end_date), "UTC")
    
    # Each ensemble member is associated with its own input file.  Therefore, each of these need to be lists.
    existing.dbfile <- list()
    existing.input <- list()
    
    existing_records <- list(input.id = NULL, dbfile.id = NULL)  # Empty vectors are null
    files.to.delete <- list()
    
    for (i in 1:ensemble) {
      ensemble_name <- paste(formatname, i, sep=".")
      existing.dbfile[[i]] <- PEcAn.DB::dbfile.input.check(siteid = site.id,
                                                             mimetype = mimetype, 
                                                             formatname = ensemble_name, 
                                                             parentid = input.id, 
                                                             startdate = start_date,
                                                             enddate = end_date, 
                                                             con = con, 
                                                             hostname = host$name, 
                                                             exact.dates = TRUE,
                                                             pattern = pattern)
      
      if(nrow(existing.dbfile[[i]]) > 0) {
        existing.input[[i]] <- PEcAn.DB::db.query(paste0("SELECT * FROM inputs WHERE id=", existing.dbfile[["container_id"]]),con)
        
        # Date/time processing for existing input
        existing.input[[i]]$start_date <- lubridate::force_tz(lubridate::as_datetime(existing.input$start_date), "UTC")
        existing.input[[i]]$end_date   <- lubridate::force_tz(lubridate::as_datetime(existing.input$end_date), "UTC")
        
        ## Obtain machine information
        #Grab machine info of file that exists
        existing.machine <- PEcAn.DB::db.query(paste0("SELECT * from machines where id  = '",
                                                      existing.dbfile$machine_id, "'"), con)
        
        #Grab machine info of host machine
        machine.host <- ifelse(host$name == "localhost", PEcAn.remote::fqdn(), host$name)
        machine <- PEcAn.DB::db.query(paste0("SELECT * from machines where hostname = '",
                                             machine.host, "'"), con)
        
        # If the files aren't on the machine, we have to download them, so "overwrite" is meaningless.
        if (existing.machine$id == machine$id) {
          if (overwrite) { #If the files are on the current machine, check to see if we should overwrite them.
            #Collect files for deletion, and store them in a list.
            #Actually setting up the deletion will be done after the loop.
            #c() concantanes the elements in a list, not just vectors.
            files.to.delete <- c(files.to.delete, as.list(PEcAn.remote::remote.execute.R( paste0("list.files('",
                                                                                                 existing.dbfile[["file_path"]],
                                                                                                 "', full.names=TRUE)"),
                                                                                          host, user = NA, verbose = TRUE,R = Rbinary, scratchdir = outfolder)))
          } else { # If we're not overriding, we can just use the files that are already here.
            existing_records$input.id = c(existing_records$input.id, existing.input$id)
            existing_records$dbfile.id = c(existing_records$dbfile.id, existing.dbfile$id)
          }
        } else { # Else to "existing.machine$id == machine$id"
          insert.new.file <- TRUE
        }
        
      } else { # Else to "nrow(existing.dbfile[[i]]) > 0)"
        existing.input[[i]] <- data.frame() # We don't want there to be a "gap" in existing input which would cause the lists to not be parellel.
                                            # Empty data frames are screened for when input/dbfile are processed below.
      }
    } # -- End for loop --
    
    # Set up files to be deleted.  The deletion will actually happen after the function finishes execution.  In case there
    # are any errors, this practice will make sure that the old files are preserved.
    if (length(files.to.delete) > 0) { # Each list item is a file to delete.
      file.deletion.commands <- .get.file.deletion.commands(unlist(files.to.delete)) 
      
      PEcAn.remote::remote.execute.R( file.deletion.commands$move.to.tmp,
                                      host, user = NA, 
                                      verbose = TRUE,R = Rbinary, scratchdir = outfolder)
      
      successful <- FALSE
      on.exit(if (exists("successful") && successful) {
        PEcAn.logger::logger.info("Conversion successful, with overwrite=TRUE. Deleting old files.")
        PEcAn.remote::remote.execute.R( file.deletion.commands$delete.tmp, 
                                        host, user = NA, 
                                        verbose = TRUE,  R = Rbinary, scratchdir = outfolder )
      } else {
        PEcAn.logger::logger.info("Conversion failed. Replacing old files.")
        PEcAn.remote::remote.execute.R( file.deletion.commands$replace.from.tmp, 
                                        host, user = NA, 
                                        verbose = TRUE, R = Rbinary, scratchdir = outfolder )
      }
      )#Close on.exit
    }
    
    # If all of the files for an existing ensemble exist, we'll just use those files.  Otherwise, we'll need to run the function to
    # fill in the gaps.  (The function should be smart enough not to overwrite previous files unless overwrite == TRUE).  If overwrite is TRUE,
    # then exisitng_records$input.id will have length 0, and this if statement won't be entered.
    if (length(existing_records$input.id) == ensemble) {
      if (ensemble == 1) { # Used to give a little more precise of an info message.
        PEcAn.logger::logger.info("File with forecast data in the given range already exists on this machine.")
      } else {
        PEcAn.logger::logger.info("Files for all ensemble members for this forecast already exist on this machine.")
      }
      
      ### Debugging statement covering possible functione exit.
      print("Exiting early")
      
      return(existing_records)
    }
    
    
    ### Print 
    print(existing.input)
    print("--------------------------------------------------")
    print(existing.dbfile)
    print("--------------------------------------------------")
    ##----------------------------------------- End of forecast section --------------------------------##
    
  } else if (exact.dates) {
    
    # Find Existing input with exact dates.
    
    existing.dbfile <- PEcAn.DB::dbfile.input.check(siteid = site.id,
                                          mimetype = mimetype, 
                                          formatname = formatname, 
                                          parentid = input.id, 
                                          startdate = start_date,
                                          enddate = end_date, 
                                          con = con, 
                                          hostname = host$name, 
                                          exact.dates = TRUE,
                                          pattern = pattern
                                         )
    
    
    PEcAn.logger::logger.debug("File id =", existing.dbfile$id,
                 " File name =", existing.dbfile$file_name,
                 " File path =", existing.dbfile$file_path,
                 " Input id =", existing.dbfile$container_id,
                 digits = 10)
    
    PEcAn.logger::logger.info("end CHECK for existing input record")
    
    
    if (nrow(existing.dbfile) > 0) {
      
      existing.input <- PEcAn.DB::db.query(paste0("SELECT * FROM inputs WHERE id=", existing.dbfile[["container_id"]]),con)
      
      # Convert dates to Date objects and strip all time zones
      # (DB values are timezone-free)
      start_date <- lubridate::force_tz(lubridate::as_date(start_date), "UTC")
      end_date   <- lubridate::force_tz(lubridate::as_date(end_date), "UTC")
      
      existing.input$start_date <- lubridate::force_tz(lubridate::as_date(existing.input$start_date), "UTC")
      existing.input$end_date   <- lubridate::force_tz(lubridate::as_date(existing.input$end_date), "UTC")
      
      ## Do overwrite if set to TRUE
      if(overwrite){
        # collect files to flag for deletion
        files.to.delete <- PEcAn.remote::remote.execute.R( paste0("list.files('",
                                                    existing.dbfile[["file_path"]],
                                                    "', full.names=TRUE)"),
                                             host, user = NA, verbose = TRUE,R = Rbinary, scratchdir = outfolder)
        
        file.deletion.commands <- .get.file.deletion.commands(files.to.delete)
        
        PEcAn.remote::remote.execute.R( file.deletion.commands$move.to.tmp,
                          host, user = NA, 
                          verbose = TRUE,R = Rbinary, scratchdir = outfolder)
        
        
        # Schedule files to be replaced or deleted on exiting the function
        successful <- FALSE
        on.exit(if (exists("successful") && successful) {
                PEcAn.logger::logger.info("Conversion successful, with overwrite=TRUE. Deleting old files.")
                PEcAn.remote::remote.execute.R( file.deletion.commands$delete.tmp, 
                                  host, user = NA, 
                                  verbose = TRUE,  R = Rbinary, scratchdir = outfolder )
        } else {
                PEcAn.logger::logger.info("Conversion failed. Replacing old files.")
                PEcAn.remote::remote.execute.R( file.deletion.commands$replace.from.tmp, 
                                  host, user = NA, 
                                  verbose = TRUE, R = Rbinary, scratchdir = outfolder )
        }
        )#Close on.exit
      }
      
      
      
      #Grab machine info of file that exists
      existing.machine <- PEcAn.DB::db.query(paste0("SELECT * from machines where id  = '",
                                          existing.dbfile$machine_id, "'"), con)
      
      #Grab machine info of host machine
      machine.host <- ifelse(host$name == "localhost", PEcAn.remote::fqdn(), host$name)
      machine <- PEcAn.DB::db.query(paste0("SELECT * from machines where hostname = '",
                                 machine.host, "'"), con)
      
      if (existing.machine$id != machine$id) {
        
        PEcAn.logger::logger.info("Valid Input record found that spans desired dates, but valid files do not exist on this machine.")
        PEcAn.logger::logger.info("Downloading all years of Valid input to ensure consistency")
        insert.new.file <- TRUE
        start_date <- existing.input$start_date
        end_date   <- existing.input$end_date
        
      } else {      
        # There's an existing input that spans desired start/end dates with files on this machine        
        PEcAn.logger::logger.info("Skipping this input conversion because files are already available.")
        return(list(input.id = existing.input$id, dbfile.id = existing.dbfile$id))
      }
      
      
    } else {
      # No existing record found. Should be good to go with regular conversion.
    }
    
    ##-------------------------end of exact.dates chunk------------------------------------#
    
  } else {
    
    existing.dbfile <- PEcAn.DB::dbfile.input.check(siteid = site.id,
                                          mimetype = mimetype, 
                                          formatname = formatname,
                                          parentid = input.id,
                                          startdate = start_date,
                                          enddate = end_date, 
                                          con = con, 
                                          hostname = host$name,
                                          pattern = pattern
                                         )
    
    PEcAn.logger::logger.debug("File id =", existing.dbfile$id,
                 " File name =", existing.dbfile$file_name,
                 " File path =", existing.dbfile$file_path,
                 " Input id =", existing.dbfile$container_id,
                 digits = 10)
    
    PEcAn.logger::logger.info("end CHECK for existing input record.")
    
    if (nrow(existing.dbfile) > 0) {
      
      existing.input <- PEcAn.DB::db.query(paste0("SELECT * FROM inputs WHERE id=", existing.dbfile[["container_id"]]),con)
      
      # Convert dates to Date objects and strip all time zones
      # (DB values are timezone-free)
      start_date <- lubridate::force_tz(lubridate::as_date(start_date), "UTC")
      end_date   <- lubridate::force_tz(lubridate::as_date(end_date), "UTC")
      
      existing.input$start_date <- lubridate::force_tz(lubridate::as_date(existing.input$start_date), "UTC")
      existing.input$end_date   <- lubridate::force_tz(lubridate::as_date(existing.input$end_date), "UTC")
      
      if (overwrite) {
        # collect files to flag for deletion
        
        files.to.delete <- PEcAn.remote::remote.execute.R( paste0("list.files('",
                                                    existing.dbfile[["file_path"]],
                                                    "', full.names=TRUE)"),
                                             host, user = NA, verbose = TRUE,R = Rbinary, scratchdir = outfolder)
        
        file.deletion.commands <- .get.file.deletion.commands(files.to.delete)
        
        PEcAn.remote::remote.execute.R( file.deletion.commands$move.to.tmp,
                          host, user = NA, 
                          verbose = TRUE,R = Rbinary, scratchdir = outfolder)
        
        # Schedule files to be replaced or deleted on exiting the function
        successful <- FALSE
        on.exit(if (exists("successful") && successful) {
          PEcAn.logger::logger.info("Conversion successful, with overwrite=TRUE. Deleting old files.")
          PEcAn.remote::remote.execute.R( file.deletion.commands$delete.tmp,
                            host, user = NA, 
                            verbose = TRUE,  R = Rbinary, scratchdir = outfolder )
          
        } else {
          
          PEcAn.logger::logger.info("Conversion failed. Replacing old files.")
          PEcAn.remote::remote.execute.R( file.deletion.commands$replace.from.tmp,
                            host, user = NA,
                            verbose = TRUE, R = Rbinary, scratchdir = outfolder )
        } 
        )#close on on.exit
        
      } else if ((start_date >= existing.input$start_date) &&
                 (end_date <= existing.input$end_date)) {
        
        #Grab machine info of file that exists
        existing.machine <- PEcAn.DB::db.query(paste0("SELECT * from machines where id  = '",
                                            existing.dbfile$machine_id, "'"), con)
        
        #Grab machine info of 
        machine.host <- ifelse(host$name == "localhost", PEcAn.remote::fqdn(), host$name)
        machine <- PEcAn.DB::db.query(paste0("SELECT * from machines where hostname = '",
                                   machine.host, "'"), con)
        
        if(existing.machine$id != machine$id){
          PEcAn.logger::logger.info("Valid Input record found that spans desired dates, but valid files do not exist on this machine.")
          PEcAn.logger::logger.info("Downloading all years of Valid input to ensure consistency")
          insert.new.file <- TRUE
          start_date <- existing.input$start_date
          end_date   <- existing.input$end_date
        } else {
          # There's an existing input that spans desired start/end dates with files on this machine           
          PEcAn.logger::logger.info("Skipping this input conversion because files are already available.")
          return(list(input.id = existing.input$id, dbfile.id = existing.dbfile$id))
        }
        
      } else {
        # Start/end dates need to be updated so that the input spans a continuous
        # timeframe
        start_date <- min(start_date, existing.input$start_date)
        end_date <- max(end_date, existing.input$end_date)
        PEcAn.logger::logger.info(
          paste0(
            "Changed start/end dates to '",
            start_date,
            "'/'",
            end_date,
            "' ",
            " so that existing input can be updated while maintaining continuous time span."
          )
        )
        
        # There might be existing files for some years (but not all; checked that above)
        # fcn should be smart enough not overwrite the existing ones, and hopefully won't
        # waste time working on them either At the end, if convert.inputs was successful
        # we'll need to update its start/end dates .
      } 
    } else {
      # No existing record found. Should be good to go.
    }
  }
  
  #---------------------------------------------------------------------------------------------------------------#
  # Get machine information
  
  machine.host <- ifelse(host$name == "localhost", PEcAn.remote::fqdn(), host$name)
  machine <- PEcAn.DB::db.query(paste0("SELECT * from machines where hostname = '",
                             machine.host, "'"), con)
  
  if (nrow(machine) == 0) {
    PEcAn.logger::logger.error("machine not found", host$name)
    return(NULL)
  }
  
  if (missing(input.id) || is.na(input.id) || is.null(input.id)) {
    input <- dbfile <- NULL
  } else {
    input <- PEcAn.DB::db.query(paste("SELECT * from inputs where id =", input.id), con)
    if (nrow(input) == 0) {
      PEcAn.logger::logger.error("input not found", input.id)
      return(NULL)
    }
    
    dbfile <- PEcAn.DB::db.query(paste("SELECT * from dbfiles where container_id =", input.id,
                             " and container_type = 'Input' and machine_id =", machine$id), con)
    if (nrow(dbfile) == 0) {
      PEcAn.logger::logger.error("dbfile not found", input.id)
      return(NULL)
    }
    if (nrow(dbfile) > 1) {
      PEcAn.logger::logger.warn("multiple dbfile records, using last", dbfile)
      dbfile <- dbfile[nrow(dbfile), ]
    }
  }
  
  #--------------------------------------------------------------------------------------------------#
  # Perform Conversion
  conversion <- "local.remote"  #default
  
  if (!is.null(browndog) && host$name == "localhost") {
    # perform conversions with Brown Dog - only works locally right now
    
    # Determine outputtype using formatname and mimetype of output file Add issue to
    # github that extension of formats table to include outputtype Convert to netcdf
    # - only using localhost
    if (mimetype == "application/x-netcdf") {
      outputtype <- "pecan.zip"
    } else {
      # Convert to model specific format
      if (formatname == "ed.met_driver_header_files_format" || formatname == 
          "ed.met_driver_header files format") {
        outputtype <- "ed.zip"
      } else if (formatname == "Sipnet.climna") {
        outputtype <- "clim"
      } else if (formatname == "DALEC meteorology") {
        outputtype <- "dalec.dat"
      } else if (formatname == "LINKAGES met") {
        outputtype <- "linkages.dat"
      } else {
        PEcAn.logger::logger.severe(paste("Unknown formatname", formatname))
      }
    }
    
    # create curl options
    if (!is.null(browndog$username) && !is.null(browndog$password)) {
      userpwd <- paste(browndog$username, browndog$password, sep = ":")
      curloptions <- list(userpwd = userpwd, httpauth = 1L)
    }
    curloptions <- c(curloptions, followlocation = TRUE)
    
    # check if we can do conversion
    out.html <- RCurl::getURL(paste0("http://dap-dev.ncsa.illinois.edu:8184/inputs/",
                              browndog$inputtype), .opts = curloptions)
    if (outputtype %in% unlist(strsplit(out.html, "\n"))) {
      PEcAn.logger::logger.info(paste("Conversion from", browndog$inputtype, "to", outputtype, 
                        "through Brown Dog"))
      conversion <- "browndog"
    }
  }
  
  if (conversion == "browndog") {
    url <- file.path(browndog$url, outputtype)
    
    # loop over files in localhost and zip to send to Brown Dog
    files <- list.files(dbfile$file_path, pattern = dbfile$file_name)
    files <- grep(dbfile$file_name, files, value = TRUE)
    zipfile <- paste0(dbfile$file_name, ".", browndog$inputtype)
    system(paste("cd", dbfile$file_path, "; zip", zipfile, paste(files, collapse = " ")))
    zipfile <- file.path(dbfile$file_path, zipfile)
    
    # check for and create output folder
    if (!file.exists(outfolder)) {
      dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
    }
    
    # post zipped file to Brown Dog
    html <- RCurl::postForm(url, fileData = RCurl::fileUpload(zipfile), .opts = curloptions)
    link <- XML::getHTMLLinks(html)
    file.remove(zipfile)
    
    # download converted file
    outfile <- file.path(outfolder, unlist(strsplit(basename(link), "_"))[2])
    download.url(url = link, file = outfile, timeout = 600, .opts = curloptions, retry404 = TRUE)
    
    # unzip downloaded file if necessary
    if (file.exists(outfile)) {
      if (utils::tail(unlist(strsplit(outfile, "[.]")), 1) == "zip") {
        fname <- utils::unzip(outfile, list = TRUE)$Name
        utils::unzip(outfile, files = fname, exdir = outfolder, overwrite = TRUE)
        file.remove(outfile)
      } else {
        fname <- list.files(outfolder)
      }
    }
    
    # settings$run$inputs$path <- outputfile 
    # what if there is more than 1 output file?
    rows <- length(fname)
    result <- data.frame(file = character(rows), 
                         host = character(rows), 
                         mimetype = character(rows), 
                         formatname = character(rows), 
                         startdate = character(rows), 
                         enddate = character(rows), 
                         stringsAsFactors = FALSE)
    for (i in seq_len(rows)) {
      old.file <- file.path(dbfile$file_path, files[i])
      new.file <- file.path(outfolder, fname[i])
      
      # create array with results
      result$file[i]       <- new.file
      result$host[i]       <- PEcAn.remote::fqdn()
      result$startdate[i]  <- paste(input$start_date, "00:00:00")
      result$enddate[i]    <- paste(input$end_date, "23:59:59")
      result$mimetype[i]   <- mimetype
      result$formatname[i] <- formatname
    }
  } else if (conversion == "local.remote") {
    # perform conversion on local or remote host
    
    fcn.args <- input.args
    fcn.args$overwrite  <- overwrite
    fcn.args$in.path    <- dbfile$file_path
    fcn.args$in.prefix  <- dbfile$file_name
    fcn.args$outfolder  <- outfolder
    fcn.args$start_date <- start_date
    fcn.args$end_date   <- end_date
    
    arg.string <- listToArgString(fcn.args)
    
    if (!missing(format.vars)) {
      arg.string <- paste0(arg.string, ", format=", paste0(list(format.vars)))
    }
    
    cmdFcn <- paste0(pkg, "::", fcn, "(", arg.string, ")")
    PEcAn.logger::logger.debug(paste0("convert.input executing the following function:\n", cmdFcn))
    
    ### temporarily removed for debugging since it takes a long time to execute
    ### result <- PEcAn.remote::remote.execute.R(script = cmdFcn, host, user = NA, verbose = TRUE, R = Rbinary, scratchdir = outfolder)
    
    ### save(result, file = "~/Tests/sample2.gefs.Rdata")
    load("~/Tests/sample2.gefs.Rdata")  ### For testing, because calling NOAA_GEFS every time will take a while.
    print("result saved.")
    
    # Wraps the result in a list.  This way, everything returned by fcn will be a list, and all of the 
    # code below can process everything as if it were a list without worrying about data types.
    if (is.data.frame(result)) {
      result <- list(result)  
    }
  }
  
  PEcAn.logger::logger.info("RESULTS: Convert.Input")
  ### PEcAn.logger::logger.info(result) ### Too much output
  ### PEcAn.logger::logger.info(names(result[[i]]))
  
  if (length(result[[1]]) <= 1){ # result, a list, is gauranteed to have at least one elemet.  However that element could be an empty data frame.
    PEcAn.logger::logger.debug(paste0("Processing data failed, please check validity of args:", arg.string))
    PEcAn.logger::logger.severe(paste0("Unable to process data using this function:",fcn))
  }
  
  #--------------------------------------------------------------------------------------------------#
  # Insert into Database
  outlist <- unlist(strsplit(outname, "_"))
  
  # Wrap in a list for consistant processing later
  print("Line 485")
  if (exists("existing.input") && is.data.frame(existing.input)) {
    print("Line 487")
    existing.input <- list(existing.input)
  }
  print("Line 480")
  
  existing.dbfile
  if (exists("existing.dbfile") && is.data.frame(existing.dbfile)) {
    print("Line 493")
    print(exists("existing.dbfile"))
    print("Good now...")
    print(existing.dbfile)
    print("Ok")
    existing.dbfile <- list(existing.dbfile)
  }
  
  print("Line 497")
  
  ######_______________________________________
  # New arrangement of database adding code to deal with ensembles.
  if (write) {
    # Setup newinput.  This list will contain two variables: a vector of input IDs and a vector of DB IDs for each entry in result.
    # This list is the thing that will be returned.
    newinput = list(input.id = NULL, dbfile.id = NULL) #Blank vectors are null.
    for(i in 1:length(result)) {  # Master for loop
      flag = TRUE
      
      print("Line 504")
      
      if (exists("existing.input") && nrow(existing.input[[i]]) > 0 && 
          (existing.input[[i]]$start_date != start_date || existing.input[[i]]$end_date != end_date)) {
        # Updating record with new dates
        PEcAn.DB::db.query(paste0("UPDATE inputs SET start_date='", start_date, "', end_date='",
                                  end_date, "'  WHERE id=", existing.input[[i]]$id), 
                           con)
        #Record has been updated and file downloaded so just return existing dbfile and input pair
        flag = FALSE
        ### Function updated to now only return valid data through one exit point.
        ### return(list(input.id = existing.input$id, dbfile.id = existing.dbfile$id))
        
        # The overall structure of this loop has been set up so that exactly one input.id and one dbfile.id will be written to newinput every interation.
        newinput$input.id <- c(newinput$input.id, new_entry$input.id)
        newinput$dbfile.id <- c(newinput$dbfile.id, new_entry$dbfile.id)
      }
      
      print("Line 522")
      
      if (overwrite) {
        if (exists("existing.input") && nrow(existing.input) > 0) {
            PEcAn.DB::db.query(paste0("UPDATE inputs SET name='", basename(dirname(result[[i]]$file[1])),
                                      "' WHERE id=", existing.input[[i]]$id), con)
          
        }
        
        if (exists("existing.dbfile") && nrow(existing.dbfile) > 0) {
            PEcAn.DB::db.query(paste0("UPDATE dbfiles SET file_path='", dirname(result[[i]]$file[1]),
                                      "', ", "file_name='", result[[i]]$dbfile.name[1], 
                                      "' WHERE id=", existing.dbfile[[i]]$id), con)
          
        }
      }
      
      print("Line 539")
      
      ### change from is.null to is.na?  download.raw.met.module.R calls this with NA, and is.null(NA) returns FALSE.
      parent.id <- ifelse(is.null(input[i]), NA, input[i]$id)  ### unfortunately, this will also have to be a list
      
      print("Line 544")
      
      if (insert.new.file && flag) {
        dbfile.id <- PEcAn.DB::dbfile.insert(in.path = dirname(result[[1]]$file[1]),
                                             in.prefix = result$dbfile.name[1], 
                                             'Input', existing.input$id, 
                                             con, reuse=TRUE, hostname = machine$hostname)
        newinput$input.id  <- c(newinput$input.id, existing.input$id)
        newinput$dbfile.id <- c(newinput$dbfile.id, dbfile.id)
      } else if (flag) {
        formatbase <- formatname
        if(ensemble - 1 > 0) {  #Each ensemble member gets their own separate input file in the database.
          formatbase <- paste(formatname, i, sep=".")
        }
        
        new_entry <- PEcAn.DB::dbfile.input.insert(in.path = dirname(result[[i]]$file[1]),
                                                   in.prefix = result[[i]]$dbfile.name[1], 
                                                   siteid = site.id, 
                                                   startdate = start_date,
                                                   enddate = end_date, 
                                                   mimetype, 
                                                   formatbase, 
                                                   parentid = parent.id,
                                                   con = con, 
                                                   hostname = machine$hostname,
                                                   allow.conflicting.dates = allow.conflicting.dates)
        newinput$input.id <- c(newinput$input.id, new_entry$input.id)
        newinput$dbfile.id <- c(newinput$dbfile.id, new_entry$dbfile.id)
      }
      
    }
    return(newinput)
  } else {
    ### Easy enough to copy and paste.
  }
  
  
  ##__________________________________________
  
  ## insert new record into database
  if (write) { # Defaults to TRUE
    if (exists("existing.input") && nrow(existing.input) > 0 && 
        (existing.input$start_date != start_date || existing.input$end_date != end_date)) {
      # Updating record with new dates
      PEcAn.DB::db.query(paste0("UPDATE inputs SET start_date='", start_date, "', end_date='",
                      end_date, "'  WHERE id=", existing.input$id), 
               con)
      #Record has been updated and file downloaded so just return existing dbfile and input pair
      return(list(input.id = existing.input$id, dbfile.id = existing.dbfile$id))
    }
    
    if (overwrite) {
      # A bit hacky, but need to make sure that all fields are updated to expected
      # values (i.e., what they'd be if convert.input was creating a new record)
      
      ### This should be updated to 
      ### a) wrap existing.input and existing.dbfile in a list, if they aren't already
      ### b) iterate over that list, even if length one.  (Currently, we're only interating over results.)
      if (exists("existing.input") && nrow(existing.input) > 0) {
        for (i in 1:length(result)) { # In most cases (when there's no ensemble) length(result) = 1, so this is exactly as if the loop weren't there
                                      # and the code was result$file[1]
          PEcAn.DB::db.query(paste0("UPDATE inputs SET name='", basename(dirname(result[[i]]$file[1])),
                                    "' WHERE id=", existing.input$id), con)
        }
      }
      if (exists("existing.dbfile") && nrow(existing.dbfile) > 0) {
        for (i in 1:length(result)) {
          PEcAn.DB::db.query(paste0("UPDATE dbfiles SET file_path='", dirname(result[[i]]$file[1]),
                                    "', ", "file_name='", result[[i]]$dbfile.name[1], 
                                    "' WHERE id=", existing.dbfile$id), con)
        }
      }
    }
    
    parent.id <- ifelse(is.null(input), NA, input$id)  ### unfortunately, this will also have to be a list
    
    if ("newsite" %in% names(input.args) && !is.null(input.args[["newsite"]])) {
      site.id <- input.args$newsite
    }
    
    if (insert.new.file) { #Defaults to FALSE
      dbfile.id <- PEcAn.DB::dbfile.insert(in.path = dirname(result[[1]]$file[1]),
                                 in.prefix = result$dbfile.name[1], 
                                 'Input', existing.input$id, 
                                 con, reuse=TRUE, hostname = machine$hostname)
      newinput <- list()
      newinput$input.id  <- existing.input$id
      newinput$dbfile.id <- dbfile.id 
    } else {
      
      formatbase = formatname
      
      #Iterate over every member of the ensemble and add their input.id and dbfile id's to parellel vectors in the list
      for (i in 1:length(result)) {
        
      }
    }
    
    ### Before, this function used to return a list with $input.id and $dbfile.id from some return points, and from others, a data frame of
    ### one row with columns $input.id and $dbfile.id, among others.  Now, it always returns a list with two vecotrs: $input.id and $dbfile.id
    
    successful <- TRUE
    return(newinput)
  } else {
    PEcAn.logger::logger.warn("Input was not added to the database")
    successful <- TRUE
    return(NULL)
  }
} # convert.input


.get.file.deletion.commands <- function(files.to.delete) {
  if(length(files.to.delete) > 0) {
    tmp.dirs <- file.path(unique(dirname(files.to.delete)), 'tmp')
    tmp.paths <- file.path(dirname(files.to.delete), 'tmp', basename(files.to.delete))
    
    tmp.dirs.string <- paste0("c(", paste(paste0("'", tmp.dirs, "'"), collapse=', '), ")")
    tmp.path.string <- paste0("c(", paste(paste0("'", tmp.paths, "'"), collapse=', '), ")")
    original.path.string <- paste0("c(", paste(paste0("'", files.to.delete, "'"), collapse=', '), ")")
    
    move.to.tmp <- paste0(
      "dir.create(", tmp.dirs.string, ", recursive=TRUE, showWarnings=FALSE); ",
      "file.rename(from=", original.path.string, ", to=", tmp.path.string, ")"
    ) 
    
    replace.from.tmp <- paste0(
      "file.rename(from=", tmp.path.string, ", to=", original.path.string, ");",
      "unlink(", tmp.dirs.string, ", recursive=TRUE)"
    )
    
    delete.tmp <- paste0(
      "unlink(", tmp.dirs.string, ", recursive=TRUE)"
    )
    return(list(move.to.tmp=move.to.tmp, replace.from.tmp=replace.from.tmp, delete.tmp=delete.tmp))
  } else {
    return(NULL)
  }
} # .get.file.deletion.commands
