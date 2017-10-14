##' Convert input by applying fcn and insert new record into database
##'
##'
##' @name convert.input
##' @title convert.input
##' @export
##' @author Betsy Cowdery, Michael Dietze, Ankur Desai, Tony Gardella

convert.input <- function(input.id, outfolder, formatname, mimetype, site.id, start_date, 
                          end_date, pkg, fcn, con = con, host, browndog, write = TRUE, 
                          format.vars, overwrite = FALSE, exact.dates = FALSE, 
                          allow.conflicting.dates = TRUE, insert.new.file = FALSE, pattern = NULL,...) {
  input.args <- list(...)
  
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
                    formatname, mimetype, site.id, start_date, end_date))
  
  
  ##----------------------------------------------------------------------------------------------------------------##  
  
  if (exact.dates) {
    
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
    
    result <- PEcAn.remote::remote.execute.R(script = cmdFcn, host, user = NA, verbose = TRUE, R = Rbinary, scratchdir = outfolder)
  }
  
  PEcAn.logger::logger.info("RESULTS: Convert.Input")
  PEcAn.logger::logger.info(result)
  PEcAn.logger::logger.info(names(result))
  
  if (length(result) <= 1){
    PEcAn.logger::logger.debug(paste0("Processing data failed, please check validity of args:", arg.string))
    PEcAn.logger::logger.severe(paste0("Unable to process data using this function:",fcn))
  }
  
  #--------------------------------------------------------------------------------------------------#
  # Insert into Database
  outlist <- unlist(strsplit(outname, "_"))
  
  ## insert new record into database
  if (write) {
    
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
      if (exists("existing.input") && nrow(existing.input) > 0) {
        PEcAn.DB::db.query(paste0("UPDATE inputs SET name='", basename(dirname(result$file[1])),
                        "' WHERE id=", existing.input$id), con)
      }
      if (exists("existing.dbfile") && nrow(existing.dbfile) > 0) {
        PEcAn.DB::db.query(paste0("UPDATE dbfiles SET file_path='", dirname(result$file[1]),
                        "', ", "file_name='", result$dbfile.name[1], 
                        "' WHERE id=", existing.dbfile$id), con)
      }
    }
    
    parent.id <- ifelse(is.null(input), NA, input$id)
    
    if ("newsite" %in% names(input.args) && !is.null(input.args[["newsite"]])) {
      site.id <- input.args$newsite
    }
    
    if (insert.new.file) {
      dbfile.id <- PEcAn.DB::dbfile.insert(in.path = dirname(result$file[1]),
                                 in.prefix = result$dbfile.name[1], 
                                 'Input', existing.input$id, 
                                 con, reuse=TRUE, hostname = machine$hostname)
      newinput <- list()
      newinput$input.id  <- existing.input$id
      newinput$dbfile.id <- dbfile.id 
    } else {
      newinput <- PEcAn.DB::dbfile.input.insert(in.path = dirname(result$file[1]),
                                      in.prefix = result$dbfile.name[1], 
                                      siteid = site.id, 
                                      startdate = start_date,
                                      enddate = end_date, 
                                      mimetype, 
                                      formatname, 
                                      parentid = parent.id,
                                      con = con, 
                                      hostname = machine$hostname,
                                      allow.conflicting.dates = allow.conflicting.dates)  
    }
    
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
