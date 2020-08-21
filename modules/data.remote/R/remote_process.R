##' call rp_control (from RpTools Python package) and store the output in BETY
##'
##' @name remote_process
##' @title remote_process
##' @export
##'
##' @param settings PEcAn settings list containing remotedata tags: raw_mimetype, raw_formatname, source, collection, scale, projection, qc, algorithm, credfile, pro_mimetype, pro_formatname, out_get_data, out_process_data, overwrite
##'
##' @examples
##' \dontrun{
##' remote_process(settings)
##' }
##' @author Ayush Prasad, Istem Fer
##'

remote_process <- function(settings) {
  # Information about the date variables used in remote_process:
  # req_start, req_end : start, end dates requested by the user, the user does not have to be aware about the status of the requested file in the DB
  # start, end : effective start, end dates created after checking the DB status. These dates are sent to rp_control for downloading and processing data
  # write_raw_start, write_raw_end : start, end dates which are used while inserting and updating the DB
  # the "pro" version of these variables have the same meaning and are used to refer to the processed file
  
  # The value of remotefile_check_flag denotes the following cases:
  
  # When processed file is requested,
  # 1 - There are no existing raw and processed files of the requested type in the DB
  # 2 - Requested processed file does not exist, the raw file used to create is it present and matches with the requested daterange
  # 3 - Requested processed file does not exist, raw file used to create it is present but has to be updated to match with the requested daterange
  # 4 - Both processed and raw file of the requested type exists, but they have to be updated to match with the requested daterange
  # 5 - Raw file required for creating the processed file exists with the required daterange and the processed file needs to be updated. Here the new processed file will now contain data for the entire daterange of the existing raw file
  # 6 - There is a existing processed file of the requested type but the raw file used to create it has been deleted. Here, the raw file will be created again and the processed file will be replaced entirely with the one created from new raw file
  
  # When raw file is requested,
  # 1 - There is no existing raw the requested type in the DB
  # 2 - existing raw file will be updated
  
  RpTools <- reticulate::import("RpTools")
  
  # extract the variables from the settings list
  siteid           <- as.numeric(settings$run$site$id)
  siteid_short     <- paste0(siteid %/% 1e+09, "-", siteid %% 1e+09)
  raw_mimetype     <- settings$remotedata$raw_mimetype
  raw_formatname   <- settings$remotedata$raw_formatname
  outdir           <- settings$database$dbfiles
  start            <- as.character(as.Date(settings$run$start.date))
  end              <- as.character(as.Date(settings$run$end.date))
  source           <- settings$remotedata$source
  collection       <- settings$remotedata$collection
  scale            <- settings$remotedata$scale
  if (!is.null(scale)) {
    scale <- as.double(settings$remotedata$scale)
    scale <- format(scale, nsmall = 1)
  }
  projection       <- settings$remotedata$projection
  qc               <- settings$remotedata$qc
  if (!is.null(qc)) {
    qc <- as.double(settings$remotedata$qc)
    qc <- format(qc, nsmall = 1)
  }
  algorithm        <- settings$remotedata$algorithm
  credfile         <- settings$remotedata$credfile
  pro_mimetype     <- settings$remotedata$pro_mimetype
  pro_formatname   <- settings$remotedata$pro_formatname
  out_get_data     <- settings$remotedata$out_get_data
  out_process_data <- settings$remotedata$out_process_data
  overwrite        <- settings$remotedata$overwrite
  if (is.null(overwrite)) {
    overwrite <- FALSE
  }
  
  
  PEcAn.logger::severeifnot("Check if siteid is of numeric type and is not NULL",
                            is.numeric(siteid))
  PEcAn.logger::severeifnot("Check if outdir is of character type and is not NULL",
                            is.character(outdir))
  PEcAn.logger::severeifnot("Check if source is of character type and is not NULL",
                            is.character(source))
  # PEcAn.logger::severeifnot("Source should be one of gee, appeears", source == "gee" || source == "appeears")
  # collection validation to be implemented
  if (!is.null(projection)) {
    PEcAn.logger::severeifnot("projection should be of character type",
                              is.character(projection))
  }
  if (!is.null(algorithm)) {
    PEcAn.logger::severeifnot("algorithm should be of character type",
                              is.character(algorithm))
  }
  if (!is.null(credfile)) {
    PEcAn.logger::severeifnot("credfile should be of character type",
                              is.character(credfile))
  }
  PEcAn.logger::severeifnot(
    "Check if out_get_data is of character type and is not NULL",
    is.character(out_get_data)
  )
  if (!is.null(out_process_data)) {
    PEcAn.logger::severeifnot("out_process_data should be of character type",
                              is.character(out_process_data))
  }
  
  
  dbcon <- PEcAn.DB::db.open(settings$database$bety)
  on.exit(PEcAn.DB::db.close(dbcon), add = TRUE)
  
  # collection dataframe used to map Google Earth Engine collection names to their PEcAn specific names
  collection_lut <- data.frame(
    stringsAsFactors = FALSE,
    original_name = c(
      "LANDSAT/LC08/C01/T1_SR",
      "COPERNICUS/S2_SR",
      "NASA_USDA/HSL/SMAP_soil_moisture"
    ),
    pecan_code = c("l8", "s2", "smap")
  )
  getpecancode <- collection_lut$pecan_code
  names(getpecancode) <- collection_lut$original_name
  
  if (source == "gee") {
    collection = unname(getpecancode[collection])
  }
  
  # construct raw file name
  raw_file_name <-
    construct_raw_filename(collection, siteid_short, scale, projection, qc)
  
  
  # check if any data is already present in the inputs table
  dbstatus <-
    remotedata_db_check(
      raw_file_name     = raw_file_name,
      start             = start,
      end               = end,
      siteid            = siteid,
      siteid_short      = siteid_short,
      out_get_data      = out_get_data,
      algorithm         = algorithm,
      out_process_data  = out_process_data,
      overwrite         = overwrite,
      dbcon             = dbcon
    )
  
  remotefile_check_flag  <- dbstatus$remotefile_check_flag 
  start                  <- dbstatus$start                 
  end                    <- dbstatus$end                   
  stage_get_data         <- dbstatus$stage_get_data        
  write_raw_start        <- dbstatus$write_raw_start       
  write_raw_end          <- dbstatus$write_raw_end         
  raw_merge              <- dbstatus$raw_merge             
  existing_raw_file_path <- dbstatus$existing_raw_file_path
  stage_process_data     <- dbstatus$stage_process_data    
  write_pro_start        <- dbstatus$write_pro_start       
  write_pro_end          <- dbstatus$write_pro_end         
  pro_merge              <- dbstatus$pro_merge             
  input_file             <- dbstatus$input_file            
  existing_pro_file_path <- dbstatus$existing_pro_file_path
  raw_check              <- dbstatus$raw_check             
  pro_check              <- dbstatus$pro_check  

  
  # construct outdir path
  outdir <-
    file.path(outdir, paste(toupper(source), "site", siteid_short, sep = "_"))

  # extract the AOI of the site from BETYdb
  coords <-
    unlist(PEcAn.DB::db.query(
      sprintf("select ST_AsGeoJSON(geometry) from sites where id=%f", siteid),
      con = dbcon
    ), use.names = FALSE)
  
  fcn.args <- list()
  fcn.args$coords                 <- coords
  fcn.args$outdir                 <- outdir
  fcn.args$start                  <- start
  fcn.args$end                    <- end
  fcn.args$source                 <- source
  fcn.args$collection             <- collection
  fcn.args$siteid                 <- siteid_short
  fcn.args$scale                  <- as.double(scale)
  fcn.args$projection             <- projection
  fcn.args$qc                     <- as.double(qc)
  fcn.args$algorithm              <- algorithm
  fcn.args$input_file             <- input_file
  fcn.args$credfile               <- credfile
  fcn.args$out_get_data           <- out_get_data
  fcn.args$out_process_data       <- out_process_data
  fcn.args$stage_get_data         <- stage_get_data
  fcn.args$stage_process_data     <- stage_process_data
  fcn.args$raw_merge              <- raw_merge
  fcn.args$pro_merge              <- pro_merge
  fcn.args$existing_raw_file_path <- existing_raw_file_path
  fcn.args$existing_pro_file_path <- existing_pro_file_path
  
  
  arg.string <- PEcAn.utils::listToArgString(fcn.args)
  
  cmdFcn <- paste0("RpTools$rp_control(", arg.string, ")")
  PEcAn.logger::logger.debug(paste0("Remote module executing the following function:\n", cmdFcn))
  
  # call rp_control
  output <- do.call(RpTools$rp_control, fcn.args)

  # insert output data in the DB
  db_out <-
    remotedata_db_insert(
      output                = output,
      remotefile_check_flag = remotefile_check_flag,
      siteid                = siteid,
      out_get_data          = out_get_data,
      out_process_data      = out_process_data,
      write_raw_start       = write_raw_start,
      write_raw_end         = write_raw_end,
      write_pro_start       = write_pro_start,
      write_pro_end         = write_pro_end,
      raw_check             = raw_check,
      pro_check             = pro_check,
      raw_mimetype          = raw_mimetype,
      raw_formatname        = raw_formatname,
      pro_mimetype          = pro_mimetype,
      pro_formatname        = pro_formatname,
      dbcon                 = dbcon
    )
  
  
  # return the ids and paths of the inserted data
  if (!is.null(out_get_data)) {
    settings$remotedata$raw_id   <- db_out[[1]]
    settings$remotedata$raw_path <- db_out[[2]]
  }
  if (!is.null(out_process_data)) {
    settings$remotedata$pro_id   <- db_out[[3]]
    settings$remotedata$pro_path <- db_out[[4]]
  }
  
  return (settings)
}



##' construct the raw file name
##'
##' @name construct_raw_filename
##' @title construct_raw_filename
##' @param collection collection or product requested from the source
##' @param siteid shortform of siteid
##' @param scale scale, NULL by default
##' @param projection projection, NULL by default
##' @param qc qc_parameter, NULL by default
##' @return raw_file_name
##' @examples
##' \dontrun{
##' raw_file_name <- construct_raw_filename(
##'   collection="s2",
##'   siteid="0-721",
##'   scale=10.0
##'   projection=NULL
##'   qc=1.0)
##' }
##' @author Ayush Prasad
construct_raw_filename <-
  function(collection,
           siteid,
           scale = NULL,
           projection = NULL,
           qc = NULL) {
    # use NA if a parameter is not applicable and is NULL
    if (is.null(scale)) {
      scale <- "NA"
    } else{
      scale <- format(scale, nsmall = 1)
    }
    if (is.null(projection)) {
      projection <- "NA"
    }
    if (is.null(qc)) {
      qc <- "NA"
    } else{
      qc <- format(qc, nsmall = 1)
    }
    raw_file_name <-
      paste(collection, scale, projection, qc, "site", siteid, sep = "_")
    return(raw_file_name)
  }




##' set dates, stage and merge status for remote data download
##'
##' @name set_stage
##' @title set_stage
##' @param result dataframe containing id, site_id, name, start_date, end_date from inputs table and file_path from dbfiles table
##' @param req_start start date requested by the user
##' @param req_end end date requested by the user
##' @param stage the stage which needs to be set, get_remote_data or process_remote_data
##' @return list containing req_start, req_end, stage, merge, write_start, write_end
##' @examples
##' \dontrun{
##' raw_check <- set_stage(
##'   result,
##'   req_start,
##'   req_end,
##'   get_remote_data)
##' }
##' @author Ayush Prasad
set_stage   <- function(result, req_start, req_end, stage) {
  db_start  <- as.Date(result$start_date)
  db_end    <- as.Date(result$end_date)
  req_start <- as.Date(req_start)
  req_end   <- as.Date(req_end)
  stage     <- TRUE
  merge     <- TRUE
  
  # data already exists
  if ((req_start >= db_start) && (req_end <= db_end)) {
    req_start   <- "dont write"
    req_end     <- "dont write"
    stage       <- FALSE
    merge       <- FALSE
    write_start <- "dont write"
    write_end   <- "dont write"
  } else if (req_start < db_start && db_end < req_end) {
    # data has to be replaced
    merge       <- "replace"
    write_start <- req_start
    write_end   <- req_end
    stage       <- TRUE
  } else if ((req_start > db_start) && (req_end > db_end)) {
    # forward case
    req_start   <- db_end + 1
    write_start <- db_start
    write_end   <- req_end
  } else if ((req_start < db_start) && (req_end < db_end)) {
    # backward case
    req_end     <- db_start - 1
    write_end   <- db_end
    write_start <- req_start
  }
  return (list(req_start, req_end, stage, merge, write_start, write_end))
  
}




##' check the status of the requested data in the DB
##'
##' @name  remotedata_db_check
##' @title remotedata_db_check
##' @param raw_file_name raw_file_name
##' @param start start date requested by user
##' @param end end date requested by the user
##' @param siteid siteid of the site 
##' @param siteid_short short form of the siteid
##' @param out_get_data out_get_data
##' @param algorithm algorithm
##' @param out_process_data out_process_data
##' @param overwrite overwrite
##' @param dbcon  BETYdb con
##' @return list containing remotefile_check_flag, start, end, stage_get_data, write_raw_start, write_raw_end, raw_merge, existing_raw_file_path, stage_process_data, write_pro_start, write_pro_end, pro_merge, input_file, existing_pro_file_path, raw_check, pro_check
##' @examples
##' \dontrun{
##' dbstatus <- remotedata_db_check(
##'   raw_file_name,
##'   start,
##'   end,
##'   siteid,
##'   siteid_short,
##'   out_get_data,
##'   algorithm,
##'   out_process_data,
##'   overwrite
##'   dbcon)
##' }
##' @author Ayush Prasad
remotedata_db_check <-
  function(raw_file_name,
           start,
           end,
           siteid,
           siteid_short,
           out_get_data,
           algorithm,
           out_process_data,
           overwrite,
           dbcon) {
    
    # Information about the date variables used:
    # req_start, req_end : start, end dates requested by the user, the user does not have to be aware about the status of the requested file in the DB
    # start, end : effective start, end dates created after checking the DB status. These dates are sent to rp_control for downloading and processing data
    # write_raw_start, write_raw_end : start, end dates which are used while inserting and updating the DB
    # the "pro" version of these variables have the same meaning and are used to refer to the processed file
    
    req_start              <- start
    req_end                <- end
    input_file             <- NULL
    stage_get_data         <- NULL
    stage_process_data     <- NULL
    raw_merge              <- NULL
    pro_merge              <- NULL
    existing_raw_file_path <- NULL
    existing_pro_file_path <- NULL
    write_raw_start        <- NULL
    write_raw_end          <- NULL
    write_pro_start        <- NULL
    write_pro_end          <- NULL
    raw_check              <- NULL
    pro_check              <- NULL
    remotefile_check_flag  <- NULL
    
    existing_data <-
      PEcAn.DB::db.query(paste0("SELECT * FROM inputs WHERE site_id=", siteid), dbcon)
    if (nrow(existing_data) >= 1) {
      if (overwrite) {
        PEcAn.logger::logger.warn("overwrite is set to TRUE, any existing file will be entirely replaced")
        if (!is.null(out_process_data)) {
          pro_file_name = paste0(algorithm,
                                 "_",
                                 out_process_data,
                                 "_site_",
                                 siteid_short)
          if (nrow(pro_check <-
                   PEcAn.DB::db.query(
                     sprintf(
                       "SELECT inputs.id, inputs.site_id, inputs.name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.name=dbfiles.file_name AND inputs.name LIKE '%s%%';",
                       pro_file_name
                     ),
                     dbcon
                   )) == 1) {
            if (nrow(raw_check <-
                     PEcAn.DB::db.query(
                       sprintf(
                         "SELECT inputs.id, inputs.site_id, inputs.name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.name=dbfiles.file_name AND inputs.name LIKE '%s%%';",
                         raw_file_name
                       ),
                       dbcon
                     )) == 1) {
              remotefile_check_flag <- 4
            } else{
              remotefile_check_flag <- 6
            }
          } else{
            remotefile_check_flag <- 1
          }
          stage_process_data <- TRUE
          pro_merge          <- "repace"
          write_pro_start    <- start
          write_pro_end      <- end
        } else if (!is.null(out_get_data)) {
          if (nrow(raw_check <-
                   PEcAn.DB::db.query(
                     sprintf(
                       "SELECT inputs.id, inputs.site_id, inputs.name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.name=dbfiles.file_name AND inputs.name LIKE '%s%%';",
                       raw_file_name
                     ),
                     dbcon
                   )) == 1) {
            remotefile_check_flag <- 2
          } else{
            remotefile_check_flag <- 1
          }
        }
        stage_get_data         <- TRUE
        start                  <- req_start
        end                    <- req_end
        write_raw_start        <- start
        write_raw_end          <- end
        raw_merge              <- "replace"
        existing_pro_file_path <- NULL
        existing_raw_file_path <- NULL
      } else if (!is.null(out_process_data)) {
        # if processed data is requested, example LAI
        
        # construct processed file name
        pro_file_name = paste0(algorithm, "_", out_process_data, "_site_", siteid_short)
        
        # check if processed file exists
        if (nrow(pro_check <-
                 PEcAn.DB::db.query(
                   sprintf(
                     "SELECT inputs.id, inputs.site_id, inputs.name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.name=dbfiles.file_name AND inputs.name LIKE '%s%%';",
                     pro_file_name
                   ),
                   dbcon
                 )) == 1) {
          datalist <-
            set_stage(pro_check, req_start, req_end, stage_process_data)
          pro_start       <- as.character(datalist[[1]])
          pro_end         <- as.character(datalist[[2]])
          write_pro_start <- datalist[[5]]
          write_pro_end   <- datalist[[6]]
          if (pro_start != "dont write" || pro_end != "dont write") {
            stage_process_data <- datalist[[3]]
            pro_merge <- datalist[[4]]
            if (pro_merge == TRUE) {
              existing_pro_file_path <- pro_check$file_path
            }
            if (stage_process_data == TRUE) {
              # check about the status of raw file
              raw_check <-
                PEcAn.DB::db.query(
                  sprintf(
                    "SELECT inputs.id, inputs.site_id, inputs.name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.name=dbfiles.file_name AND inputs.name LIKE '%s%%';",
                    raw_file_name
                  ),
                  dbcon
                )
              if (!is.null(raw_check$start_date) &&
                  !is.null(raw_check$end_date)) {
                raw_datalist <-
                  set_stage(raw_check, pro_start, pro_end, stage_get_data)
                start           <- as.character(raw_datalist[[1]])
                end             <- as.character(raw_datalist[[2]])
                write_raw_start <- raw_datalist[[5]]
                write_raw_end   <- raw_datalist[[6]]
                stage_get_data  <- raw_datalist[[3]]
                raw_merge       <- raw_datalist[[4]]
                if (stage_get_data == FALSE) {
                  input_file <- raw_check$file_path
                }
                remotefile_check_flag <- 4
                if (raw_merge == TRUE) {
                  existing_raw_file_path <- raw_check$file_path
                }
                if (pro_merge == TRUE && stage_get_data == FALSE) {
                  remotefile_check_flag <- 5
                  write_pro_start        <- raw_check$start_date
                  write_pro_end          <- raw_check$end_date
                  existing_pro_file_path <- NULL
                  pro_merge              <- FALSE
                }
              } else{
                # this case happens when the processed file has to be extended but the raw file used to create the existing processed file has been deleted
                remotefile_check_flag <- 6
                write_raw_start        <- req_start
                write_raw_end          <- req_end
                start                  <- req_start
                end                    <- req_end
                stage_get_data         <- TRUE
                existing_raw_file_path <- NULL
                write_pro_start        <- write_raw_start
                write_pro_end          <- write_raw_end
                pro_merge              <- "replace"
                existing_pro_file_path <- NULL
              }
            }
          } else{
            # requested file already exists
            pro_id <- pro_check$id
            pro_path <- pro_check$file_path
            if (nrow(raw_check <-
                     PEcAn.DB::db.query(
                       sprintf(
                         "SELECT inputs.id, inputs.site_id, inputs.name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.name=dbfiles.file_name AND inputs.name LIKE '%s%%';",
                         raw_file_name
                       ),
                       dbcon
                     )) == 1) {
              raw_id <- raw_check$id
              raw_path <- raw_check$file_path
            }
          }
        }
        else if (nrow(raw_check <-
                      PEcAn.DB::db.query(
                        sprintf(
                          "SELECT inputs.id, inputs.site_id, inputs.name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.name=dbfiles.file_name AND inputs.name LIKE '%s%%';",
                          raw_file_name
                        ),
                        dbcon
                      )) == 1) {
          # if the processed file does not exist in the DB check if the raw file required for creating it is present
          PEcAn.logger::logger.info("Requested processed file does not exist in the DB, checking if the raw file does")
          datalist <-
            set_stage(raw_check, req_start, req_end, stage_get_data)
          start           <- as.character(datalist[[1]])
          end             <- as.character(datalist[[2]])
          write_raw_start <- datalist[[5]]
          write_raw_end   <- datalist[[6]]
          write_pro_start <- req_start
          write_pro_end   <- req_end
          stage_get_data  <- datalist[[3]]
          if (stage_get_data == FALSE) {
            input_file      <- raw_check$file_path
            write_pro_start <- raw_check$start_date
            write_pro_end   <- raw_check$end_date
            remotefile_check_flag <- 2
          }
          raw_merge <- datalist[[4]]
          stage_process_data <- TRUE
          pro_merge <- FALSE
          if (raw_merge == TRUE || raw_merge == "replace") {
            existing_raw_file_path = raw_check$file_path
            remotefile_check_flag <- 3
          } else{
            existing_raw_file_path = NULL
          }
        } else{
          # if no processed or raw file of requested type exists
          start                  <- req_start
          end                    <- req_end
          write_raw_start        <- req_start
          write_raw_end          <- req_end
          write_pro_start        <- req_start
          write_pro_end          <- req_end
          stage_get_data         <- TRUE
          raw_merge              <- FALSE
          existing_raw_file_path <- NULL
          stage_process_data     <- TRUE
          pro_merge              <- FALSE
          existing_pro_file_path <- NULL
          remotefile_check_flag  <- 1
        }
      } else if (nrow(raw_check <-
                      PEcAn.DB::db.query(
                        sprintf(
                          "SELECT inputs.id, inputs.site_id, inputs.name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.name=dbfiles.file_name AND inputs.name LIKE '%s%%';",
                          raw_file_name
                        ),
                        dbcon
                      )) == 1) {
        # if only raw data is requested
        datalist <-
          set_stage(raw_check, req_start, req_end, stage_get_data)
        start              <- as.character(datalist[[1]])
        end                <- as.character(datalist[[2]])
        stage_get_data     <- datalist[[3]]
        raw_merge          <- datalist[[4]]
        write_raw_start    <- datalist[[5]]
        write_raw_end      <- datalist[[6]]
        stage_process_data <- FALSE
        if (as.character(write_raw_start) == "dont write" &&
            as.character(write_raw_end) == "dont write") {
          raw_id   <- raw_check$id
          raw_path <- raw_check$file_path
        }
        if (raw_merge == TRUE) {
          existing_raw_file_path <- raw_check$file_path
          remotefile_check_flag <- 2
        } else{
          existing_raw_file_path <- NULL
        }
        existing_pro_file_path <- NULL
      } else{
        # no data of requested type exists
        PEcAn.logger::logger.info("Requested data does not exist in the DB, retrieving for the first time")
        remotefile_check_flag  <- 1
        start <- req_start
        end   <- req_end
        if (!is.null(out_get_data)) {
          stage_get_data         <- TRUE
          write_raw_start        <- req_start
          write_raw_end          <- req_end
          raw_merge              <- FALSE
          existing_raw_file_path <- NULL
        }
        if (!is.null(out_process_data)) {
          stage_process_data     <- TRUE
          write_pro_start        <- req_start
          write_pro_end          <- req_end
          pro_merge              <- FALSE
          process_file_name      <- NULL
          existing_pro_file_path <- NULL
          remotefile_check_flag  <- 1
        }
      }
      
    } else{
      # db is completely empty for the given siteid
      PEcAn.logger::logger.info("DB is completely empty for this site")
      remotefile_check_flag  <- 1
      start                  <- req_start
      end                    <- req_end
      stage_get_data         <- TRUE
      write_raw_start        <- req_start
      write_raw_end          <- req_end
      raw_merge              <- FALSE
      existing_raw_file_path <- NULL
      if (!is.null(out_process_data)) {
        stage_process_data     <- TRUE
        write_pro_start        <- req_start
        write_pro_end          <- req_end
        pro_merge              <- FALSE
        existing_pro_file_path <- NULL
      }
    }
    
    return(
            list(
        remotefile_check_flag  = remotefile_check_flag,
        start                  = start,
        end                    = end,
        stage_get_data         = stage_get_data,
        write_raw_start        = write_raw_start,
        write_raw_end          = write_raw_end,
        raw_merge              = raw_merge,
        existing_raw_file_path = existing_raw_file_path,
        stage_process_data     = stage_process_data,
        write_pro_start        = write_pro_start,
        write_pro_end          = write_pro_end,
        pro_merge              = pro_merge,
        input_file             = input_file,
        existing_pro_file_path = existing_pro_file_path,
        raw_check              = raw_check,
        pro_check              = pro_check
      )
      )
    
    
  }





##' Insert the output data returned from rp_control into BETYdb
##'
##' @name remotedata_db_insert
##' @param output output list from rp_control
##' @param remotefile_check_flag remotefile_check_flag
##' @param siteid siteid 
##' @param out_get_data out_get_data
##' @param out_process_data out_process_data
##' @param write_raw_start write_raw_start, start date of the raw file
##' @param write_raw_end write_raw_end, end date of the raw file
##' @param write_pro_start write_pro_start 
##' @param write_pro_end write_pro_end
##' @param raw_check raw_check
##' @param pro_check pro_check
##' @param raw_mimetype raw_mimetype
##' @param raw_formatname raw_formatname
##' @param pro_mimetype pro_mimetype
##' @param pro_formatname pro_formatname
##' @param dbcon BETYdb con
##'
##' @return list containing raw_id, raw_path, pro_id, pro_path
##' @author Ayush Prasad
##' @examples
##' \dontrun{
##' db_out <- remotedata_db_insert(
##'   output,
##'   remotefile_check_flag,
##'   siteid,
##'   out_get_data,
##'   out_process_data,
##'   write_raw_start,
##'   write_raw_end,
##'   write_pro_start,
##'   write_pro_end,
##'   raw_check,
##'   pro_check
##'   raw_mimetype,
##'   raw_formatname,
##'   pro_mimetype,
##'   pro_formatname,
##'   dbcon)
##' }
remotedata_db_insert <-
  function(output,
           remotefile_check_flag,
           siteid,
           out_get_data,
           out_process_data,
           write_raw_start,
           write_raw_end,
           write_pro_start,
           write_pro_end,
           raw_check,
           pro_check,
           raw_mimetype,
           raw_formatname,
           pro_mimetype,
           pro_formatname,
           dbcon) {
    
    # The value of remotefile_check_flag denotes the following cases:
    
    # When processed file is requested,
    # 1 - There are no existing raw and processed files of the requested type in the DB
    # 2 - Requested processed file does not exist, the raw file used to create is it present and matches with the requested daterange
    # 3 - Requested processed file does not exist, raw file used to create it is present but has to be updated to match with the requested daterange
    # 4 - Both processed and raw file of the requested type exists, but they have to be updated to match with the requested daterange
    # 5 - Raw file required for creating the processed file exists with the required daterange and the processed file needs to be updated. Here the new processed file will now contain data for the entire daterange of the existing raw file
    # 6 - There is a existing processed file of the requested type but the raw file used to create it has been deleted. Here, the raw file will be created again and the processed file will be replaced entirely with the one created from new raw file
    
    # When raw file is requested,
    # 1 - There is no existing raw the requested type in the DB
    # 2 - existing raw file will be updated
    
    pro_id <- NULL
    pro_path <- NULL
    
    if (!is.null(out_process_data)) {
      # if the requested processed file already exists within the required timeline dont insert or update the DB
      if (as.character(write_pro_start) == "dont write" &&
          as.character(write_pro_end) == "dont write") {
        PEcAn.logger::logger.info("Requested processed file already exists")
        pro_id   <- pro_check$id
        pro_path <- pro_check$file_path
        raw_id   <- raw_check$id
        raw_path <- raw_check$file_path
      } else{
        if (remotefile_check_flag == 1) {
          # no processed and rawfile are present
          PEcAn.logger::logger.info("Inserting raw and processed files for the first time")
          # insert processed data
          pro_ins <-
            PEcAn.DB::dbfile.input.insert(
              in.path    = output$process_data_path,
              in.prefix  = output$process_data_name,
              siteid     = siteid,
              startdate  = write_pro_start,
              enddate    = write_pro_end,
              mimetype   = pro_mimetype,
              formatname = pro_formatname,
              con        = dbcon
            )
          # insert raw file
          raw_ins <-
            PEcAn.DB::dbfile.input.insert(
              in.path    = output$raw_data_path,
              in.prefix  = output$raw_data_name,
              siteid     = siteid,
              startdate  = write_raw_start,
              enddate    = write_raw_end,
              mimetype   = raw_mimetype,
              formatname = raw_formatname,
              con        = dbcon
            )
          pro_id   <- pro_ins$input.id
          raw_id   <- raw_ins$input.id
          pro_path <- output$process_data_path
          raw_path <- output$raw_data_path
        } else if (remotefile_check_flag == 2) {
          # requested processed file does not exist but the raw file used to create it exists within the required timeline
          PEcAn.logger::logger.info("Inserting processed file for the first time")
          pro_ins <-
            PEcAn.DB::dbfile.input.insert(
              in.path    = output$process_data_path,
              in.prefix  = output$process_data_name,
              siteid     = siteid,
              startdate  = write_pro_start,
              enddate    = write_pro_end,
              mimetype   = pro_mimetype,
              formatname = pro_formatname,
              con        = dbcon
            )
          raw_id   <- raw_check$id
          raw_path <- raw_check$file_path
          pro_id   <- pro_ins$input.id
          pro_path <- output$process_data_path
        } else if (remotefile_check_flag == 3) {
          # requested processed file does not exist, raw file used to create it is present but has to be updated to match with the requested dates
          pro_ins <- PEcAn.DB::dbfile.input.insert(
            in.path    = output$process_data_path,
            in.prefix  = output$process_data_name,
            siteid     = siteid,
            startdate  = write_pro_start,
            enddate    = write_pro_end,
            mimetype   = pro_mimetype,
            formatname = pro_formatname,
            con        = dbcon
          )
          raw_id <- raw_check$id
          PEcAn.DB::db.query(
            sprintf(
              "UPDATE inputs SET start_date='%s', end_date='%s', name='%s' WHERE id=%f;",
              write_raw_start,
              write_raw_end,
              output$raw_data_name,
              raw_id
            ),
            dbcon
          )
          PEcAn.DB::db.query(
            sprintf(
              "UPDATE dbfiles SET file_path='%s', file_name='%s' WHERE container_id=%f;",
              output$raw_data_path,
              output$raw_data_name,
              raw_id
            ),
            dbcon
          )
          pro_id   <- pro_ins$input.id
          pro_path <- output$process_data_path
        } else if (remotefile_check_flag == 4) {
          # requested processed and raw files are present and have to be updated
          pro_id   <- pro_check$id
          raw_id   <- raw_check$id
          raw_path <- output$raw_data_path
          pro_path <- output$process_data_path
          PEcAn.logger::logger.info("Updating processed and raw files")
          PEcAn.DB::db.query(
            sprintf(
              "UPDATE inputs SET start_date='%s', end_date='%s', name='%s' WHERE id=%f;",
              write_pro_start,
              write_pro_end,
              output$process_data_name,
              pro_id
            ),
            dbcon
          )
          PEcAn.DB::db.query(
            sprintf(
              "UPDATE dbfiles SET file_path='%s', file_name='%s' WHERE container_id=%f;",
              output$process_data_path,
              output$process_data_name,
              pro_id
            ),
            dbcon
          )
          PEcAn.DB::db.query(
            sprintf(
              "UPDATE inputs SET start_date='%s', end_date='%s', name='%s' WHERE id=%f",
              write_raw_start,
              write_raw_end,
              output$raw_data_name,
              raw_id
            ),
            dbcon
          )
          PEcAn.DB::db.query(
            sprintf(
              "UPDATE dbfiles SET file_path='%s', file_name='%s' WHERE container_id=%f;",
              output$raw_data_path,
              output$raw_data_name,
              raw_id
            ),
            dbcon
          )
        } else if (remotefile_check_flag == 5) {
          # raw file required for creating the processed file exists and the processed file needs to be updated
          pro_id   <- pro_check$id
          pro_path <- output$process_data_path
          raw_id   <- raw_check$id
          raw_path <- raw_check$file_path
          PEcAn.logger::logger.info("Updating the existing processed file")
          PEcAn.DB::db.query(
            sprintf(
              "UPDATE inputs SET start_date='%s', end_date='%s', name='%s' WHERE id=%f;",
              write_pro_start,
              write_pro_end,
              output$process_data_name,
              pro_id
            ),
            dbcon
          )
          PEcAn.DB::db.query(
            sprintf(
              "UPDATE dbfiles SET file_path='%s', file_name='%s' WHERE container_id=%f;",
              output$process_data_path,
              output$process_data_name,
              pro_id
            ),
            dbcon
          )
        } else if (remotefile_check_flag == 6) {
          # there is some existing processed file but the raw file used to create it is now deleted, replace the processed file entirely with the one created from new raw file
          pro_id   <- pro_check$id
          pro_path <- output$process_data_path
          raw_path <- output$raw_data_path
          PEcAn.logger::logger.info("Replacing the existing processed file and creating a new raw file")
          PEcAn.DB::db.query(
            sprintf(
              "UPDATE inputs SET start_date='%s', end_date='%s', name='%s' WHERE id=%f;",
              write_pro_start,
              write_pro_end,
              output$process_data_name,
              pro_id
            ),
            dbcon
          )
          PEcAn.DB::db.query(
            sprintf(
              "UPDATE dbfiles SET file_path='%s', file_name='%s' WHERE container_id=%f;",
              output$process_data_path,
              output$process_data_name,
              pro_id
            ),
            dbcon
          )
          raw_ins <-
            PEcAn.DB::dbfile.input.insert(
              in.path    = output$raw_data_path,
              in.prefix  = output$raw_data_name,
              siteid     = siteid,
              startdate  = write_raw_start,
              enddate    = write_raw_end,
              mimetype   = raw_mimetype,
              formatname = raw_formatname,
              con        = dbcon
            )
          raw_id <- raw_ins$input.id
        }
      }
    }
    else{
      # if the requested raw file already exists within the required timeline dont insert or update the DB
      if (as.character(write_raw_start) == "dont write" &&
          as.character(write_raw_end) == "dont write") {
        PEcAn.logger::logger.info("Requested raw file already exists")
        raw_id   <- raw_check$id
        raw_path <- raw_check$file_path
      } else{
        if (remotefile_check_flag == 1) {
          PEcAn.logger::logger.info(("Inserting raw file for the first time"))
          raw_ins <-
            PEcAn.DB::dbfile.input.insert(
              in.path    = output$raw_data_path,
              in.prefix  = output$raw_data_name,
              siteid     = siteid,
              startdate  = write_raw_start,
              enddate    = write_raw_end,
              mimetype   = raw_mimetype,
              formatname = raw_formatname,
              con        = dbcon
            )
          raw_id   <- raw_ins$input.id
          raw_path <- output$raw_data_path
        } else if (remotefile_check_flag == 2) {
          PEcAn.logger::logger.info("Updating raw file")
          raw_id   <- raw_check$id
          raw_path <- output$raw_data_path
          PEcAn.DB::db.query(
            sprintf(
              "UPDATE inputs SET start_date='%s', end_date='%s', name='%s' WHERE id=%f;",
              write_raw_start,
              write_raw_end,
              output$raw_data_name,
              raw_id
            ),
            dbcon
          )
          PEcAn.DB::db.query(
            sprintf(
              "UPDATE dbfiles SET file_path='%s', file_name='%s' WHERE container_id=%f;",
              output$raw_data_path,
              output$raw_data_name,
              raw_id
            ),
            dbcon
          )
        }
      }
    }
    
    return(list(raw_id, raw_path, pro_id, pro_path))
  }
