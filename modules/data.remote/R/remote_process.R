##' call rp_control (from RpTools Python package) and store the output in BETY
##'
##' @name remote_process
##' @title remote_process
##' @export
##'
##' @param settings PEcAn settings list containing remotedata tags: source, collection, scale, projection, qc, algorithm, credfile, out_get_data, out_process_data, overwrite
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
  outdir           <- settings$database$dbfiles
  lat              <- as.numeric(settings$run$site$lat)
  lon              <- as.numeric(settings$run$site$lon)
  start            <- as.character(as.Date(settings$run$start.date))
  end              <- as.character(as.Date(settings$run$end.date))
  source           <- settings$remotedata$source
  collection       <- settings$remotedata$collection
  reg_info         <- read_remote_registry(source, collection)
  collection       <- reg_info$pecan_name
  raw_mimetype     <- reg_info$raw_mimetype
  raw_formatname   <- reg_info$raw_formatname
  pro_mimetype     <- reg_info$pro_mimetype
  pro_formatname   <- reg_info$pro_formatname 
  

  if (!is.null(reg_info$scale)) {
    if(!is.null(settings$remotedata$scale)){
      scale <- as.double(settings$remotedata$scale)
      scale <- format(scale, nsmall = 1)
    }else{
      scale <- as.double(reg_info$scale)
      scale <- format(scale, nsmall = 1)
      PEcAn.logger::logger.warn(paste0("scale not provided, using default scale ", scale))
    }
  }else{
    scale <- NULL
  }
  
  if (!is.null(reg_info$qc)) {
    if(!is.null(settings$remotedata$qc)){
      qc <- as.double(settings$remotedata$qc)
      qc <- format(qc, nsmall = 1)
    }else{
      qc <- as.double(reg_info$qc)
      qc <- format(qc, nsmall = 1)
      PEcAn.logger::logger.warn(paste0("qc not provided, using default qc ", qc))
    }
  }else{
    qc <- NULL
  }

  if (!is.null(reg_info$projection)) {
    if(!is.null(settings$remotedata$projection)){
      projection <- settings$remotedata$projection
    }else{
      projection <- reg_info$projection
      PEcAn.logger::logger.warn(paste0("projection not provided, using default projection ", projection))
    }
  }else{
    projection <- NULL
  }
  
  algorithm        <- settings$remotedata$algorithm
  credfile         <- settings$remotedata$credfile
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
  these_sources <- gsub("^.+?\\.(.+?)\\..*$", "\\1", list.files(system.file("registration", package = "PEcAn.data.remote")))
  PEcAn.logger::severeifnot(paste0("Source should be one of ", paste(these_sources, collapse = ' ')), toupper(source) %in% these_sources)
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
  
  # extract the AOI of the site from BETYdb
  coords <-
    unlist(PEcAn.DB::db.query(
      sprintf("select ST_AsGeoJSON(geometry) from sites where id=%f", siteid),
      con = dbcon
    ), use.names = FALSE)
  
  if(!(tolower(gsub(".*type(.+),coordinates.*", "\\1",  gsub("[^=A-Za-z,0-9{} ]+","",coords))) %in% reg_info$coordtype)){
    PEcAn.logger::logger.severe(paste0("Coordinate type of the site is not supported by the requested source, please make sure that your site type is ", reg_info$coordtype))
  }
  
  # construct raw file name
  remotedata_file_names <- construct_remotedata_filename(source, collection, siteid_short, scale, projection, qc, algorithm, out_process_data)
  
  raw_file_name <- remotedata_file_names$raw_file_name
  
  pro_file_name <- remotedata_file_names$pro_file_name

  
  # check if any data is already present in the inputs table
  dbstatus <-
    remotedata_db_check(
      raw_file_name     = raw_file_name,
      pro_file_name     = pro_file_name,
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
  

  if(stage_get_data == FALSE && stage_process_data == FALSE){
    # requested data already exists, no need to call rp_control
    settings$remotedata$raw_id   <- raw_check$id
    settings$remotedata$raw_path <- raw_check$file_path
    settings$remotedata$pro_id   <- pro_check$id
    settings$remotedata$pro_path <- pro_check$file_path
    return(settings)
  }

  
  # construct outdir path
  outdir <-
    file.path(outdir, paste(toupper(source), "site", siteid_short, sep = "_"))

  
  fcn.args <- list()
  fcn.args$coords                 <- coords
  fcn.args$outdir                 <- outdir
  fcn.args$lat                    <- lat
  fcn.args$lon                    <- lon
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
  fcn.args$raw_file_name          <- raw_file_name
  fcn.args$pro_file_name          <- pro_file_name
  

  
  
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
    settings$remotedata$raw_id   <- db_out$raw_id
    settings$remotedata$raw_path <- db_out$raw_path
  }
  if (!is.null(out_process_data)) {
    settings$remotedata$pro_id   <- db_out$pro_id
    settings$remotedata$pro_path <- db_out$pro_path
  }
  
  return (settings)
}



##' construct remotedata module file names
##'
##' @name construct_remotedata_filename
##' @title construct_remotedata_filename
##' @param source source
##' @param collection collection or product requested from the source
##' @param siteid shortform of siteid
##' @param scale scale, NULL by default
##' @param projection projection, NULL by default
##' @param qc qc_parameter, NULL by default
##' @param algorithm algorithm name to process data, NULL by default
##' @param out_process_data variable name requested for the processed file, NULL by default
##' @return remotedata_file_names
##' @examples
##' \dontrun{
##' remotedata_file_names <- construct_remotedata_filename(
##'   source="gee",
##'   collection="s2",
##'   siteid="0-721",
##'   scale=10.0
##'   projection=NULL
##'   qc=1.0,
##'   algorithm="snap",
##'   out_process_data="lai")
##' }
##' @author Ayush Prasad
construct_remotedata_filename <-
  function(source,
           collection,
           siteid,
           scale = NULL,
           projection = NULL,
           qc = NULL,
           algorithm = NULL,
           out_process_data = NULL) {
    # skip if a parameter is not applicable and is NULL
    if (is.null(scale)) {
      scale_str <- "_"
    } else{
      scale_str <- paste0("_", format(scale, nsmall = 1), "_")
    }
    if (is.null(projection)) {
      prj_str <- ""
    }else{
      prj_str <- paste0(projection, "_")
    }
    if (is.null(qc)) {
      qc_str <- ""
    } else{
      qc_str <- paste0(format(qc, nsmall = 1), "_")
    }
    
    raw_file_name <- paste0(toupper(source), "_", collection, scale_str, prj_str, qc_str, "site_", siteid)
        if(!is.null(out_process_data)){
      alg_str <- paste0(algorithm, "_")
      var_str <- paste0(out_process_data, "_")
      pro_file_name <- paste0(toupper(source), "_", collection, scale_str, prj_str, qc_str, alg_str, var_str, "site_", siteid)
    }else{
      pro_file_name <- NULL
    }
    
    remotedata_file_names <- list(raw_file_name = raw_file_name,
                                  pro_file_name = pro_file_name)
    
    return(remotedata_file_names)
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
  return (list(req_start = req_start, req_end = req_end, stage = stage, merge = merge, write_start = write_start, write_end = write_end))
  
}




##' read remote module registration files
##'
##' @name read_remote_registry
##' @title read_remote_registry
##' @importFrom purrr %>%
##' @param source remote source, e.g gee or appeears
##' @param collection collection or product name
##' @return list containing original_name, pecan_name, scale, qc, projection raw_mimetype, raw_formatname pro_mimetype, pro_formatname, coordtype
##' @examples
##' \dontrun{
##'  read_remote_registry(
##'   "gee",
##'   "COPERNICUS/S2_SR")
##' }
##' @author Istem Fer
read_remote_registry <- function(source, collection){
  
  # get registration file
  register.xml <- system.file(paste0("registration/register.", toupper(source), ".xml"), package = "PEcAn.data.remote")
  
  tryCatch(expr = {
    register <- XML::xmlToList(XML::xmlParse(register.xml))
    }, 
    error = function(e){
      PEcAn.logger::logger.severe("Requested source is not available")
    } 
  )
  . <- NULL
  
  if(!(purrr::is_empty(register %>% purrr::keep(names(.) == "collection")))){
    # this is a type of source that requires different setup for its collections, e.g. GEE
    # then read collection specific information
    register <- register[[which(register %>% purrr::map_chr("original_name") == collection)]]
  }
  
  reg_list <- list()
  reg_list$original_name  <- ifelse(is.null(register$original_name), collection, register$original_name)
  reg_list$pecan_name     <- ifelse(is.null(register$pecan_name), collection, register$pecan_name)
  reg_list$scale          <- register$scale
  reg_list$qc             <- register$qc
  reg_list$projection     <- register$projection
  reg_list$raw_mimetype   <- register$raw_format$mimetype
  reg_list$raw_formatname <- register$raw_format$name
  reg_list$pro_mimetype   <- register$pro_format$mimetype
  reg_list$pro_formatname <- register$pro_format$name
  reg_list$coordtype      <- unlist(register$coord)
  
  return(reg_list)
}





##' check the status of the requested data in the DB
##'
##' @name  remotedata_db_check
##' @title remotedata_db_check
##' @param raw_file_name raw_file_name
##' @param pro_file_name pro_file_name
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
##'   pro_file_name,
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
           pro_file_name,
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
    stage_get_data         <- FALSE
    stage_process_data     <- FALSE
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
          if (nrow(pro_check <-
                   PEcAn.DB::db.query(
                     sprintf(
                       "SELECT inputs.id, inputs.site_id, dbfiles.file_name as name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.id=dbfiles.container_id AND dbfiles.file_name LIKE '%s%%';",
                       pro_file_name
                     ),
                     dbcon
                   )) == 1) {
            if (nrow(raw_check <-
                     PEcAn.DB::db.query(
                       sprintf(
                         "SELECT inputs.id, inputs.site_id, dbfiles.file_name as name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.id=dbfiles.container_id	 AND dbfiles.file_name LIKE '%s%%';",
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
          pro_merge          <- "replace"
          write_pro_start    <- start
          write_pro_end      <- end
        } else if (!is.null(out_get_data)) {
          if (nrow(raw_check <-
                   PEcAn.DB::db.query(
                     sprintf(
                       "SELECT inputs.id, inputs.site_id, dbfiles.file_name as name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.id=dbfiles.container_id AND dbfiles.file_name LIKE '%s%%';",
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

        # check if processed file exists
        if (nrow(pro_check <-
                 PEcAn.DB::db.query(
                   sprintf(
                     "SELECT inputs.id, inputs.site_id, dbfiles.file_name as name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.id=dbfiles.container_id AND dbfiles.file_name LIKE '%s%%';",
                     pro_file_name
                   ),
                   dbcon
                 )) == 1) {
          datalist <-
            set_stage(pro_check, req_start, req_end, stage_process_data)
          pro_start       <- as.character(datalist$req_start)
          pro_end         <- as.character(datalist$req_end)
          write_pro_start <- datalist$write_start
          write_pro_end   <- datalist$write_end
          if (pro_start != "dont write" || pro_end != "dont write") {
            stage_process_data <- datalist$stage
            pro_merge <- datalist$merge
            if (pro_merge == TRUE) {
              existing_pro_file_path <- file.path(pro_check$file_path, pro_check$name)
            }
            if (stage_process_data == TRUE) {
              # check about the status of raw file
              raw_check <-
                PEcAn.DB::db.query(
                  sprintf(
                    "SELECT inputs.id, inputs.site_id, dbfiles.file_name as name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.id=dbfiles.container_id AND dbfiles.file_name LIKE '%s%%';",
                    raw_file_name
                  ),
                  dbcon
                )
              if (!is.null(raw_check$start_date) &&
                  !is.null(raw_check$end_date)) {
                raw_datalist <-
                  set_stage(raw_check, pro_start, pro_end, stage_get_data)
                start           <- as.character(raw_datalist$req_start)
                end             <- as.character(raw_datalist$req_end)
                write_raw_start <- raw_datalist$write_start
                write_raw_end   <- raw_datalist$write_end
                stage_get_data  <- raw_datalist$stage
                raw_merge       <- raw_datalist$merge
                if (stage_get_data == FALSE) {
                  input_file <- raw_check$file_path
                }
                remotefile_check_flag <- 4
                if (raw_merge == TRUE) {
                  existing_raw_file_path <- file.path(raw_check$file_path, raw_check$name)
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
                         "SELECT inputs.id, inputs.site_id, dbfiles.file_name as name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.id=dbfiles.container_id AND dbfiles.file_name LIKE '%s%%';",
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
                          "SELECT inputs.id, inputs.site_id, dbfiles.file_name as name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.id=dbfiles.container_id AND dbfiles.file_name LIKE '%s%%';",
                          raw_file_name
                        ),
                        dbcon
                      )) == 1) {
          # if the processed file does not exist in the DB check if the raw file required for creating it is present
          PEcAn.logger::logger.info("Requested processed file does not exist in the DB, checking if the raw file does")
          datalist <-
            set_stage(raw_check, req_start, req_end, stage_get_data)
          start           <- as.character(datalist$req_start)
          end             <- as.character(datalist$req_end)
          write_raw_start <- datalist$write_start
          write_raw_end   <- datalist$write_end
          write_pro_start <- req_start
          write_pro_end   <- req_end
          stage_get_data  <- datalist$stage
          if (stage_get_data == FALSE) {
            input_file      <- raw_check$file_path
            write_pro_start <- raw_check$start_date
            write_pro_end   <- raw_check$end_date
            remotefile_check_flag <- 2
          }
          raw_merge <- datalist$merge
          stage_process_data <- TRUE
          pro_merge <- FALSE
          if (raw_merge == TRUE || raw_merge == "replace") {
            existing_raw_file_path = file.path(raw_check$file_path, raw_check$name)
            remotefile_check_flag <- 3
          } else{
            existing_raw_file_path <- NULL
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
                          "SELECT inputs.id, inputs.site_id, dbfiles.file_name as name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.id=dbfiles.container_id AND dbfiles.file_name LIKE '%s%%';",
                          raw_file_name
                        ),
                        dbcon
                      )) == 1) {
        # if only raw data is requested
        datalist <-
          set_stage(raw_check, req_start, req_end, stage_get_data)
        start              <- as.character(datalist$req_start)
        end                <- as.character(datalist$req_end)
        stage_get_data     <- datalist$stage
        raw_merge          <- datalist$merge
        write_raw_start    <- datalist$write_start
        write_raw_end      <- datalist$write_end
        stage_process_data <- FALSE
        if (as.character(write_raw_start) == "dont write" &&
            as.character(write_raw_end) == "dont write") {
          raw_id   <- raw_check$id
          raw_path <- raw_check$file_path
        }
        if (raw_merge == TRUE) {
          existing_raw_file_path <- file.path(raw_check$file_path, raw_check$name)
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
##' @param raw_check  id, site_id, name, start_date, end_date, of the existing raw file from inputs table and file_path from dbfiles tables
##' @param pro_check pro_check  id, site_id, name, start_date, end_date, of the existing processed file from inputs table and file_path from dbfiles tables
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
              in.path    = dirname(output$process_data_path),
              in.prefix  = basename(output$process_data_path),
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
              in.path    = dirname(output$raw_data_path),
              in.prefix  = basename(output$raw_data_path),
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
              in.path    = dirname(output$process_data_path),
              in.prefix  = basename(output$process_data_path),
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
            in.path    = dirname(output$process_data_path),
            in.prefix  = basename(output$process_data_path),
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
              basename(dirname(output$raw_data_path)),
              raw_id
            ),
            dbcon
          )
          PEcAn.DB::db.query(
            sprintf(
              "UPDATE dbfiles SET file_path='%s', file_name='%s' WHERE container_id=%f;",
              dirname(output$raw_data_path),
              basename(output$raw_data_path),
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
              basename(dirname(output$process_data_path)),
              pro_id
            ),
            dbcon
          )
          PEcAn.DB::db.query(
            sprintf(
              "UPDATE dbfiles SET file_path='%s', file_name='%s' WHERE container_id=%f;",
              dirname(output$process_data_path),
              basename(output$process_data_path),
              pro_id
            ),
            dbcon
          )
          PEcAn.DB::db.query(
            sprintf(
              "UPDATE inputs SET start_date='%s', end_date='%s', name='%s' WHERE id=%f",
              write_raw_start,
              write_raw_end,
              basename(dirname(output$raw_data_path)),
              raw_id
            ),
            dbcon
          )
          PEcAn.DB::db.query(
            sprintf(
              "UPDATE dbfiles SET file_path='%s', file_name='%s' WHERE container_id=%f;",
              dirname(output$raw_data_path),
              basename(output$raw_data_path),
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
              basename(dirname(output$process_data_path)),
              pro_id
            ),
            dbcon
          )
          PEcAn.DB::db.query(
            sprintf(
              "UPDATE dbfiles SET file_path='%s', file_name='%s' WHERE container_id=%f;",
              dirname(output$process_data_path),
              basename(output$process_data_path),
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
              basename(dirname(output$process_data_path)),
              pro_id
            ),
            dbcon
          )
          PEcAn.DB::db.query(
            sprintf(
              "UPDATE dbfiles SET file_path='%s', file_name='%s' WHERE container_id=%f;",
              dirname(output$process_data_path),
              basename(output$process_data_path),
              pro_id
            ),
            dbcon
          )
          raw_ins <-
            PEcAn.DB::dbfile.input.insert(
              in.path    = dirname(output$raw_data_path),
              in.prefix  = basename(output$raw_data_path),
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
              in.path    = dirname(output$raw_data_path),
              in.prefix  = basename(output$raw_data_path),
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
              basename(dirname(output$raw_data_path)),
              raw_id
            ),
            dbcon
          )
          PEcAn.DB::db.query(
            sprintf(
              "UPDATE dbfiles SET file_path='%s', file_name='%s' WHERE container_id=%f;",
              dirname(output$raw_data_path),
              basename(dirname(output$raw_data_path)),
              raw_id
            ),
            dbcon
          )
        }
      }
    }
    
    return(list(raw_id = raw_id, raw_path = raw_path, pro_id = pro_id, pro_path = pro_path))
  }