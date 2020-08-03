##' call the Python - remote_process from PEcAn and store the output in BETY
##' 
##' @name call_remote_process
##' @title call_remote_process
##' @export
##' @param settings PEcAn settings list containing remotedata tags: siteid, sitename, raw_mimetype, raw_formatname, geofile, outdir, start, end, source, collection, scale, projection, qc, algorithm, credfile, pro_mimetype, pro_formatname, out_get_data, out_process_data
##' @examples
##' \dontrun{
##' call_remote_process(settings)
##' }
##' @author Ayush Prasad
##'

call_remote_process <- function(settings){
  
  # information about the date variables used in call_remote_process -
  # req_start, req_end : start, end dates requested by the user, the user does not be aware about the status of the requested file in the DB  
  # start, end : effective start, end dates created after checking the DB status. These dates are sent to remote_process for downloading and processing data
  # write_raw_start, write_raw_end : start, end dates which are used while inserting and updating the DB
  # the "pro" version of these variables have the same meaning and are used to refer to the processed file

  reticulate::import_from_path("remote_process", file.path("..", "inst"))
  reticulate::source_python(file.path("..", "inst", "remote_process.py"))
  
  input_file <- NULL
  stage_get_data <- NULL
  stage_process_data <- NULL
  raw_merge <- NULL
  pro_merge <- NULL
  existing_raw_file_path <- NULL
  existing_pro_file_path <- NULL
  
  # extract the variables from the settings list
  siteid <- settings$remotedata$siteid
  sitename <- settings$remotedata$sitename
  raw_mimetype <- settings$remotedata$raw_mimetype
  raw_formatname <- settings$remotedata$raw_formatname
  geofile <- settings$remotedata$geofile
  outdir <- settings$remotedata$outdir
  start <- settings$remotedata$start
  end <- settings$remotedata$end
  source <- settings$remotedata$source
  collection <- settings$remotedata$collection
  scale <- settings$remotedata$scale
  if(!is.null(scale)){
  scale <- as.double(settings$remotedata$scale)
  scale <- format(scale, nsmall = 1)
  }
  projection <- settings$remotedata$projection
  qc <- settings$remotedata$qc
  if(!is.null(qc)){
  qc <- as.double(settings$remotedata$qc)
  qc <- format(qc, nsmall = 1)
  }
  algorithm <- settings$remotedata$algorithm
  credfile <- settings$remotedata$credfile
  pro_mimetype <- settings$remotedata$pro_mimetype
  pro_formatname <- settings$remotedata$pro_formatname
  out_get_data <- settings$remotedata$out_get_data
  out_process_data <- settings$remotedata$out_process_data
  
  
  dbcon <- PEcAn.DB::db.open(settings$database$bety)
  flag <- 0

  # collection dataframe used to map Google Earth Engine collection names to their PEcAn specific names
  collection_lut <- data.frame(stringsAsFactors=FALSE,
                          original_name = c("LANDSAT/LC08/C01/T1_SR", "COPERNICUS/S2_SR", "NASA_USDA/HSL/SMAP_soil_moisture"),
                          pecan_code = c("l8", "s2", "smap")
  )
  getpecancode <- collection_lut$pecan_code
  names(getpecancode) <- collection_lut$original_name
  
  if(source == "gee"){
  collection = unname(getpecancode[collection])
  }

  req_start <- start
  req_end <- end
  
  raw_file_name = construct_raw_filename(sitename, source, collection, scale, projection, qc)
  
  # check if any data is already present in the inputs table
  existing_data <- PEcAn.DB::db.query(paste0("SELECT * FROM inputs WHERE site_id=", siteid), dbcon)
  if(nrow(existing_data) >= 1){
    
    # if processed data is requested, example lai
    if(!is.null(out_process_data)){
      
      # construct processed file name
      pro_file_name = paste0(sitename, "_", out_process_data, "_", algorithm)
      
      # check if processed file exists
      if(nrow(pro_check <- PEcAn.DB::db.query(sprintf("SELECT inputs.id, inputs.site_id, inputs.name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.name=dbfiles.file_name AND inputs.name LIKE '%s%%';", pro_file_name), dbcon)) == 1){
        datalist <- set_stage(pro_check, req_start, req_end, stage_process_data)
        pro_start <- as.character(datalist[[1]])
        pro_end <- as.character(datalist[[2]])
        write_pro_start <- datalist[[5]]
        write_pro_end <- datalist[[6]]
        if(pro_start != "dont write" || pro_end != "dont write"){
        stage_process_data <- datalist[[3]]
        pro_merge <- datalist[[4]]
        if(pro_merge == TRUE){
          existing_pro_file_path <- pro_check$file_path
        }
        if(stage_process_data == TRUE){
          # check about the status of raw file
          raw_check <- PEcAn.DB::db.query(sprintf("SELECT inputs.id, inputs.site_id, inputs.name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.name=dbfiles.file_name AND inputs.name LIKE '%s%%';", raw_file_name), dbcon)
          raw_datalist <- set_stage(raw_check, pro_start, pro_end, stage_get_data)
          start <- as.character(raw_datalist[[1]])
          end <- as.character(raw_datalist[[2]])
          write_raw_start <- raw_datalist[[5]]
          write_raw_end <- raw_datalist[[6]]
          stage_get_data <- raw_datalist[[3]]
          raw_merge <- raw_datalist[[4]]
          if(stage_get_data == FALSE){
            input_file <- raw_check$file_path
          }
          flag <- 4
          if(raw_merge == TRUE){
            existing_raw_file_path <- raw_check$file_path
          }
          if(pro_merge == TRUE && stage_get_data == FALSE){
            flag <- 5
            write_pro_start <- raw_check$start_date
            write_pro_end <- raw_check$end_date
            existing_pro_file_path <- NULL
            pro_merge <- FALSE
          }
        }
        }
      }
      else if(nrow(raw_check <- PEcAn.DB::db.query(sprintf("SELECT inputs.id, inputs.site_id, inputs.name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.name=dbfiles.file_name AND inputs.name LIKE '%s%%';", raw_file_name), dbcon)) ==1){
        # if the processed file does not exist in the DB check if the raw file required for creating it is present
        PEcAn.logger::logger.info("Requested processed file does not exist in the DB, checking if the raw file does")
        datalist <- set_stage(raw_check, req_start, req_end, stage_get_data)
        start <- as.character(datalist[[1]])
        end <- as.character(datalist[[2]])
        write_raw_start <- datalist[[5]]
        write_raw_end <- datalist[[6]]
        write_pro_start <- req_start
        write_pro_end <- req_end
        stage_get_data <- datalist[[3]]
        if(stage_get_data == FALSE){
          input_file <- raw_check$file_path
          write_pro_start <- raw_check$start_date
          write_pro_end <- raw_check$end_date
          flag <- 2
        }
        raw_merge <- datalist[[4]]
        stage_process_data <- TRUE
        pro_merge <- FALSE
        if(raw_merge == TRUE || raw_merge == "replace"){
          existing_raw_file_path = raw_check$file_path
          flag <- 3
        }else{
          existing_raw_file_path = NULL
        }
      }else{
        # if no processed or raw file of requested type exists
        start <- req_start
        end <- req_end
        write_raw_start <- req_start
        write_raw_end <- req_end
        write_pro_start <- req_start
        write_pro_end <- req_end
        stage_get_data <- TRUE
        raw_merge <- FALSE
        existing_raw_file_path = NULL
        stage_process_data <- TRUE
        pro_merge <- FALSE
        existing_pro_file_path = NULL
        flag <- 1
      } 
    }else if(nrow(raw_check <- PEcAn.DB::db.query(sprintf("SELECT inputs.id, inputs.site_id, inputs.name, inputs.start_date, inputs.end_date, dbfiles.file_path FROM inputs INNER JOIN dbfiles ON inputs.name=dbfiles.file_name AND inputs.name LIKE '%s%%';", raw_file_name), dbcon)) == 1){
      # if only raw data is requested
      datalist <- set_stage(raw_check, req_start, req_end, stage_get_data)
      start <- as.character(datalist[[1]])
      end <- as.character(datalist[[2]])
      stage_get_data <- datalist[[3]]
      raw_merge <- datalist[[4]]
      write_raw_start <- datalist[[5]]
      write_raw_end <- datalist[[6]]
      stage_process_data <- FALSE
      if(raw_merge == TRUE){
        existing_raw_file_path <- raw_check$file_path
      }else{
        existing_raw_file_path <- NULL 
      }
      existing_pro_file_path <- NULL
    }else{
      # no data of requested type exists
      PEcAn.logger::logger.info("nothing of requested type exists")
      flag <- 1
      start <- req_start
      end <- req_end
      if(!is.null(out_get_data)){
        stage_get_data <- TRUE
        write_raw_start <- req_start
        write_raw_end <- req_end
        raw_merge <- FALSE
        existing_raw_file_path = NULL
      }
      if(!is.null(out_process_data)){
        stage_process_data <- TRUE
        write_pro_start <- req_start
        write_pro_end <- req_end
        pro_merge <- FALSE
        process_file_name <- NULL
        existing_pro_file_path <- NULL
        flag <- 1
      }
    }
    
  }else{
    # db is completely empty for the given siteid
    PEcAn.logger::logger.info("DB is completely empt for this site")
    flag <- 1
    start <- req_start
    end <- req_end
    stage_get_data <- TRUE
    write_raw_start <- req_start
    write_raw_end <- req_end
    raw_merge <- FALSE
    existing_raw_file_path = NULL
    if(!is.null(out_process_data)){
      stage_process_data <- TRUE
      write_pro_start <- req_start
      write_pro_end <- req_end
      pro_merge <- FALSE
      existing_pro_file_path <- NULL
    }
  }
    

  # call remote_process  
  output = remote_process(geofile=geofile, outdir=outdir, start=start, end=end, source=source, collection=collection, scale=as.double(scale), projection=projection, qc=as.double(qc), algorithm=algorithm, input_file=input_file, credfile=credfile, out_get_data=out_get_data, out_process_data=out_process_data, stage_get_data=stage_get_data, stage_process_data=stage_process_data, raw_merge=raw_merge, pro_merge=pro_merge, existing_raw_file_path=existing_raw_file_path, existing_pro_file_path=existing_pro_file_path)
  
  
  # inserting data in the DB
  if(!is.null(out_process_data)){
    # if the requested processed file already exists within the required timeline dont insert or update the DB
    if(as.character(write_pro_start) == "dont write" && as.character(write_pro_end) == "dont write"){
      PEcAn.logger::logger.info("Requested processed file already exists")
    }else{
    if(flag == 1){
      # no processed and rawfile are present
      PEcAn.logger::logger.info("inserting rawfile and processed files for the first time")
      # insert processed data
      PEcAn.DB::dbfile.input.insert(in.path = output$process_data_path, in.prefix = output$process_data_name, siteid = siteid, startdate = write_pro_start, enddate = write_pro_end, mimetype = pro_mimetype, formatname = pro_formatname, con = dbcon)
      # insert raw file
      PEcAn.DB::dbfile.input.insert(in.path = output$raw_data_path, in.prefix = output$raw_data_name, siteid = siteid, startdate = write_raw_start, enddate = write_raw_end, mimetype = raw_mimetype, formatname = raw_formatname, con = dbcon)
    }else if(flag == 2){
      # requested processed file does not exist but the raw file used to create it exists within the required timeline
      PEcAn.logger::logger.info("inserting proc file for the first time")
      PEcAn.DB::dbfile.input.insert(in.path = output$process_data_path, in.prefix = output$process_data_name, siteid = siteid, startdate = write_pro_start, enddate = write_pro_end, mimetype = pro_mimetype, formatname = pro_formatname, con = dbcon)
    }else if(flag == 3){
      # requested processed file does not exist, raw file used to create it is present but has to be updated to match with the requested dates
      PEcAn.DB::dbfile.input.insert(in.path = output$process_data_path, in.prefix = output$process_data_name, siteid = siteid, startdate = write_pro_start, enddate = write_pro_end, mimetype = pro_mimetype, formatname = pro_formatname, con = dbcon)
      raw_id = raw_check$id
      PEcAn.DB::db.query(sprintf("UPDATE inputs SET start_date='%s', end_date='%s', name='%s' WHERE id=%f;", write_raw_start, write_raw_end, output$raw_data_name, raw_id), dbcon)
      PEcAn.DB::db.query(sprintf("UPDATE dbfiles SET file_path='%s', file_name='%s' WHERE container_id=%f;", output$raw_data_path, output$raw_data_name, raw_id), dbcon)      
    }else if(flag == 4){
      # requested processed and raw files are present and have to be updated
      pro_id = pro_check$id
      raw_id = raw_check$id
      PEcAn.logger::logger.info("updating processed and raw files")
      PEcAn.DB::db.query(sprintf("UPDATE inputs SET start_date='%s', end_date='%s', name='%s' WHERE id=%f;", write_pro_start, write_pro_end, output$process_data_name, pro_id), dbcon)
      PEcAn.DB::db.query(sprintf("UPDATE dbfiles SET file_path='%s', file_name='%s' WHERE container_id=%f;", output$process_data_path, output$process_data_name, pro_id), dbcon)
      PEcAn.DB::db.query(sprintf("UPDATE inputs SET start_date='%s', end_date='%s', name='%s' WHERE id=%f", write_raw_start, write_raw_end, output$raw_data_name, raw_id), dbcon)
      PEcAn.DB::db.query(sprintf("UPDATE dbfiles SET file_path='%s', file_name='%s' WHERE container_id=%f;", output$raw_data_path, output$raw_data_name, raw_id), dbcon)
    }else if(flag == 5){
      # raw file required for creating the processed file exists and the processed file needs to be updated
      pro_id = pro_check$id
      PEcAn.logger::logger.info("Updating the existing processed file")
      PEcAn.DB::db.query(sprintf("UPDATE inputs SET start_date='%s', end_date='%s', name='%s' WHERE id=%f;", write_pro_start, write_pro_end, output$process_data_name, pro_id), dbcon)
      PEcAn.DB::db.query(sprintf("UPDATE dbfiles SET file_path='%s', file_name='%s' WHERE container_id=%f;", output$process_data_path, output$process_data_name, pro_id), dbcon)
    }
    }
  }
  else{
    # if the requested raw file already exists within the required timeline dont insert or update the DB
    if(as.character(write_raw_start) == "dont write" && as.character(write_raw_end) == "dont write"){
      PEcAn.logger::logger.info("Requested raw file already exists")
    }else{
    if(flag == 1){
      PEcAn.logger::logger.info(("Inserting raw file for the first time"))
      PEcAn.DB::dbfile.input.insert(in.path = output$raw_data_path, in.prefix = output$raw_data_name, siteid = siteid, startdate = write_raw_start, enddate = write_raw_end, mimetype = raw_mimetype, formatname = raw_formatname, con = dbcon)
    }else{
      PEcAn.logger::logger.info("Updating raw file")
      raw_id = raw_check$id
      PEcAn.DB::db.query(sprintf("UPDATE inputs SET start_date='%s', end_date='%s', name='%s' WHERE id=%f;", write_raw_start, write_raw_end, output$raw_data_name, raw_id), dbcon)
      PEcAn.DB::db.query(sprintf("UPDATE dbfiles SET file_path='%s', file_name='%s' WHERE container_id=%f;", output$raw_data_path, output$raw_data_name, raw_id), dbcon)
    }
    }
  }
  
  PEcAn.DB::db.close(con=dbcon)
  
}



##' construct the raw file name  
##' 
##' @name construct_raw_filename
##' @title construct_raw_filename
##' @param sitename site name  
##' @param source source of the remote data
##' @param collection collection or product requested from the source
##' @param scale scale, NULL by default
##' @param projection projection, NULL by default
##' @param qc qc_parameter, NULL by default 
##' @return raw_file_name
##' @examples
##' \dontrun{
##' raw_file_name <- construct_raw_filename(
##'   sitename="Reykjavik",
##'   source="gee",
##'   collection="s2",
##'   scale=10.0
##'   projection=NULL
##'   qc=1.0)
##' }
##' @author Ayush Prasad
construct_raw_filename <- function(sitename, source, collection, scale=NULL, projection=NULL, qc=NULL){
  # use NA if a parameter is not applicable and is NULL
  if(is.null(scale)){
    scale <- "NA"
  }else{
    scale <- format(scale, nsmall = 1)
  }
  if(is.null(projection)){
    projection <- "NA"
  }
  if(is.null(qc)){
    qc <- "NA"
  }else{
    qc <- format(qc, nsmall = 1)
  }
  raw_file_name <- paste(sitename, source, collection, scale, projection, qc, sep = "_")
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
set_stage <- function(result, req_start, req_end, stage){
  db_start = as.Date(result$start_date)
  db_end = as.Date(result$end_date)
  req_start = as.Date(req_start)
  req_end = as.Date(req_end)
  stage <- TRUE
  merge <- TRUE
  
  # data already exists
  if((req_start >= db_start) && (req_end <= db_end)){
    req_start <- "dont write"
    req_end <- "dont write"
    stage <- FALSE
    merge <- FALSE
    write_start <- "dont write" 
    write_end <- "dont write" 
  }else if(req_start < db_start && db_end < req_end){
    # data has to be replaced
    merge <- "replace"
    write_start <-req_start
    write_end <-req_end
    stage <- TRUE
  }else if((req_start > db_start) && (req_end > db_end)){
    # forward case
    req_start <- db_end + 1
    write_start <- db_start
    write_end <- req_end
  }else if((req_start < db_start) && (req_end < db_end))     {
    # backward case
    req_end <- db_start - 1
    write_end <- db_end
    write_start <- req_start
  }
  return (list(req_start, req_end, stage, merge, write_start, write_end))
  
} 