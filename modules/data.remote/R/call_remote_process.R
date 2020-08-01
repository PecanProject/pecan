call_remote_process <- function(siteid, sitename, raw_mimetype, raw_formatname, geofile, outdir, start, end, source, collection, scale=NULL, projection=NULL, qc=NULL, algorithm=NULL, credfile=NULL, pro_mimetype=NULL, pro_formatname=NULL, out_get_data=NULL, out_process_data=NULL){
  
  reticulate::import_from_path("remote_process", "/home/carya/pecan/modules/data.remote/inst")
  reticulate::source_python('/home/carya/pecan/modules/data.remote/inst/remote_process.py')
  
  
  input_file <- NULL
  stage_get_data <- NULL
  stage_process_data <- NULL
  raw_merge <- NULL
  pro_merge <- NULL
  existing_raw_file_path <- NULL
  existing_pro_file_path <- NULL
  projection <- NULL
  flag <- 0
  
  collection_lut <- data.frame(stringsAsFactors=FALSE,
                          original_name = c("LANDSAT/LC08/C01/T1_SR", "COPERNICUS/S2_SR", "NASA_USDA/HSL/SMAP_soil_moisture"),
                          pecan_code = c("l8", "s2", "smap")
  )
  getpecancode <- collection_lut$pecan_code
  names(getpecancode) <- collection_lut$original_name
  
  collection = unname(getpecancode[collection])
  print(collection)

  req_start <- start
  req_end <- end
  
  raw_file_name = paste0(source, "_", collection, "_", sitename)
  
  existing_data <- PEcAn.DB::db.query(paste0("select * from inputs where site_id=", siteid), dbcon)
  
  if(nrow(existing_data) >= 1){
    
    if(!is.null(out_process_data)){
      # construct pro file name
      pro_file_name = paste0(sitename, "_", out_process_data, "_", algorithm)
      
      print("print pro file name")
      print(pro_file_name)
      
      # check if pro file exists
      if(nrow(pro_check <- PEcAn.DB::db.query(sprintf("select inputs.id, inputs.site_id, inputs.name, inputs.start_date, inputs.end_date, dbfiles.file_path from inputs INNER JOIN dbfiles ON inputs.name=dbfiles.file_name AND inputs.name LIKE '%s%%'", pro_file_name), dbcon)) == 1){
        print("were you here?")
        datalist <- set_date_stage(pro_check, req_start, req_end, stage_process_data)
        pro_start <- as.character(datalist[[1]])
        pro_end <- as.character(datalist[[2]])
        write_pro_start <- datalist[[5]]
        write_pro_end <- datalist[[6]]
        if(pro_start == "dont write" || pro_end == "dont write"){
          # break condition
        }else{
        stage_process_data <- datalist[[3]]
        pro_merge <- datalist[[4]]
        if(pro_merge == TRUE){
          existing_pro_file_path <- pro_check$file_path
        }else{
          existing_pro_file_path <- NULL
        }
        if(stage_process_data == TRUE){
          # check about the status of raw file
          raw_check <- PEcAn.DB::db.query(sprintf("select inputs.id, inputs.site_id, inputs.name, inputs.start_date, inputs.end_date, dbfiles.file_path from inputs INNER JOIN dbfiles ON inputs.name=dbfiles.file_name AND inputs.name LIKE '%s%%'", raw_file_name), dbcon)
          raw_datalist <- set_date_stage(raw_check, pro_start, pro_end, stage_get_data)
          start <- as.character(raw_datalist[[1]])
          end <- as.character(raw_datalist[[2]])
          write_raw_start <- raw_datalist[[5]]
          write_raw_end <- raw_datalist[[6]]
          stage_get_data <- raw_datalist[[3]]
          raw_merge <- raw_datalist[[4]]
          if(stage_get_data == FALSE){
            input_file = raw_check$file_path
          }
          if(raw_merge == TRUE){
            existing_raw_file_path <- raw_check$file_path
          }else{
            existing_raw_file_path <- NULL
          }
          print("here as well?")
        }
        
        }
      }else if(nrow(raw_check <- PEcAn.DB::db.query(sprintf("select inputs.id, inputs.site_id, inputs.name, inputs.start_date, inputs.end_date, dbfiles.file_path from inputs INNER JOIN dbfiles ON inputs.name=dbfiles.file_name AND inputs.name LIKE '%s%%'", raw_file_name), dbcon)) ==1){
        # atleast check if raw data exists
        print("printing raw file name")
        print(raw_file_name)
        print("whya are you here?")
        datalist <- set_date_stage(raw_check, req_start, req_end, stage_get_data)
        start <- as.character(datalist[[1]])
        end <- as.character(datalist[[2]])
        write_raw_start <- datalist[[5]]
        write_raw_end <- datalist[[6]]
        write_pro_start <- req_start
        write_pro_end <- req_end
        stage_get_data <- datalist[[3]]
        raw_merge <- datalist[[4]]
        print("showing status of raw merge")
        print(raw_merge)
        stage_process_data <- TRUE
        pro_merge <- FALSE
        if(raw_merge == TRUE){
          existing_raw_file_path = raw_check$file_path
        }else{
          existing_raw_file_path = NULL
        }
      }else{
        print("you shouldnt be here !!!!!!")
        # if no pro or raw of requested type exists
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
    }else if(nrow(raw_check <- PEcAn.DB::db.query(sprintf("select inputs.id, inputs.site_id, inputs.name, inputs.start_date, inputs.end_date, dbfiles.file_path from inputs INNER JOIN dbfiles ON inputs.name=dbfiles.file_name AND inputs.name LIKE '%s%%'", raw_file_name), dbcon)) == 1){
      # if only raw data is requested
      print(raw_check)
      datalist <- set_date_stage(raw_check, req_start, req_end, stage_get_data)
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
      # nothing of requested type exists
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
      }
    }
      
    
    
    
  }else{
    # db is completely empty for the given siteid
    flag <- 1
    print("i am here")
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
      existing_pro_file_path <- NULL
    }
  }
    
    

  
  #################### calling remote_process ##########################
  print(geofile)
  print(outdir)
  print(start)
  print(end)
  print(source)
  print(collection)
  print(scale)
  print(projection)
  print(qc)
  print(algorithm)
  print(input_file)
  print(credfile)
  print(out_get_data)
  print(out_process_data)
  print(stage_get_data)
  print(stage_process_data)
  print(raw_merge)
  print(pro_merge)
  print(existing_raw_file_path)
  print(existing_raw_file_path)
  output = remote_process(geofile=geofile, outdir=outdir, start=start, end=end, source=source, collection=collection, scale=scale, projection=projection, qc=qc, algorithm=algorithm, input_file=input_file, credfile=credfile, out_get_data=out_get_data, out_process_data=out_process_data, stage_get_data=stage_get_data, stage_process_data=stage_process_data, raw_merge=raw_merge, pro_merge=pro_merge, existing_raw_file_path=existing_raw_file_path, existing_pro_file_path=existing_pro_file_path)
  
  
  

  ################### saving files to DB ################################
  
  if(!is.null(out_process_data)){
    # if req pro and raw is for the first time being inserted - comment for down
    if(flag == 1){
      PEcAn.logger::logger.info("inserting for first time")
      # insert processed data
      PEcAn.DB::dbfile.input.insert(in.path = output$process_data_path, in.prefix = output$process_data_name, siteid = siteid, startdate = write_pro_start, enddate = write_pro_end, mimetype = pro_mimetype, formatname = pro_formatname, con = dbcon)
      # insert raw file
      PEcAn.DB::dbfile.input.insert(in.path = output$raw_data_path, in.prefix = output$raw_data_name, siteid = siteid, startdate = write_raw_start, enddate = write_raw_end, mimetype = raw_mimetype, formatname = raw_formatname, con = dbcon)
    }else if(stage_get_data == FALSE){
      pro_id = pro_check$id
      PEcAn.logger::logger.info("updating process")
      db.query(sprintf("UPDATE inputs set start_date='%s', end_date='%s', name='%s' where id=%f", write_pro_start, write_pro_end, output$process_data_name, pro_id), dbcon)
      db.query(sprintf("UPDATE dbfiles set file_path='%s', file_name='%s', where container_id=%f", output$process_data_path, output$process_data_name, pro_id), dbcon)
    }else{
      pro_id = pro_check$id
      raw_id = raw_check$id
      PEcAn.logger::logger.info("updating process and raw")
      db.query(sprintf("UPDATE inputs set start_date='%s', end_date='%s', name='%s' where id=%f", write_pro_start, write_pro_end, output$process_data_name, pro_id), dbcon)
      db.query(sprintf("UPDATE dbfiles set file_path='%s', file_name='%s' where container_id=%f", output$process_data_path, output$process_data_name, pro_id), dbcon)
      PEcAn.logger::logger.info("now updating raw")
      db.query(sprintf("UPDATE inputs set start_date='%s', end_date='%s', name='%s' where id=%f", write_raw_start, write_raw_end, output$raw_data_name, raw_id), dbcon)
      db.query(sprintf("UPDATE dbfiles set file_path='%s', file_name='%s' where container_id=%f", output$raw_data_path, output$raw_data_name, raw_id), dbcon)
    }
  }else{
    if(flag == 1){
      PEcAn.logger::logger.info(("inserting raw for the first time"))
      PEcAn.DB::dbfile.input.insert(in.path = output$raw_data_path, in.prefix = output$raw_data_name, siteid = siteid, startdate = write_raw_start, enddate = write_raw_end, mimetype = raw_mimetype, formatname = raw_formatname, con = dbcon)
    }else{
      PEcAn.logger::logger.info("updating raw data")
      raw_id = raw_check$id
      db.query(sprintf("UPDATE inputs set start_date='%s', end_date='%s', name='%s' where id=%f", write_raw_start, write_raw_end, output$raw_data_name, raw_id), dbcon)
      db.query(sprintf("UPDATE dbfiles set file_path='%s', file_name='%s' where container_id=%f", output$raw_data_path, output$raw_data_name, raw_id), dbcon)
    }
    
  }
  
  
  
  
  
  
 
  
  

}





set_date_stage <- function(result, req_start, req_end, stage){
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
    write_start <- NULL
    write_end <- NULL
  }else if(req_start < db_start && db_end < req_end){
    # file replace case [i think can be removed]
    merge <- "replace"
    write_start <-req_start
    write_end <-req_end
    stage <- TRUE
  }else if( ( (req_end > db_end) && (db_start <= req_start && req_start <= db_end) ) || ( (req_start > db_start) && (req_end > db_end))){
    # forward case
    print("extending forward")
    req_start <- db_end + 1
    write_start <- db_start
    write_end <- req_end
  }else if( ( (req_start < db_start) && (db_start <= req_end && req_end <= db_end))  ||  ( (req_start < db_start) && (req_end < db_end)))     {
    # backward case
    req_end <- db_start - 1
    write_end <- db_end
    write_start <- req_start
  }
  return (list(req_start, req_end, stage, merge, write_start, write_end))
  
} 

