permute.nc <- function(input.id,outfolder,write){

  # Rechunk dimensions and convert the time dimension from unlimited to fixed.

  n <- nchar(outfolder)
  if(substr(outfolder,n,n) != "/"){outfolder = paste0(outfolder,"/")}
  
  outname = tail(unlist(strsplit(outfolder,'/')),n=1)

  ## Query inputs, site, dbfiles, machine
  dbparms <- list(driver="PostgreSQL" , user = "bety", dbname = "bety", password = "bety", host = "psql-pecan.bu.edu")
  con     <- db.open(dbparms)
  
  # Check to see if input is already in dbfiles table 
  check <- input.name.check(outname, con, dbparams)
  if(is.null(check)==FALSE){
    logger.error('Input is already in the database.')
    db.close(con)
    return(check) 
  }
  
  dbfile = db.query(paste("SELECT * from dbfiles where container_id =",input.id," and container_type = 'Input'"),con)
  if(nrow(dbfile)==0){print(c("dbfile not found",input.id));return(NULL)}
  
  in.path  <- dbfile$file_path
  prefix   <- dbfile$file_name
  out.path <- outfolder
  
  ## get file names
  
  files = dir(in.path,prefix)
  files = files[grep(pattern="*.nc",files)]
  
  if(length(files) == 0) return(NULL)
  
  if(!file.exists(out.path)) dir.create(out.path)
  
  for(i in 1:length(files)){    
    
    infile = file.path(in.path,files[i])
    tempfile = file.path(out.path,paste0(files[i],"_temp"))
    outfile = file.path(out.path,files[i])
    
    if(file.exists(infile)==TRUE && file.exists(outfile)==FALSE){
      system(paste0("nccopy -k 3 -u -c time/8,y/277,x/349 ",infile," ",tempfile))
      system(paste0("ncpdq --no_tmp_fl -h -O -a y,x,time ",tempfile," ",outfile))
    }
  }

  ## insert new record into database
  if(write==TRUE){
    formatname <- 'CF Meteorology'
    mimetype <- 'application/x-netcdf'
    filename <- paste0(outfolder,"NARR.")
    
    newinput <- dbfile.input.insert(filename, site$id, paste(input$start_date), paste(input$end_date), 
                                    mimetype, formatname,input$id,con=con,machine$hostname,outname) 
    db.close(con)
    return(newinput$input.id)
  }else{
    logger.warn('Input was not added to the database')
    db.close(con)
    return(NULL)
  }
}