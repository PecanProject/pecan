download.raw <- function(data.set,outfolder,pkg,raw.host,start_year,end_year){
  
  ## PSQL connection
  dbparms <- list(driver="PostgreSQL" , user = "bety", dbname = "bety", password = "bety", host = "psql-pecan.bu.edu")
  con     <- db.open(dbparms)
  

  # Check to see if input is already in dbfiles table 
  n <- nchar(outfolder)
  if(substr(outfolder,n,n) != "/"){outfolder = paste0(outfolder,"/")}
  
  check <- input.name.check(data.set, con, dbparams)
  if(is.null(check)==FALSE){
    print('Input is already in the database.')
    file = db.query(paste0("SELECT * from dbfiles where file_path = '", outfolder, "'"),con)
    if(is.na(file$container_id) || !(file$container_id == check && file$container_type == 'Input')){
      db.query(paste0("UPDATE dbfiles SET container_id = ",check,", container_type = 'Input' where file_path = '",outfolder,"'"),con)
      print("Updated db file")   
    }else{print("Didn't need to add/update db file")}
    db.close(con)
    return(check) 
  }
  
  # Download files
  
  fcn      <- paste0("download.",data.set)
  args     <- c(pkg,fcn,outfolder,start_year,end_year)
  cmdArgs  <- paste(args,collapse=" ")
  Rfcn     <- "pecan/scripts/Rfcn.R"
  host     <- system("hostname",intern=TRUE)
  username <- NULL 
  

  if(raw.host %in% c("localhost",host)){
    ## if the machine is local, run conversion function
    system(paste(Rfcn,cmdArgs))
  }else{
    ## if the machine is remote, run conversion remotely
    usr = ifelse(username==NULL | username=="","",paste0(username,"@"))
    system2("ssh",paste0(usr,paste(raw.host,Rfcn,cmdArgs)))
  }
  
  # Add new record

  filename   <- paste0(outfolder,data.set,".")
  formatname <- 'CF Meteorology'
  mimetype   <- 'application/x-netcdf'
  site.id    <- db.query(paste0("SELECT * from sites where sitename = '", data.set, " Domain'"),con)[["id"]] #Easy fix - Could be problematic... 
  
  startdate <- paste0(start_year,"-01-01 00:00:00")
  enddate <- paste0(end_year,"-12-31 23:59:00")
  
  newinput <- dbfile.input.insert(filename, site.id, startdate, enddate, mimetype, formatname, parentid=NA, con, raw.host, name=data.set) 
  
  return(newinput$input.id)
  
  
}



