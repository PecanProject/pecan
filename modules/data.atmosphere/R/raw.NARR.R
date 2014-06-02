raw.NARR <- function(outfolder,start_year,end_year,pkg,NARR.host){
  
  # Check database for file, and instert if not already there.
  args     <- c(pkg,"download.NARR",outfolder,start_year,end_year)  
  cmdArgs  <- paste(args,collapse=" ")
  Rfcn     <- "pecan/scripts/Rfcn.R"
  host     <- system("hostname",intern=TRUE)
  username <- NULL
  
  if(NARR.host %in% c("localhost",host)){
    ## if the machine is local, run conversion function
    system(paste(Rfcn,cmdArgs))
  }else{
    ## if the machine is remote, run conversion remotely
    usr = ifelse(username==NULL | username=="","",paste0(username,"@"))
    system2("ssh",paste0(usr,paste(NARR.host,Rfcn,cmdArgs)))
  }
  
  dbparms  <- list(driver="PostgreSQL" , user = "bety", dbname = "bety", password = "bety", host = "psql-pecan.bu.edu")
  con      <- db.open(dbparms)
  input.id <- db.query(paste0("SELECT id FROM inputs WHERE name = '","NARR", "'"), con, dbparams)[['id']]
  
  n <- nchar(outfolder)
  if(substr(outfolder,n,n) != "/") outfolder = paste0(outfolder,"/")
  filename <- paste0(outfolder,"NARR.")
  type <- 'Input'
  
  file = db.query(paste0("SELECT * from dbfiles where file_path = '", outfolder, "'"),con)
  
  if(all(dim(file) == c(0, 0))){
    file.id = dbfile.insert(filename, type, input.id, con, host)
    print("Added db file!")
  }else{
    if(!(file$container_id == input.id && file$container_type == 'Input')){
      db.query(paste0("UPDATE dbfiles SET container_id = ",input.id,", container_type = 'Input' where file_path = '",outfolder,"'"),con)
      print("Updated db file!")   
    }else{print("Didn't need to add/update db file!")}
  }
   return(invisible(db.query(paste0("SELECT * from dbfiles where file_path = '", outfolder, "'"), con)[['id']]))
}

