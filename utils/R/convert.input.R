##' Convert input by applying fcn and insert new record into database
##'
##'

convert.input <- function(input.id,outfolder,pkg,fcn,write,username,dbparms,con,...){
  
  l <- list(...)
  n <- nchar(outfolder)
  if(substr(outfolder,n,n) != "/"){outfolder = paste0(outfolder,"/")}
  
  outname = tail(unlist(strsplit(outfolder,'/')),n=1)
  
  # Check to see if input is already in dbfiles table 
  check <- input.name.check(outname, con, dbparams)
  if(is.null(check)==FALSE){
    logger.error('Input is already in the database.')
    db.close(con)
    return(check) 
  }
  
  input = db.query(paste("SELECT * from inputs where id =",input.id),con)
  if(nrow(input)==0){print(c("input not found",input.id));db.close(con);return(NULL)}
  
  # dbfile may return more than one row -> may need to loop over machine ids
  dbfile = db.query(paste("SELECT * from dbfiles where container_id =",input.id," and container_type = 'Input'"),con)
  if(nrow(dbfile)==0){print(c("dbfile not found",input.id));db.close(con);return(NULL)}
  
  machine = db.query(paste("SELECT * from machines where id = ",dbfile$machine_id),con)
  if(nrow(machine)==0){print(c("machine not found",dbfile$machine_id));db.close(con);return(NULL)}
  
  host = system("hostname",intern=TRUE)
  
  args = c(pkg,fcn,dbfile$file_path,dbfile$file_name,outfolder)
  
  # Use existing site, unless otherwise specified (ex: subsetting case)
  if("newsite" %in% names(l) && is.null(l[["newsite"]])==FALSE){
    site = db.query(paste("SELECT * from sites where id =",l$newsite),con)
    if(nrow(site)==0){logger.error("Site not found"); db.close(con);return(NULL)} 
    if(!(is.na(site$geometry))){
      site$lon <- db.query(paste("SELECT ST_X(geometry) AS lon FROM sites where id =",l$newsite),con)[[1]]
      site$lat <- db.query(paste("SELECT ST_Y(geometry) AS lat FROM sites where id =",l$newsite),con)[[1]]
      args = c(args, site$lat, site$lon)
    }else{logger.error("No lat and lon for extraction site"); db.close(con);return(NULL)}
  }else{
    site  = db.query(paste("SELECT * from sites where id =",input$site_id),con) 
    if(nrow(site)==0){logger.error("Site not found");db.close(con);return(NULL)} 
  }      
  
  cmdArgs = paste(args,collapse=" ")
  #  Rfcn = system.file("scripts/Rfcn.R", package = "PEcAn.all")
  Rfcn = "pecan/scripts/Rfcn.R"
  
  #chkArgs = paste(c("PEcAn.data.atmosphere", "extract.success",args[3:5]),collapse=" ")
  
  if(machine$hostname %in% c("localhost",host)){
    ## if the machine is local, run conversion function
    system(paste0("~/",Rfcn," ",cmdArgs))
    #success <- system(paste(Rfcn,chkArgs),intern=TRUE)
  } else {
    ## if the machine is remote, run conversion remotely
    usr = ifelse(username==NULL | username=="","",paste0(username,"@"))
    system2("ssh",paste0(usr,paste(machine$hostname,Rfcn,cmdArgs)))
    #success <- system(paste0("ssh ",usr,paste(machine$hostname,Rfcn,chkArgs)),intern=TRUE)
  }
  
  ## Check if the conversion was successful, currently not very robust
#   if(unlist(strsplit(success,' '))[2] == TRUE){
#     logger.info("Conversion was successful")
#   }else{
#     logger.error("Conversion was not successful")
#     db.close(con)
#     return(NULL)
#   }
  
  ### NOTE: We will eventually insert Brown Dog REST API calls here
  
  ## insert new record into database
  if(write==TRUE){
    outlist <- unlist(strsplit(outname,"_"))
    if("ED" %in% outlist){
      filename <- paste0(outfolder," ")
      formatname <- 'ed.met_driver_header files format'
      mimetype <- 'text/plain'
    }else if("SIPNET" %in% outlist){
      filename <- paste0(outfolder," ")
      formatname <- 'Sipnet.climna'
      mimetype <- 'text/csv'
    }else{
      filename <- paste0(outfolder,dbfile$file_name)
      formatname <- 'CF Meteorology'
      mimetype <- 'application/x-netcdf'
    }
    
    newinput <- dbfile.input.insert(filename, site$id, paste(input$start_date), paste(input$end_date), 
                                    mimetype, formatname,input$id,con=con,machine$hostname) 
    return(newinput$input.id)
  }else{
    logger.warn('Input was not added to the database')
    db.close(con)
    return(NULL)
  }
  db.close(con)
}
