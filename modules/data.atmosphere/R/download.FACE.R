download.FACE <- function(data.set,outfolder,pkg,raw.host){
  # Download Raw FACE data from the internet
  
  n <- nchar(outfolder)
  if(substr(outfolder,n,n) != "/") outfolder = paste0(outfolder,"/")
  
  # Check database for file, and instert if not already there.
  
  args     <- c(pkg,fcn,outfolder,l)  
  cmdArgs  <- paste(args,collapse=" ")
  Rfcn     <- "pecan/scripts/Rfcn.R"
  host     <- system("hostname",intern=TRUE)
  username <- NULL
  
  site.list <- c("DUKE","KSCO","NDFF","ORNL","PHAC","RHIN")
  
  raw.id <- ()
  
  for (site in site.list){
    
    outfolder.site <- paste0(outfolder,site,"/")
    if(!file.exists(outfolder.site)) dir.create(outfolder.site)
    cmd <- paste("wget -c ftp://cdiac.ornl.gov/.private/eCO2_Modelling/Site_Data/",site,"/", site,"_forcing_h.nc -O ",outfolder.site, site,"_forcing_h.nc",sep="")
    
    if(raw.host %in% c("localhost",host)){
      ## if the machine is local, run conversion function
      system(cmd)
    }else{
      ## if the machine is remote, run conversion remotely
      usr = ifelse(username==NULL | username=="","",paste0(username,"@"))
      system2("ssh",paste0(usr,paste(raw.host,Rfcn,cmdArgs)))
    }
    
    dbparms  <- list(driver="PostgreSQL" , user = "bety", dbname = "bety", password = "bety", host = "psql-pecan.bu.edu")
    con      <- db.open(dbparms)
    
    check <- input.name.check(site, con, dbparams)
    if(is.null(check)==FALSE){
      logger.error('Input is already in the database.')
      db.close(con)
      return(check) 
    }
    
    
    input.id <- db.query(paste0("SELECT id FROM inputs WHERE name = '",site, "'"), con, dbparams)
    
    
    filename <- paste0(outfolder.site,site)
    type <- 'Input'
    
    formatname <- 'FACE'
    mimetype <- 'application/x-netcdf'
    outlist <- unlist(strsplit(outname,"_"))
    if("ED" %in% outlist){filename <- paste0(outfolder," ")
    }else{filename <- paste0(outfolder,dbfile$file_name)}
    
    newinput <- dbfile.input.insert(filename, site$id, paste(input$start_date), paste(input$end_date), 
                                    mimetype, formatname,input$id,con=con,machine$hostname,outname) 
    raw.id <- c(raw.id, newinput$input.id)
    
  }
  return(raw.id)
} 
