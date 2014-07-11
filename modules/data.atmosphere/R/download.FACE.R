download.FACE <- function(data.set,outfolder,pkg,raw.host,start_year,end_year){
  # Download Raw FACE data from the internet
  
  n <- nchar(outfolder)
  if(substr(outfolder,n,n) != "/") outfolder = paste0(outfolder,"/")
  
  # Check database for file, and instert if not already there.
  
  args     <- c(pkg,fcn,outfolder)  
  cmdArgs  <- paste(args,collapse=" ")
  Rfcn     <- "pecan/scripts/Rfcn.R"
  host     <- system("hostname",intern=TRUE)
  username <- NULL
  
  site.list <- c("DUKE","RHIN") #,"KSCO","NDFF","ORNL","PHAC"
  site.num  <- c(853,1000000003) # Haven't made sites yet
  N <- length(site.list)
  
  raw.id <- c()
  
  for (i in 1:N){
    
    site <- site.list[i]
    site.id <- site.num[i]
    
    outfolder.site <- paste0(outfolder,site,"/")
    cmd <- paste0("mkdir -p ",outfolder.site,"; cd ",outfolder.site,"; wget -c ftp://cdiac.ornl.gov/.private/eCO2_Modelling/Site_Data/",site,"/", site,"_forcing_h.nc")
    
    if(is.null())
    
    if(raw.host %in% c("localhost",host)){
      ## if the machine is local, run conversion function
      system(cmd)
    }else{
      ## if the machine is remote, run conversion remotely
      usr = ifelse(username==NULL | username=="","",paste0(username,"@"))
      system2("ssh",paste0(usr,paste(raw.host,cmd)))
      years <- system2("ssh",paste0(usr,paste0(raw.host," ncdump -v 'YEAR'  /projectnb/cheas/pecan.data/input/FACE/",site,"/",site,"_forcing_h.nc")),stdout=TRUE)
    }
    
    dbparms  <- list(driver="PostgreSQL" , user = "bety", dbname = "bety", password = "bety", host = "psql-pecan.bu.edu")
    con      <- db.open(dbparms)
    
    check <- input.name.check(site, con, dbparams)
    if(is.null(check)==FALSE){
      logger.error('Input is already in the database.')
      db.close(con)
      raw.id <- c(raw.id, check)
    }
    
    j <- grep("YEAR =",years)
    ys <- as.numeric(substr(unlist(strsplit(years[j],","))[1],nchar(unlist(strsplit(years[j],","))[1])-4,nchar(unlist(strsplit(years[j],","))[1]) ))
    ye <- as.numeric(unlist(strsplit(years[length(years)-1],";"))[1])
    
    start_date <- paste0(ys,"-01-01 00:00:00")
    end_date   <- paste0(ye,"-12-31 23:59:00")
    
    filename <- paste0(outfolder.site,site)
    type <- 'Input'
        
    formatname <- 'FACE'
    mimetype <- 'application/x-netcdf'
    filename <- paste0(outfolder,dbfile$file_name)
    
    newinput <- dbfile.input.insert(filename, site$id, paste(input$start_date), paste(input$end_date), 
                                    mimetype, formatname,input$id,con=con,machine$hostname,outname) 
    raw.id <- c(raw.id, newinput$input.id)
    
  }
  return(raw.id)
} 
