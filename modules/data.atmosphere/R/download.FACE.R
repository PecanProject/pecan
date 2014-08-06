# Download Raw FACE data from the internet
download.FACE <- function(data.set,outfolder,pkg,raw.host,start_year,end_year,site.id,dbparams,con){
  
  n <- nchar(outfolder)
  if(substr(outfolder,n,n) != "/") outfolder = paste0(outfolder,"/")
  
  # Check database for file, and instert if not already there.

  site <- unlist(strsplit(data.set,"_"))[[2]]
  
  raw.file <- paste0(site,"_forcing_h.nc")

  cmd1 <- paste0("mkdir -p ", outfolder)
  cmd2 <- paste0("wget -c ftp://cdiac.ornl.gov/.private/eCO2_Modelling/Site_Data/",site,"/", raw.file, " -O ",outfolder,"FACE_",raw.file)
  
  Rfcn     <- "pecan/scripts/Rfcn.R"
  host     <- system("hostname",intern=TRUE)
  
  if(raw.host %in% c("localhost",host)){
    ## if the machine is local, run conversion function
    system(cmd1); system(cmd2)
  }else{
    ## if the machine is remote, run conversion remotely
    usr = ifelse(username==NULL | username=="","",paste0(username,"@"))
    system2("ssh",paste0(usr,paste(raw.host,cmd1)))
    system2("ssh",paste0(usr,paste(raw.host,cmd2)))
    years <- system2("ssh",paste0(usr,paste0(raw.host," ncdump -v 'YEAR' ",outfolder,"FACE_",raw.file)),stdout=TRUE)
  }
  
  check <- input.name.check(data.set, con, dbparams)
  if(is.null(check)==FALSE){
    logger.error('Input is already in the database.')
    raw.id <- check
    return(raw.id)
  }
  
  if(is.na(start_year) |is.na(end_year)){
    j <- grep("YEAR =",years)
    start_year <- as.numeric(substr(unlist(strsplit(years[j],","))[1],nchar(unlist(strsplit(years[j],","))[1])-4,nchar(unlist(strsplit(years[j],","))[1]) ))
    end_year <- as.numeric(unlist(strsplit(years[length(years)-1],";"))[1])
  }
  
  start_date <- paste0(start_year,"-01-01 00:00:00")
  end_date   <- paste0(end_year,"-12-31 23:59:00")
  
  filename <- paste0(outfolder,data.set)
  type <- 'Input'
  
  formatname <- 'FACE'
  mimetype <- 'application/x-netcdf'
  
  #(filename, siteid, startdate, enddate, mimetype, formatname, parentid=NA, con, hostname=fqdn())
  newinput <- dbfile.input.insert(filename, site.id, start_date, end_date, mimetype,formatname,parentid=NA,con=con,raw.host) 
  
  raw.id <- c(newinput$input.id)

return(raw.id)
} 
