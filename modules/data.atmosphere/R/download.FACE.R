# Download Raw FACE data from the internet
download.FACE <- function(site.name,outfolder,pkg,raw.host,start_year,end_year,site.id,dbparams,con){
  
  case   <- c("a","e")
  raw.id <- list( a = NA, b = NA)
  
  for(i in 1:2){
    lname <- unlist(strsplit(outfolder,'/'))
    
    outname = paste0(tail(lname,n=1),"_",case[i]) 
    outfolder = paste0(paste0(lname,collapse = "/"),"_",case[i],"/") 
    print(outname)
    print(outfolder)

  
  # Check database for file, and instert if not already there.

  site <- site.name
  
  raw.file <- paste0(site,"_forcing_h.nc")
  new.file <- paste0(outfolder,"FACE_",site,"_",case[i],"_forcing_h.nc")

  cmd1 <- paste0("mkdir -p ", outfolder)
  cmd2 <- paste0("wget -c ftp://cdiac.ornl.gov/.private/eCO2_Modelling/Site_Data/",site,"/", raw.file, " -O ",new.file)
  cmd3 <- paste0(" ncdump -v 'YEAR' ",new.file)
  
  if(i == 1){
    cmd4 <-  paste0("ncks -x -v ",case[2],"CO2,",case[2],"O3  ", new.file ," -O ", new.file)
  }else{cmd4 <-  paste0("ncks -x -v ",case[1],"CO2,",case[1],"O3  ", new.file ," -O ", new.file)}
  
  Rfcn     <- "pecan/scripts/Rfcn.R"
  host     <- system("hostname",intern=TRUE)
  
  if(raw.host %in% c("localhost",host)){
    ## if the machine is local, run conversion function
    system(cmd1); system(cmd2)
    years <- system(cmd3,intern=TRUE)
    system(cmd4)
  }else{
    ## if the machine is remote, run conversion remotely
    usr = ifelse(username==NULL | username=="","",paste0(username,"@"))
    system2("ssh",paste0(usr,paste(raw.host,cmd1)))
    system2("ssh",paste0(usr,paste(raw.host,cmd2)))
    years <- system2("ssh",paste0(usr,paste0(raw.host,cmd3)),stdout=TRUE)
    system2("ssh",paste0(usr,paste(raw.host,cmd4)))
  }
    
  check <- input.name.check(outname, con, dbparams)
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
  
  filename <- paste0(outfolder,data.set,"_",case[i])
  type <- 'Input'
  
  formatname <- 'FACE'
  mimetype <- 'application/x-netcdf'
  
  newinput <- dbfile.input.insert(filename, 
                                  siteid = site.id, 
                                  startdate = start_date, 
                                  enddate = end_date, 
                                  mimetype, 
                                  formatname,
                                  parentid=NA,
                                  con = con,
                                  hostname = raw.host) 
  
  raw.id[i] <- newinput$input.id
}
return(raw.id)
} 
