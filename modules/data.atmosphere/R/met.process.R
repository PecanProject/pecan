met.process <- function(site, input, start_date, end_date, model, host, con){
  
  require(PEcAn.all)
  require(PEcAn.data.atmosphere)
  require(RPostgreSQL)
  
  met <- input 
  ifelse(met == "NARR", regional<- TRUE, regional<- FALSE) # Either regional or site run
  
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date   <- as.POSIXlt(end_date, tz = "GMT")
  
  username <- ""
  dir      <- "/fs/data4/"
  
  #--------------------------------------------------------------------------------------------------#
  # Download raw met from the internet 
  
  outfolder  <- paste0(dir,met,"/")
  pkg        <- "PEcAn.data.atmosphere"
  fcn        <- paste0("download.",met)
  ifelse(met == "NARR", site.id <- 1135, site.id <- site$id)
  
  args <- list(site.id, outfolder, start_date, end_date, overwrite=FALSE, verbose=FALSE, pkg,raw.host = host$name,dbparms,con=con,write=TRUE)
  
  raw.id <- do.call(fcn,args)
  print(raw.id)
  
  #--------------------------------------------------------------------------------------------------#
  # Change to CF Standards
  
  input.id  <-  raw.id
  outfolder <-  paste0(dir,met,"_CF/")
  pkg       <- "PEcAn.data.atmosphere"
  fcn       <-  paste0("met2CF.",met)
  formatname <- 'CF Meteorology'
  mimetype <- 'application/x-netcdf'
  
  cf.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id,start_date,end_date,pkg,fcn,write,username,con=con,raw.host=host$name)
  
  
#   
#   if(regional){ #ie NARR right now
#     
#     site <- site$id
#     
#     str_ns    <- paste0(site %/% 1000000000, "-", site %% 1000000000)
#     
#     if (extract == TRUE){
#       input.id <- cf.id
#       outfolder <- paste0(dir,met,"_CF_site_",str_ns,"/")
#       pkg       <- "PEcAn.data.atmosphere"
#       fcn       <- "extract.nc"
#       write     <- TRUE
#       formatname <- 'CF Meteorology'
#       mimetype <- 'application/x-netcdf'
#       
#       ready.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id,start_year,end_year,pkg,fcn,write,username,con=con,newsite = site,raw.host=host$name,write=TRUE)
#     }else{
#       
#       # run gapfilling 
#       ready.id <- convert.input()
#     }
#     
#     #--------------------------------------------------------------------------------------------------#
#     # Prepare for Model
#     
#     if(model == "ED2"){
#       mod.formatname <- 'ed.met_driver_header_files_format'
#       mod.mimetype <- 'text/plain'
#     }else if(model == "SIPNET"){
#       mod.formatname <- 'Sipnet.climna'
#       mod.mimetype <- 'text/csv'
#     }else if(model == "BIOCRO"){
#       mod.formatname <- 'biocromet'
#       mod.mimetype <- 'text/csv'
#     }else if(model == "DALEC"){
#       mod.formatname <- 'DALEC meteorology'
#       mod.mimetype <- 'text/plain'
#     }
#     
#     source("modules/data.atmosphere/R/site.lst.R")
#     lst <- site.lst(newsite,con)
#     
#     # Convert to model format
#     input.id  <- ready.id
#     outfolder <- paste0(dir,met,"_",model,"_site_",str_ns,"/")
#     pkg       <- paste0("PEcAn.",model)
#     fcn       <- paste0("met2model.",model)
#     write     <- TRUE
#     overwrite <- ""
#     
#     model.id <- convert.input(input.id,outfolder,mod.formatname,mod.mimetype,newsite,start_year,end_year,pkg,fcn,write,username,con=con,lst=lst,overwrite=overwrite,raw.host=raw.host,write=TRUE)
#     
#     
#   }
  
  
  
  
  
}