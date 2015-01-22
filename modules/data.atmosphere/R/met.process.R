##' @name met.process
##' @title met.process
##' @export
##'
##' @param site Site info from settings file
##' @param input currently "NARR" or "Ameriflux"
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param model model_type name
##' @param host Host info from settings file
##' @param bety  database settings from settings file
##' @param dir  directory to write outputs to
##' 
##' @author Elizabeth Cowdery
met.process <- function(site, input, start_date, end_date, model, host, bety, dir){
  
  require(PEcAn.all)
  require(PEcAn.data.atmosphere)
  require(RPostgreSQL)
  
  driver   <- "PostgreSQL"
  user     <- bety$user
  dbname   <- bety$dbname
  password <- bety$password
  bety.host<- bety$host
  username <- ""
  dbparms <- list(driver=driver, user=user, dbname=dbname, password=password, host=bety.host)
  con       <- db.open(dbparms)
  
  met <- input 
  ifelse(met == "NARR", regional<- TRUE, regional<- FALSE) # Either regional or site run
  
  #--------------------------------------------------------------------------------------------------#
  # Download raw met from the internet 
  
  outfolder  <- paste0(dir,met,"/")
  pkg        <- "PEcAn.data.atmosphere"
  fcn        <- paste0("download.",met)
  if(met == "NARR"){
    site.id <- 1135
    raw.id <- 1000000127
  }else{
    site.id <- site
    args <- list(site.id, outfolder, start_date, end_date, overwrite=FALSE, verbose=FALSE) #, pkg,raw.host = host,dbparms,con=con)
    raw.id <- do.call(fcn,args)
  } 
  
  #--------------------------------------------------------------------------------------------------#
  # Change to CF Standards
  
  input.id  <-  raw.id
  outfolder <-  paste0(dir,met,"_CF/")
  pkg       <- "PEcAn.data.atmosphere"
  fcn       <-  paste0("met2CF.",met)
  formatname <- 'CF Meteorology'
  mimetype <- 'application/x-netcdf'
  
  if(met == "NARR"){
    cf.id <- 1000000023 #ID of permuted CF files
  }else{
    cf.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id,start_date,end_date,pkg,fcn,write,username,con=con,raw.host=host,write=TRUE) 
  }
  
  #--------------------------------------------------------------------------------------------------#
  # Extraction 
  
  if(regional){ #ie NARR right now
    
    str_ns    <- paste0(site %/% 1000000000, "-", site %% 1000000000)
    
    input.id <- cf.id
    outfolder <- paste0(dir,met,"_CF_site_",str_ns,"/")
    pkg       <- "PEcAn.data.atmosphere"
    fcn       <- "extract.nc"
    formatname <- 'CF Meteorology'
    mimetype <- 'application/x-netcdf'
    
    ready.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id,start_date,end_date,pkg,fcn,write,username=username,con=con,
                              newsite = site,raw.host=host,write=TRUE)
 
    }else{   
    # run gapfilling 
    ready.id <- convert.input()
    }
  
  #--------------------------------------------------------------------------------------------------#
  # Prepare for Model
  
  if(model == "ED2"){
    formatname <- 'ed.met_driver_header_files_format'
    mimetype <- 'text/plain'
  }else if(model == "SIPNET"){
    formatname <- 'Sipnet.climna'
    mimetype <- 'text/csv'
  }else if(model == "BIOCRO"){
    formatname <- 'biocromet'
    mimetype <- 'text/csv'
  }else if(model == "DALEC"){
    formatname <- 'DALEC meteorology'
    mimetype <- 'text/plain'
  }
  
  lst <- site.lst(site,con)
  
  # Convert to model format
  input.id  <- ready.id
  outfolder <- paste0(dir,met,"_",model,"_site_",str_ns,"/")
  pkg       <- paste0("PEcAn.",model)
  fcn       <- paste0("met2model.",model)
  write     <- TRUE
  overwrite <- ""
  
  model.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id,start_date,end_date,pkg,fcn,write,username=username,con=con,
                            lst=lst,overwrite=overwrite,raw.host=host,write=TRUE)

  db.close(con)
  return(outfolder)
  
}
