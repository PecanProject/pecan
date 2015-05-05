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
##' @param dbparms  database settings from settings file
##' @param dir  directory to write outputs to
##' 
##' @author Elizabeth Cowdery, Michael Dietze
met.process <- function(site, input_met, start_date, end_date, model, host, dbparms, dir, browndog=NULL){
  
  require(RPostgreSQL)
  require(XML)
  
  #get met source and potentially determine where to start in the process
  met <- ifelse(is.null(input_met$source), logger.error("Must specify met source"),input_met$source)
  
  download.raw <- ifelse(is.null(input_met$id),TRUE,FALSE) 
  # What will that id be? The raw id? Or the "closest" to what the user wants?
  # Then unnecessary steps could be skipped?
  
  #read in registration xml for met specific information
  register <- xmlToList(xmlParse(file.path(find.package("PEcAn.data.atmosphere"),"registration",paste0("register.",met,".xml"))))
                        
  #setup connection and host information
  con      <- db.open(dbparms)
  username <- ""  
  ifelse(host$name == "localhost", machine.host <- fqdn(), machine.host <- hostname)
  machine = db.query(paste0("SELECT * from machines where hostname = '",machine.host,"'"),con)
  
  #setup additional browndog arguments
  if(!is.null(browndog)){browndog$inputtype <- register$format$inputtype}
  
  #setup site database number, lat, lon and name
  new.site <- data.frame(id = as.numeric(site$id), lat = db.site.lat.lon(site$id,con=con)$lat, lon = db.site.lat.lon(site$id,con=con)$lon)
  str_ns    <- paste0(new.site$id %/% 1000000000, "-", new.site$id %% 1000000000)  

  #--------------------------------------------------------------------------------------------------#
  # Download raw met from the internet 
  
  outfolder  <- file.path(dir,met)
  pkg        <- "PEcAn.data.atmosphere"
  fcn        <- paste0("download.",met)
  
  if(register$scale=="regional" & download.raw==TRUE){ #Right now this only means NARR but will need to be generalized once we have more regional met products
    
    print("start CHECK")
    check = db.query(
      paste0("SELECT i.start_date, i.end_date, d.file_path, d.container_id, d.id  from dbfiles as d join inputs as i on i.id = d.container_id where i.site_id =",register$siteid,
             " and d.container_type = 'Input' and i.format_id=",register$format$id, " and d.machine_id =",machine$id,
             " and (i.start_date, i.end_date) OVERLAPS (DATE '", as.POSIXlt(start_date, tz = "GMT"),"',DATE '",as.POSIXlt(end_date, tz = "GMT"),"')"),con)
    print("end CHECK")
    options(digits=10)
    print(check)
    if(length(check)>0){
      raw.id <- list(input.id=check$container_id, dbfile.id=check$id)
    }else{
      
      args <- list(outfolder, start_date, end_date)
      cmdFcn  = paste0(pkg,"::",fcn,"(",paste0("'",args,"'",collapse=","),")")
      new.files <- remote.execute.R(cmdFcn,host$name,user=NA, verbose=TRUE)
      
      raw.id <- dbfile.input.insert(in.path=dirname(new.files$file[1]),
                                    in.prefix=new.files$dbfile.name[1], 
                                    siteid = site$id,  
                                    startdate = start_date, 
                                    enddate = end_date, 
                                    mimetype=new.files$mimetype[1], 
                                    formatname=new.files$formatname[1],
                                    parentid = NA,
                                    con = con,
                                    hostname = host$name)
    }
    
  }else if(register$scale=="site" & download.raw==TRUE) { # Site-level met
    
    print("start CHECK")
    check = db.query(
      paste0("SELECT i.start_date, i.end_date, d.file_path, d.container_id, d.id  from dbfiles as d join inputs as i on i.id = d.container_id where i.site_id =",site$id,
             " and d.container_type = 'Input' and i.format_id=",register$format$id, " and d.machine_id =",machine$id,
             " and (i.start_date, i.end_date) OVERLAPS (DATE '", as.POSIXlt(start_date, tz = "GMT"),"',DATE '",as.POSIXlt(end_date, tz = "GMT"),"')"),con)
    print("end CHECK")
    options(digits=10)
    print(check)
    if(length(check)>0){
      raw.id <- list(input.id=check$container_id, dbfile.id=check$id)
    }else{
      
      outfolder = paste0(outfolder,"_site_",str_ns)
      args <- list(site$name, outfolder, start_date, end_date)
      
      cmdFcn  = paste0(pkg,"::",fcn,"(",paste0("'",args,"'",collapse=","),")")
      new.files <- remote.execute.R(script=cmdFcn,host=host$name,user=NA,verbose=TRUE,R="R")
      
      ## insert database record
      raw.id <- dbfile.input.insert(in.path=dirname(new.files$file[1]),
                                    in.prefix=new.files$dbfile.name[1], 
                                    siteid = site$id, 
                                    startdate = start_date, 
                                    enddate = end_date, 
                                    mimetype=new.files$mimetype[1], 
                                    formatname=new.files$formatname[1],
                                    parentid=NA,
                                    con = con,
                                    hostname = host$name)
    }    
  }
  
  
  #--------------------------------------------------------------------------------------------------#
  # Change to  CF Standards
    
  print("### Change to CF Standards")
  
  input.id  <-  raw.id[1]
  pkg       <- "PEcAn.data.atmosphere"
  formatname <- 'CF Meteorology'
  mimetype <- 'application/x-netcdf'
  format.id <- 33
  
  
  if(register$scale=="regional"){   
    
    input_name <- paste0(met,"_CF")
    outfolder  <- file.path(dir,input_name)
    
    print("start CHECK")
    check = db.query(
      paste0("SELECT i.start_date, i.end_date, d.file_path, d.container_id, d.id  from dbfiles as d join inputs as i on i.id = d.container_id where i.site_id =",register$siteid,
             " and d.container_type = 'Input' and i.format_id=",format.id, " and d.machine_id =",machine$id, " and i.name = '", input_name,
             "' and (i.start_date, i.end_date) OVERLAPS (DATE '", as.POSIXlt(start_date, tz = "GMT"),"',DATE '",as.POSIXlt(end_date, tz = "GMT"),"')"),con)
    print("end CHECK")
    options(digits=10)
    print(check)
    if(length(check)>0){
      cf0.id <- list(input.id=check$container_id, dbfile.id=check$id)
    }else{
      
      fcn1 <- paste0("met2CF.",met)
      fcn2 <- paste0("met2CF.",register$format$mimetype)
      if(exists(fcn1)){
        fcn <- fcn1
      }else if(exists(fcn2)){
        fcn <- fcn2
      }else{logger.error("met2CF function doesn't exists")}
      
      cf0.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,fcn,
                              username,con=con,hostname=host$name,browndog=NULL,write=TRUE)
    }
    
    input_name <- paste0(met,"_CF_Permute")    
    fcn       <-  "permute.nc"
    outfolder  <- file.path(dir,input_name)
    
    print("start CHECK") 
    check = db.query(
      paste0("SELECT i.start_date, i.end_date, d.file_path, d.container_id, d.id  from dbfiles as d join inputs as i on i.id = d.container_id where i.site_id =",register$siteid,
             " and d.container_type = 'Input' and i.format_id=",format.id, " and d.machine_id =",machine$id, " and i.name = '", input_name,
             "' and (i.start_date, i.end_date) OVERLAPS (DATE '", as.POSIXlt(start_date, tz = "GMT"),"',DATE '",as.POSIXlt(end_date, tz = "GMT"),"')"),con)
    print("end CHECK")
    options(digits=10)
    print(check)
    if(length(check)>0){
      cf.id <- list(input.id=check$container_id, dbfile.id=check$id)
    }else{
      # Just a draft of what would happen - doesn't include using the cluster so it would be SLOW. Hasn't been tested.
      cf.id <- convert.input(cf0.id, outfolder2,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,permute.nc,
                             username,con=con,hostname=host$name,browndog=NULL,write=TRUE)
    }
    
  }else if(register$scale=="site"){   
    
    input_name <- paste0(met,"_CF_site_",str_ns)
    outfolder  <- file.path(dir,input_name)
    
    print("start CHECK")   
    check = db.query(
      paste0("SELECT i.start_date, i.end_date, d.file_path, d.container_id, d.id  from dbfiles as d join inputs as i on i.id = d.container_id where i.site_id =",register$siteid,
             " and d.container_type = 'Input' and i.format_id=",33, " and d.machine_id =",machine$id, " and i.name = '", input_name,
             "' and (i.start_date, i.end_date) OVERLAPS (DATE '", as.POSIXlt(start_date, tz = "GMT"),"',DATE '",as.POSIXlt(end_date, tz = "GMT"),"')"),con)
    print("end CHECK")
    options(digits=10)
    print(check)
    if(length(check)>0){
      cf.id <- list(input.id=check$container_id, dbfile.id=check$id)
    }  
    
    fcn1 <- paste0("met2CF.",met)
    fcn2 <- paste0("met2CF.",register$format$mimetype)
    if(exists(fcn1)){
      fcn <- fcn1
    }else if(exists(fcn2)){
      fcn <- fcn2
    }else{logger.error("met2CF function doesn't exists")}
    
    cf.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,fcn,
                           username,con=con,hostname=host$name,browndog=NULL,write=TRUE) 
  }
  
  #--------------------------------------------------------------------------------------------------#
  # Change to Site Level - Standardized Met (i.e. ready for conversion to model specific format)
  
  if(register$scale=="regional"){ #### Site extraction 
    
    print("# Site Extraction")
    
    input.id   <- cf.id[1]
    outfolder  <- file.path(dir,paste0(met,"_CF_site_",str_ns))
    pkg        <- "PEcAn.data.atmosphere"
    fcn        <- "extract.nc"
    formatname <- 'CF Meteorology'
    mimetype   <- 'application/x-netcdf'
    
    ready.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,fcn,
                              username,con=con,hostname=host$name,browndog=NULL,write=TRUE,
                              slat=new.site$lat,slon=new.site$lon,newsite=new.site$id)
    
  }else if(register$scale=="site"){ ##### Site Level Processing
    
    print("# Run Gapfilling") # Does NOT take place on browndog!
    
    input.id   <- cf.id[1]
    outfolder  <- file.path(dir,paste0(met,"_CF_gapfill_site_",str_ns))
    pkg        <- "PEcAn.data.atmosphere"
    fcn        <- "metgapfill"
    formatname <- 'CF Meteorology'
    mimetype   <- 'application/x-netcdf'
    lst        <- site.lst(site,con)
    
    ready.id   <- convert.input(input.id,outfolder,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,fcn,
                                username,con=con,hostname=host$name,browndog=NULL,write=TRUE,lst=lst)
    
  }
  print("Standardized Met Produced")
  
  #--------------------------------------------------------------------------------------------------#
  # Prepare for Model
  
  # Determine output format name and mimetype   
  model_info <- db.query(paste0("SELECT f.name, f.id, f.mime_type from modeltypes as m join modeltypes_formats as mf on m.id
                                = mf.modeltype_id join formats as f on mf.format_id = f.id where m.name = '",model,"' AND mf.tag='met'"),con)
  formatname <- model_info[1]
  mimetype   <- model_info[3]   
  
  print("# Convert to model format")
  
  input.id  <- ready.id[1]
  outfolder <- file.path(dir,paste0(met,"_",model,"_site_",str_ns))
  pkg       <- paste0("PEcAn.",model)
  fcn       <- paste0("met2model.",model)
  lst       <- site.lst(site,con)
    
  model.id  <- convert.input(input.id,outfolder,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,fcn,
                             username,con=con,hostname=host$name,browndog,write=TRUE,lst=lst,lat=new.site$lat,lon=new.site$lon)
  
  print(c("Done model convert",model.id[1]))
  
  model.file <- db.query(paste("SELECT * from dbfiles where id =",model.id[[2]]),con)[["file_name"]]
  
  db.close(con)
  return(file.path(outfolder, model.file))
  
}

#################################################################################################################################

##' @name db.site.lat.lon
##' @title db.site.lat.lon
##' @export
##' @param site.id
##' @param con
##' @author Betsy Cowdery
##' 
db.site.lat.lon <- function(site.id,con){
  site <- db.query(paste("SELECT id, ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id =",site.id),con)
  if(nrow(site)==0){logger.error("Site not found"); return(NULL)} 
  if(!(is.na(site$lat)) && !(is.na(site$lat))){
    return(list(lat = site$lat, lon = site$lon))
  }
}