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
  
  con      <- db.open(dbparms)
  username <- ""  
  
  # What if input_met$id is specified? 
  # What will that id be? The raw id? Or the "closest" to what the user wants?
  # Then unnecessary steps could be skipped?

  # Determine if met data is regional or site - this needs to be automated!
  met <- input_met$source
  
  # Met workflow doens't work if you don't give a source
  # Check if input id
  # Check what stage input id (raw, CF, ...)
  
  # Either regional or site run
  regional <- met == "NARR" # Ultimately should  
  
  new.site = as.numeric(site$id)
  str_ns    <- paste0(new.site %/% 1000000000, "-", new.site %% 1000000000)  
  
  #--------------------------------------------------------------------------------------------------#
  # Download raw met from the internet 
  
  outfolder  <- file.path(dir,met)
  pkg        <- "PEcAn.data.atmosphere"
  fcn        <- paste0("download.",met)
  
  
  if(met == "NARR"){
    
    # Raw NARR was originally downloaded on geo and pecan2, but was removed to save space.
    # If this section is run, NARR will be re-downloaded, which isn't necessary.
    # Should we make running this code optional?
    download.NARR <- FALSE
    if(download.NARR){
          site.id <- 1135
          
          args <- list(outfolder, start_date, end_date)
          cmdFcn  = paste0(pkg,"::",fcn,"(",paste0("'",args,"'",collapse=","),")")
          remote.execute.R(cmdFcn,host$name,user=NA, verbose=TRUE)
          
          mimetype = 'application/x-netcdf'
          formatname = "NARR"
          check <- dbfile.input.check(site.id, start_date, end_date, mimetype, formatname, con=con, hostname=fqdn())
          
          if(length(check)>0){
            raw.id = check$container_id[1]
          }else{
            raw.id <- dbfile.input.insert(in.path = outfolder, 
                                          in.prefix = "NARR", 
                                          siteid = 1135, 
                                          startdate = start_date, 
                                          enddate = end_date, 
                                          mimetype =  mimetype, 
                                          formatname = formatname,
                                          parentid = NA,
                                          con = con,
                                          hostname = host$name)$input.id
            raw.id <- newinput$input.id #1000000127
          }
    }else{
      raw.id <- 1000000127  
    }   
  } 
  
  else if(met == "Ameriflux"){
    
    # Move site.code to inside download.ameriflux, set site$name as argument in CmdFcn
    # in.prefix will be defined from new.files
    # else if -> else 
    
    outfolder = paste0(outfolder,"_site_",str_ns)
    args <- list(site$name, outfolder, start_date, end_date)
    
    cmdFcn  = paste0(pkg,"::",fcn,"(",paste0("'",args,"'",collapse=","),")")
    new.files <- remote.execute.R(script=cmdFcn,host=host$name,user=NA,verbose=TRUE,R="R")
    
    check = dbfile.input.check(site$id, start_date, end_date, 
                               mimetype=new.files$mimetype[1], formatname=new.files$formatname[1], 
                               con=con, hostname=new.files$host[1])
    if(length(check)>0){
      raw.id = check$container_id[1]
    }else{
      ## insert database record
      raw.id <- dbfile.input.insert(in.path=dirname(new.files$file[1]),
                                    in.prefix=head(unlist(strsplit(basename(new.files$file[1]), "[.]")),1), 
                                    siteid = site$id, 
                                    startdate = start_date, 
                                    enddate = end_date, 
                                    mimetype=new.files$mimetype[1], 
                                    formatname=new.files$formatname[1],
                                    parentid=NA,
                                    con = con,
                                    hostname = host$name)$input.id
    }    
  } 
  
  else {  ## site level, not Ameriflux
    print("NOT AMERIFLUX")
    site.id <- site$id
    args <- list(site.id, outfolder, start_date, end_date, overwrite=FALSE, verbose=FALSE) #, pkg,raw.host = host,dbparms,con=con)
    raw.id <- do.call(fcn,args)
    
  }
  
  #--------------------------------------------------------------------------------------------------#
  # Change to  CF Standards
  
  print("### Change to CF Standards")
  
  input.id  <-  raw.id
  outfolder  <- file.path(dir,paste0(met,"_CF_site_",str_ns))
  pkg       <- "PEcAn.data.atmosphere"
  
  # From input.id check formatname and mimetype
  #   model_info <- db.query(paste0("SELECT f.name, f.id, f.mime_type from modeltypes as m join modeltypes_formats as mf on m.id
  #                                 = mf.modeltype_id join formats as f on mf.format_id = f.id where m.name = '",model,"' AND mf.tag='met'"),con)
  # if exists(PEcAn.data.atmosphere::met2CF.met){use it}
  # else if( if exists(PEcAn.data.atmosphere::met2CF.mimetype)){use it} # for example met2CF.csv -> needs additional argument of format
  # else {error}
  
  # all met2CF funtions need to take additional arguments
  
  
  fcn       <-  paste0("met2CF.",met)
  formatname <- 'CF Meteorology'
  mimetype <- 'application/x-netcdf'
  
  if(met == "NARR"){   
    # Similar problem as NARR download 
    # Conversion to CF standard takes one step but then need to rechunk and permute before ready 
    # for extraction. This means 3x the NARR dataset and lots of time. We've done the conversion,
    # deleted all but the final permuted data to save space. Shouldn't run again. 
    cf.rechunk.permute.NARR <- FALSE
    
    if(cf.rechunk.permute.NARR){
      # Just a draft of what would happen - doesn't include using the cluster so it would be SLOW. Hasn't been tested.
      cf1.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,fcn,
                              username,con=con,hostname=host$name,browndog=NULL,write=TRUE)
      cf.id <- convert.input(cf1.id, outfolder2,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,permute.nc,
                             username,con=con,hostname=host$name,browndog=NULL,write=TRUE)
    }
    cf.id <- 1000000023 #ID of permuted CF files in bety - they already exist on pecan2 and geo
  }else{    
    cf.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,fcn,
                           username,con=con,hostname=host$name,browndog=NULL,write=TRUE) 
  }
    
  #--------------------------------------------------------------------------------------------------#
  # Change to Site Level - Standardized Met (i.e. ready for conversion to model specific format)
  
  if(regional){ #### Site extraction (only for NARR right now)
    
    print("# Site Extraction")
    
    input.id   <- cf.id
    outfolder  <- file.path(dir,paste0(met,"_CF_site_",str_ns))
    pkg        <- "PEcAn.data.atmosphere"
    fcn        <- "extract.nc"
    formatname <- 'CF Meteorology'
    mimetype   <- 'application/x-netcdf'
    
    new.lat <- db.site.lat.lon(new.site,con=con)$lat
    new.lon <- db.site.lat.lon(new.site,con=con)$lon
    
    ready.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,fcn,
                              username,con=con,hostname=host$name,browndog=NULL,write=TRUE,
                              slat=new.lat,slon=new.lon,newsite=new.site)
    
  }else{ ##### Site Level Processing
    
    print("# Run Gapfilling") # Does NOT take place on browndog!

    input.id   <- cf.id
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
  
  input.id  <- ready.id
  outfolder <- file.path(dir,paste0(met,"_",model,"_site_",str_ns))
  pkg       <- paste0("PEcAn.",model)
  fcn       <- paste0("met2model.",model)
  lst       <- site.lst(site,con)
  
  model.id  <- convert.input(input.id,outfolder,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,fcn,
                            username,con=con,hostname=host$name,browndog,write=TRUE,lst=lst)
  
  print(c("Done model convert",model.id,outfolder))
  
  db.close(con)
  return(outfolder)
  
}


##' @name find.prefix
##' @title find.prefix
##' @export
##' @param files
##' @author Betsy Cowdery
find.prefix <- function(files){
  
  if(length(files)==1){
    tail <- tail(unlist(strsplit(files, "/")),1)
    prefix <- head(unlist(strsplit(tail, "[.]")),1)
    return(prefix)
  }
  
  files.split <- try(strsplit(unlist(files), "[.]"), silent = TRUE)
  
  if(!inherits(files.split, 'try-error')){
    
    files.split <- lapply(files.split, `length<-`,max(unlist(lapply(files.split, length))))
    files.df <- as.data.frame(do.call(rbind, files.split))
    files.uniq <- sapply(files.df, function(x)length(unique(x))) 
    
    prefix <- ""
    ifelse(files.uniq[1] == 1,prefix <- as.character(files.df[1,1]),return(prefix))
    
    for(i in 2:length(files.uniq)){
      if(files.uniq[i]==1){
        prefix <- paste(prefix,as.character(files.df[1,i]),sep = ".")
      }else{
        return(prefix)
      }
    }
  }else{   
    bases <- lapply(as.character(files),basename)
    if(length(unique(bases))==1){
      prefix <- bases[1]
    }else{
      prefix <- ""
    }
  }
  return(prefix)
}


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