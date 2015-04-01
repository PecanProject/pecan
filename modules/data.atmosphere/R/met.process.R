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
##' @author Elizabeth Cowdery, Michael Dietze
met.process <- function(site, input_met, start_date, end_date, model, host, dbparms, dir, browndog=NULL){
  
  require(RPostgreSQL)
  
  driver   <- "PostgreSQL"
  con      <- db.open(dbparms)
  
  username <- ""  
  
  #' Determine where download and conversion will take place -  either on Brown Dog or specified directory
  #' Would like to ultimately make "convert" even more specific:
  #' convert = "bd_raw"
  #'         = "dir_raw"
  #'         = "bd_cf"
  #          = "dir_cf"
  #'         = "bd_model"
  #'         = "dir_model"
  met <- input_met$source
  if(!exists("input_met$id") || input_met$id==""){
    if (!is.null(browndog$url) && (browndog$url != "")){
      convert = "browndog"
    }else{
      convert = "dir"
    }
  }else{
    convert=""
    raw.id=as.numeric(input_met$id)
  }
  
  # Determine if met data is regional or site - this needs to be automated!
  regional <- met == "NARR" # Either regional or site run
  new.site = as.numeric(site$id)
  str_ns    <- paste0(new.site %/% 1000000000, "-", new.site %% 1000000000)
  
  #   #--------------------------------------------------------------------------------------------------#
  #   # BROWN DOG 
  #   if(convert == "browndog"){
  #     require(lubridate)
  #     require(data.table)
  #     require(RCurl)
  #     require(XML)
  #     
  #     # Determine Brown Dog Output Type - ultimately will be added to the database
  #     if(model_info[[2]] == 24){ #SIPNET
  #       outputtype <- 'clim'   
  #     }
  #     #   else if(model == "BIOCRO"){   
  #     #   }else if(model == "DALEC"){
  #     #   }else if(model == "LINKAGES"){
  #     #   }
  #       
  #     url <- file.path(browndog$url,outputtype) 
  #     print(url)
  #     
  #     if(met=="Ameriflux"){site.dl = sub(".* \\((.*)\\)", "\\1", site$name)}
  #     
  #     xmldata = newXMLNode("input")
  #     newXMLNode("type", tolower(met) , parent = xmldata) # For Ameriflux, type is ameriflux - do caps matter? Don't know how different met will be treated
  #     newXMLNode("site", site.dl, parent = xmldata)
  #     newXMLNode("start_date", paste(start_date), parent = xmldata)
  #     newXMLNode("end_date", paste(end_date), parent = xmldata)
  #     xmldata <- saveXML(xmldata)
  #     
  #     # post to browndog
  #     html <- postForm(url,"fileData" = fileUpload("pecan.xml", xmldata, "text/xml"))
  #     link <- getHTMLLinks(html)
  #     print(link)
  #     
  #     outfolder <- file.path(dir,paste0(met,"_",model,"_site_",str_ns)) # This would change if specifying convert = bd_step  
  #     if(!file.exists(outfolder)){
  #       dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)
  #     }
  #     outputfile <- file.path(outfolder, paste(site.dl, strptime(start_date, "%Y-%m-%d"), strptime(end_date, "%Y-%m-%d"), outputtype, sep="."))
  #     dl_file(link, outputfile, 0) # My download function - don't know if Rob's while loop is better?
  #     
  #     settings$run$inputs$path <- outputfile
  #     
  #     start_year <- year(as.POSIXlt(start_date, tz = "GMT"))
  #     end_year <- year(as.POSIXlt(end_date, tz = "GMT"))
  #     rows <- end_year - start_year + 1
  #     results <- data.frame(file=outputfile, 
  #                           host=host$name,
  #                           mimetype, 
  #                           formatname,
  #                           startdate=start_date, 
  #                           enddate=end_date,
  #                           stringsAsFactors = FALSE)    
  # 
  #     
  #   } # End conversion in Brown Dog
  
  #--------------------------------------------------------------------------------------------------#
  # Download raw met from the internet 
  
  outfolder  <- file.path(dir,met)
  pkg        <- "PEcAn.data.atmosphere"
  fcn        <- paste0("download.",met)
  
  
  if(met == "NARR"){
    
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
    if(met == "Ameriflux"){
      
      site.code = sub(".* \\((.*)\\)", "\\1", site$name)
      
      outfolder = paste0(outfolder,"_site_",str_ns)
      args <- list(site.code, outfolder, start_date, end_date)
      
      cmdFcn  = paste0(pkg,"::",fcn,"(",paste0("'",args,"'",collapse=","),")")
      new.files <- remote.execute.R(cmdFcn,host$name,user=NA,verbose=TRUE)
      
      
      host$name = new.files$host[1]
      
      check = dbfile.input.check(site$id, start_date, end_date, 
                                 mimetype=new.files$mimetype[1], formatname=new.files$formatname[1], 
                                 con=con, hostname=new.files$host[1])
      if(length(check)>0){
        raw.id = check$container_id[1]
      }else{
        ## insert database record
        raw.id <- dbfile.input.insert(in.path=dirname(new.files$file[1]),
                                      in.prefix=site.code, 
                                      siteid = site$id, 
                                      startdate = start_date, 
                                      enddate = end_date, 
                                      mimetype=new.files$mimetype[1], 
                                      formatname=new.files$formatname[1],
                                      parentid=NA,
                                      con = con,
                                      hostname = host$name)$input.id
      }    
    } else {  ## site level, not Ameriflux
      print("NOT AMERIFLUX")
      site.id <- site$id
      args <- list(site.id, outfolder, start_date, end_date, overwrite=FALSE, verbose=FALSE) #, pkg,raw.host = host,dbparms,con=con)
      raw.id <- do.call(fcn,args)
      
    }
    
  } # End conversion in directory
  
  #--------------------------------------------------------------------------------------------------#
  print("### Change to CF Standards")
  
  input.id  <-  raw.id
  outfolder  <- file.path(dir,paste0(met,"_CF_site_",str_ns))
  pkg       <- "PEcAn.data.atmosphere"
  fcn       <-  paste0("met2CF.",met)
  formatname <- 'CF Meteorology'
  mimetype <- 'application/x-netcdf'
  
  if(met == "NARR"){
    cf.id <- 1000000023 #ID of permuted CF files
  }else{    
    cf.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,fcn,
                           username,con=con,hostname=host$name,browndog,write=TRUE) 
  }
  
  
  #--------------------------------------------------------------------------------------------------#
  # Extraction 
  
  if(regional){ #ie NARR right now    
    
    input.id   <- cf.id
    outfolder  <- file.path(dir,paste0(met,"_CF_site_",str_ns))
    pkg        <- "PEcAn.data.atmosphere"
    fcn        <- "extract.nc"
    formatname <- 'CF Meteorology'
    mimetype   <- 'application/x-netcdf'
    
    new.lat <- db.site.lat.lon(new.site,con=con)$lat
    new.lon <- db.site.lat.lon(new.site,con=con)$lon
    
    ready.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,fcn,
                              username,con=con,hostname=host$name,browndog,write=TRUE,
                              slat=new.lat,slon=new.lon,newsite=new.site)
    
  }else{ 
    #### SITE-LEVEL PROCESSING ##########################
    
    print("# run gapfilling") 
    #    ready.id <- convert.input()
    #    ready.id <- metgapfill(outfolder, site.code, file.path(settings$run$dbfiles, "gapfill"), start_date=start_date, end_date=end_date)
    
    input.id   <- cf.id
    outfolder  <- file.path(dir,paste0(met,"_CF_gapfill_site_",str_ns))
    pkg        <- "PEcAn.data.atmosphere"
    fcn        <- "metgapfill"
    formatname <- 'CF Meteorology'
    mimetype   <- 'application/x-netcdf'
    lst        <- site.lst(site,con)
    
    ready.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,fcn,
                              username,con=con,hostname=host$name,browndog=NULL,write=TRUE,lst=lst)
    
  }
  print("Standardized Met Produced")
  
  #--------------------------------------------------------------------------------------------------#
  # Prepare for Model
  
  lst <- site.lst(site,con)
  
  print("# Convert to model format")
  input.id  <- ready.id
  outfolder <- file.path(dir,paste0(met,"_",model,"_site_",str_ns))
  pkg       <- paste0("PEcAn.",model)
  fcn       <- paste0("met2model.",model)
  
  model_info <- db.query(paste0("SELECT f.name, f.id, f.mime_type from modeltypes as m join modeltypes_formats as mf on m.id = mf.modeltype_id join formats as f on mf.format_id = f.id where m.name = '",model,"'"),con)
  formatname <- model_info[1]
  mimetype   <- model_info[3] 
  
  model.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id=site$id,start_date,end_date,pkg,fcn,
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

## Betsy's brute force fix for downloading files from Brown Dog

##' @name dl_file
##' @title dl_file
##' @export
##' @param link - path to file to be downloaded
##' @param outfolder - destination of downloaded file(s)
##' @param i - number of times to try download 
##' @param sleep - the number of seconds to pause between download attempts
##' @author Betsy Cowdery
dl_file <- local({ 
  n <- 0
  function(link, outfolder, i=40, sleep=3){
    n = n+1
    r <- try(download.file(link, outfolder, quiet = TRUE), silent = TRUE)
    if(inherits(r, 'try-error') & n <= 40){
      cat("*")
      Sys.sleep(sleep)
      dl_file(link, outfolder, i)
    }
    if(inherits(r, 'try-error') & n > 40){
      cat(sprintf("Download failed after %.0f seconds", (n-1)*sleep))
      return(NULL)
    }
    if(inherits(r, 'integer')){
      cat(sprintf("Download succeeded after %.0f seconds", (n-1)*sleep))
    }
  }
})

#' Is it better to do a while loop?
#'        while(!file.exists(outputfile)) {
#           tryCatch({
#             download.file(url, outputfile)
#           }, error = function(e) {
#             file.remove(outputfile)
#           })
#         }