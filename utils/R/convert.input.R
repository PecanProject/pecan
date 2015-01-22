##' Convert input by applying fcn and insert new record into database
##'
##'
##' @name convert.input
##' @title convert.input
##' @export
##' @author Betsy Cowdery, Michael Dietze
convert.input <- function(input.id,outfolder,formatname,mimetype,site.id,start_date,end_date,
                          pkg,fcn,username,con=con,host='localhost',write=TRUE,...){
  
  l <- list(...); print(l)
  n <- nchar(outfolder)
  if(substr(outfolder,n,n) != "/"){outfolder = paste0(outfolder,"/")}
  
  outname = tail(unlist(strsplit(outfolder,'/')),n=1)
  
  startdate <- as.POSIXlt(start_date, tz = "GMT")
  enddate   <- as.POSIXlt(end_date, tz = "GMT")
  
  check = dbfile.input.check(site.id, startdate, enddate, mimetype, formatname, con=con, hostname=host)
  print("CHECK")
  print(check)
  if(length(check)>0){
    return(check$container_id)
  }
  
  input = db.query(paste("SELECT * from inputs where id =",input.id),con)
  if(nrow(input)==0){logger.error("input not found",input.id);return(NULL)}
  
  machine = db.query(paste0("SELECT * from machines where hostname = '",host,"'"),con)
  # machine = db.query(paste("SELECT * from machines where id = ",dbfile$machine_id),con)
  if(nrow(machine)==0){logger.error("machine not found",host);return(NULL)}
  
  # dbfile may return more than one row -> may need to loop over machine ids
  dbfile = db.query(paste("SELECT * from dbfiles where container_id =",input.id," and container_type = 'Input' and machine_id =",machine$id),con)
  if(nrow(dbfile)==0){logger.error("dbfile not found",input.id);return(NULL)}
  if(nrow(dbfile)>1){
    logger.warning("multiple dbfile records, using last",dbfile);
    dbfile = dbfile[nrow(dbfile),]
  }
  
    
#  args = c(pkg,fcn,dbfile$file_path,dbfile$file_name,outfolder,
#           paste0("'",start_date,"'"), paste0("'",end_date,"'"))
  args = c(dbfile$file_path,dbfile$file_name,outfolder,
           start_date,end_date)
  
  # Use existing site, unless otherwise specified (ex: subsetting case)
  if("newsite" %in% names(l) && is.null(l[["newsite"]])==FALSE){
    site <- db.query(paste("SELECT id, ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id =",l$newsite),con)
    if(nrow(site)==0){logger.error("Site not found"); return(NULL)} 
    if(!(is.na(site$lat)) && !(is.na(site$lat))){
      args = c(args, site$lat, site$lon)
    }else{logger.error("No lat and lon for extraction site"); return(NULL)}
  }else{
    site <- db.query(paste("SELECT id, sitename as name, ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id =",input$site_id),con)
    if(nrow(site)==0){logger.error("Site not found");return(NULL)} 
  }      
  
  outlist <- unlist(strsplit(outname,"_"))
  if("ED2" %in% outlist){args <- c(args, l$lst)}
  
  print(args)
  cmdFcn  = paste0(pkg,"::",fcn,"(",paste0("'",args,"'",collapse=","),")")
  result <- remote.execute.R(cmdFcn,host,verbose=FALSE)


  # cmdArgs = paste(args,collapse=" ")
  #  Rfcn = system.file("scripts/Rfcn.R", package = "PEcAn.all")
  # Rfcn = "pecan/scripts/Rfcn.R"
#  if(machine$hostname %in% c("localhost",host)){
#    ## if the machine is local, run conversion function
#    result <- system(paste0("~/",Rfcn," ",cmdArgs))
#  } else {
#    ## if the machine is remote, run conversion remotely
#    usr = ifelse(is.null(username)| username=="","",paste0(username,"@"))
#    result <- system2("ssh",paste0(usr,paste(machine$hostname,Rfcn,cmdArgs)))
#  }
  
  
  ### NOTE: We will eventually insert Brown Dog REST API calls here
  
  ## insert new record into database
  if(write==TRUE){
    ### Hack
#    if("ED2" %in% outlist){
#      in.path <- outfolder
#      in.prefix <- "ED_MET_DRIVER_HEADER"
#    }else if("SIPNET" %in% outlist){
#      in.path <- outfolder
#      in.prefix <- "sipnet.clim"
#    }else{
#      in.path <- outfolder
#      in.prefix <- dbfile$file_name
#    }
    
#    in.prefix=strsplit(basename(result$file[1]),".",fixed=TRUE)[[1]][1]
    in.prefix=find.prefix(basename(result$file))
    newinput <- dbfile.input.insert(in.path=dirname(result$file[1]),
                                    in.prefix=in.prefix,
                                    siteid = site$id, 
                                    startdate = paste(input$start_date), 
                                    enddate = paste(input$end_date), 
                                    mimetype, 
                                    formatname,
                                    parentid = input$id,
                                    con = con,
                                    hostname = machine$hostname) 
    return(newinput$input.id)
  }else{
    logger.warn('Input was not added to the database')
    return(NULL)
  }
}