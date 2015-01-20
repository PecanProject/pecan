##' Convert input by applying fcn and insert new record into database
##'
##'

convert.input <- function(input.id,outfolder,formatname,mimetype,site.id,start_date,end_date,pkg,fcn,username,con=con,...){
  
  l <- list(...); print(l)
  n <- nchar(outfolder)
  if(substr(outfolder,n,n) != "/"){outfolder = paste0(outfolder,"/")}
  
  outname = tail(unlist(strsplit(outfolder,'/')),n=1)
  
  startdate <- as.POSIXlt(start_date, tz = "GMT")
  enddate   <- as.POSIXlt(end_date, tz = "GMT")
  
  dbfile.input.check(site.id, startdate, enddate, mimetype, formatname, con=con, hostname=fqdn())
  
  input = db.query(paste("SELECT * from inputs where id =",input.id),con)
  if(nrow(input)==0){print(c("input not found",input.id));db.close(con);return(NULL)}
  
  machine = db.query(paste0("SELECT * from machines where hostname = '",l$raw.host,"'"),con)
  # machine = db.query(paste("SELECT * from machines where id = ",dbfile$machine_id),con)
  if(nrow(machine)==0){print(c("machine not found",dbfile$machine_id));db.close(con);return(NULL)}
  
  # dbfile may return more than one row -> may need to loop over machine ids
  dbfile = db.query(paste("SELECT * from dbfiles where container_id =",input.id," and container_type = 'Input' and machine_id =",machine$id),con)
  if(nrow(dbfile)==0){print(c("dbfile not found",input.id));db.close(con);return(NULL)}
  
  
  
  host = system("hostname",intern=TRUE)
  
  args = c(pkg,fcn,dbfile$file_path,dbfile$file_name,outfolder)
  
  # Use existing site, unless otherwise specified (ex: subsetting case)
  if("newsite" %in% names(l) && is.null(l[["newsite"]])==FALSE){
    site <- db.query(paste("SELECT id, ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id =",l$newsite),con)
    if(nrow(site)==0){logger.error("Site not found"); db.close(con);return(NULL)} 
    if(!(is.na(site$lat)) && !(is.na(site$lat))){
      args = c(args, site$lat, site$lon)
    }else{logger.error("No lat and lon for extraction site"); db.close(con);return(NULL)}
  }else{
    site <- db.query(paste("SELECT id, ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id =",input$site_id),con)
    if(nrow(site)==0){logger.error("Site not found");db.close(con);return(NULL)} 
  }      
  
  outlist <- unlist(strsplit(outname,"_"))
  if("ED2" %in% outlist){args <- c(args, l$lst, startdate, enddate)}
  if("SIPNET" %in% outlist){args <- c(args, startdate, enddate)}
  if("DALEC" %in% outlist){args <- c(args, startdate, enddate)}
  
  print(args)
  cmdArgs = paste(args,collapse=" ")
  #  Rfcn = system.file("scripts/Rfcn.R", package = "PEcAn.all")
  Rfcn = "pecan/scripts/Rfcn.R"
  
  if(machine$hostname %in% c("localhost",host)){
    ## if the machine is local, run conversion function
    system(paste0("~/",Rfcn," ",cmdArgs))
  } else {
    ## if the machine is remote, run conversion remotely
    usr = ifelse(is.null(username)| username=="","",paste0(username,"@"))
    system2("ssh",paste0(usr,paste(machine$hostname,Rfcn,cmdArgs)))
  }
  
  
  ### NOTE: We will eventually insert Brown Dog REST API calls here
  
  ## insert new record into database
  if(l$write==TRUE){
    ### Hack
    if("ED2" %in% outlist){
      filename <- paste0(outfolder," ")
    }else if("SIPNET" %in% outlist){
      filename <- paste0(outfolder,"sipnet.clim")
    }else{
      filename <- paste0(outfolder,dbfile$file_name)
    }
    
    newinput <- dbfile.input.insert(filename, 
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
    db.close(con)
    return(NULL)
  }
  db.close(con)
}