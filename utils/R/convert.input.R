##' Convert input by applying fcn and insert new record into database
##'
##'
##' @name convert.input
##' @title convert.input
##' @export
##' @author Betsy Cowdery, Michael Dietze
convert.input <- function(input.id,outfolder,formatname,mimetype,site.id,start_date,end_date,
                          pkg,fcn,username,con=con,hostname='localhost',browndog, write=TRUE,...){
  print(paste("Convert.Inputs",fcn,input.id,hostname))
  print(paste(outfolder,formatname,mimetype,site.id,start_date,end_date))
  l <- list(...); print(l)
  n <- nchar(outfolder)
  if(substr(outfolder,n,n) != "/"){outfolder = paste0(outfolder,"/")}
  
  outname = tail(unlist(strsplit(outfolder,'/')),n=1)
  
  startdate <- as.POSIXlt(start_date, tz = "GMT")
  enddate   <- as.POSIXlt(end_date, tz = "GMT")
  
  #   print("start CHECK")
  #   check = dbfile.input.check(site.id, startdate, enddate, mimetype, formatname, parentid=input.id, con=con, hostname)
  #   print("end CHECK")
  #   print(check)
  #   if(length(check)>0){
  #     return(check$container_id)
  #   }
  #   
  input = db.query(paste("SELECT * from inputs where id =",input.id),con)
  if(nrow(input)==0){logger.error("input not found",input.id);return(NULL)}
  
  ifelse(hostname == "localhost", machine.host <- fqdn(), machine.host <- hostname)
  machine = db.query(paste0("SELECT * from machines where hostname = '",machine.host,"'"),con)
  # machine = db.query(paste("SELECT * from machines where id = ",dbfile$machine_id),con)
  if(nrow(machine)==0){logger.error("machine not found",hostname);return(NULL)}
  
  # dbfile may return more than one row -> may need to loop over machine ids
  dbfile = db.query(paste("SELECT * from dbfiles where container_id =",input.id," and container_type = 'Input' and machine_id =",machine$id),con)
  if(nrow(dbfile)==0){logger.error("dbfile not found",input.id);return(NULL)}
  if(nrow(dbfile)>1){
    logger.warning("multiple dbfile records, using last",dbfile);
    dbfile = dbfile[nrow(dbfile),]
  }
  
  #--------------------------------------------------------------------------------------------------#
  # Perform Conversion 
  
  conversion = "local.remote" #default
  
  if(!is.null(browndog) & hostname == 'localhost'){ # perform conversions with Brown Dog - only works locally right now
    require(RCurl)
    
    # Determine inputtype by using formatname and mimetype of input file   
    input$format <- db.query(paste0("SELECT f.name, f.mime_type from formats as f where f.id = ",input$format_id),con)   
    if(input$format$name == "Ameriflux"){
      inputtype <- 'ameriflux.zip'
    }else{
      inputtype <- 'pecan.zip'
    }  
    
    # Determine outputtype using formatname and mimetype of output file
    # Add issue to github that extension of formats table to include outputtype 
    if(mimetype ==  'application/x-netcdf'){ # Convert to netcdf - only using localhost
      outputtype <- 'pecan.zip'
    }else{ # Convert to model specific format
      if(formatname == 'ed.met_driver_header_files_format' | formatname == 'ed.met_driver_header files format'){
        outputtype <- 'ed.zip'
      }else if(formatname == 'Sipnet.climna'){
        outputtype <- 'clim'
      }else if(formatname == 'DALEC meteorology'){
        outputtype <- 'dalec.dat'
      }else if(formatname == 'LINKAGES met'){
        outputtype <- 'linkages.dat'
      }
    }
    
    # create curl options
    if (!is.null(browndog$username) && !is.null(browndog$password)) {
      userpwd <- paste(browndog$username, browndog$password, sep=":")
      curloptions <- list(userpwd = userpwd, httpauth = 1L)
    }
    curloptions <- c(curloptions, followlocation=TRUE)
    
    # check if we can do conversion 
    out.html <- getURL(paste0("http://dap-dev.ncsa.illinois.edu:8184/inputs/",inputtype), .opts = curloptions)
    if(outputtype %in% unlist(strsplit(out.html, '\n'))){
      print(paste("Conversion from", inputtype,"to", outputtype, "through Brown Dog"))
      conversion <- "browndog"
    }
  }
  
  if(conversion == "browndog"){
    
    url <- file.path(browndog$url,outputtype) 
    print(url)
    
    # loop over files in localhost and zip to send to Brown Dog 
    files <- list.files(dbfile$file_path, pattern=dbfile$file_name)
    files <- grep(dbfile$file_name,files,value=T)
    zipfile <- paste0(dbfile$file_name,".",inputtype)
    system(paste("cd", dbfile$file_path, "; zip", zipfile,  paste(files, collapse = " ")))
    zipfile <- file.path(dbfile$file_path,zipfile) 
    
    # check for and create output folder
    if(!file.exists(outfolder)){
      dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)
    }
    
    # post zipped file to Brown Dog
    html <- postForm(url,"fileData" = fileUpload(zipfile), .opts = curloptions)
    link <- getHTMLLinks(html)
    print(link)
    file.remove(zipfile)
    
    # download converted file
    outfile <- file.path(outfolder,unlist(strsplit(basename(link),"_"))[2])
    download.url(url = link, file = outfile, timeout = 600, .opts = curloptions, retry404 = TRUE)  
    print(list.files(outfolder))
    
    # unzip downloaded file if necessary
    if(file.exists(outfile)){
      if(tail(unlist(strsplit(outfile,"[.]")),1)=="zip"){
        fname <- unzip(outfile, list=TRUE)$Name
        print(fname)
        unzip(outfile, files=fname, exdir=outfolder, overwrite=TRUE)
        file.remove(outfile)
      }else{fname <- list.files(outfolder)}
    }
    
    # settings$run$inputs$path <- outputfile what if there is more than 1 output file?
    rows <- length(fname)
    result <- data.frame(file=character(rows), host=character(rows),
                         mimetype=character(rows), formatname=character(rows),
                         startdate=character(rows), enddate=character(rows),
                         stringsAsFactors = FALSE)
    for(i in 1:rows){
      old.file <- file.path(dbfile$file_path,files[i])
      new.file <- file.path(outfolder,fname[i])
      
      # create array with results
      result$file[i] <- new.file
      result$host[i] <- fqdn()
      result$startdate[i] <- paste(input$start_date, "00:00:00")
      result$enddate[i] <- paste(input$end_date, "23:59:59")
      result$mimetype[i] <- mimetype
      result$formatname[i] <- formatname    
    }
  }
  
  else if (conversion == "local.remote") { # perform conversion on local or remote host
    args = c(dbfile$file_path,dbfile$file_name,outfolder,start_date,end_date)
    if(!is.null(names(l))){
      cmdFcn  = paste0(paste0(pkg,"::",fcn,"(",paste0("'",args,"'",collapse=",")),",",paste(paste(names(l),"=",unlist(l)), collapse=","),")")
    }else{
      cmdFcn  = paste0(pkg,"::",fcn,"(",paste0("'",args,"'",collapse=","),")") 
    } 
    print(cmdFcn)
    result <- remote.execute.R(script=cmdFcn,hostname,user=NA,verbose=TRUE,R="R")
  }
  
  print("RESULTS: Convert.Input")
  print(result)
  print(names(result))
  
  #--------------------------------------------------------------------------------------------------#
  # Insert into Database
  
  # Use existing site, unless otherwise specified (ex: subsetting case, using newsite)
  if("newsite" %in% names(l) && is.null(l[["newsite"]])==FALSE){
    siteid <- l$newsite
  }else{
    siteid <- site.id
  }
  
  outlist <- unlist(strsplit(outname,"_")) 
  
  ## insert new record into database
  if(write==TRUE){
    
    #    in.prefix=strsplit(basename(result$file[1]),".",fixed=TRUE)[[1]][1]
    in.prefix=find.prefix(result$file)
    newinput <- dbfile.input.insert(in.path=dirname(result$file[1]),
                                    in.prefix=in.prefix,
                                    siteid = siteid, 
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
