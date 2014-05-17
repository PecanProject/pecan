##' Convert input by applying fcn and insert new record into database
##'
##'

convert.input <- function(input.id,outfolder,pkg,fcn,write,username,...){
  
  if(FALSE){
    ## test during development
    input.id = 288;
    l <- list(newsite = 768, year = TRUE)
    outfolder = "/projectnb/cheas/pecan.data/input/NARR_CF_768";
    pkg = "PEcAn.data.atmosphere"
    fcn = "extract.NARR.R"
    username = "ecowdery"
    write = TRUE
    
  }
  
  l <- list(...)
  
  ## Query inputs, site, dbfiles, machine
  dbparms <- list(driver="PostgreSQL" , user = "bety", dbname = "bety", password = "bety", host = "psql-pecan.bu.edu")
  con     <- db.open(dbparms)
  
  input = db.query(paste("SELECT * from inputs where id =",input.id),con)
  if(nrow(input)==0){print(c("input not found",input.id));return(NULL)}
  
  dbfile = db.query(paste("SELECT * from dbfiles where container_id =",input.id," and container_type = 'Input'"),con)
  if(nrow(dbfile)==0){print(c("dbfile not found",input.id));return(NULL)}
  
  machine = db.query(paste("SELECT * from machines where id = ",dbfile$machine_id),con)
  if(nrow(machine)==0){print(c("machine not found",dbfile$machine_id));return(NULL)}
  
  host = system("hostname",intern=TRUE)
  
  if(dbfile$file_name == ""){dbfile$file_name = "''"} 
  
  args = c(pkg,fcn,dbfile$file_path,dbfile$file_name,outfolder)
  
  # Use existing site, unless otherwise specified (ex: subsetting case)
  if("newsite" %in% names(l) && is.null(l[["newsite"]])==FALSE){
    site = db.query(paste("SELECT * from sites where id =",l$newsite),con)
    args = c(args, site$lat, site$lon)
  } else {
    site  = db.query(paste("SELECT * from sites where id =",input$site_id),con)  
  }      
  if(nrow(site)==0){print(c("site not found",input$site_id));return(NULL)} 
  
  if("year" %in% names(l) && l$year == TRUE) {
    if(is.na(input$start_date)==TRUE){input$start_date = 1979}
    if(is.na(input$end_date)==TRUE){input$end_date = 2013}
    args = c(args, input$start_date,  input$end_date)
  }
  
  cmdArgs = paste(args,collapse=" ")
  #  Rfcn = system.file("scripts/Rfcn.R", package = "PEcAn.all")
  Rfcn = "pecan/scripts/Rfcn.R"
  
  
  if(machine$hostname %in% c("localhost",host)){
    ## if the machine is local, run conversion function
    system(paste(Rfcn,cmdArgs))
  } else {
    ## if the machine is remote, run conversion remotely
    system2("ssh",paste0(username,"@",paste(machine$hostname,Rfcn,cmdArgs)))
  }
  
  ### NOTE: We will eventually insert Brown Dog REST API calls here
  
  ## Add a check to insert only if the conversion was successful
  
  ## insert new record into database
  if(write==TRUE){
    formatname <- 'CF Meteorology'
    mimetype <- 'application/x-netcdf'
    dbfile.input.insert(outfolder, site$id, input$start_date, input$end_date, 
                        mimetype, formatname,input$id,con=con,machine$hostname) 
  }
}
