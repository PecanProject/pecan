##' Convert input by applying fcn and insert new record into database
##'
##'
convert.input <- function(input.id,outfolder,pkg,fcn,...){
  
  if(FALSE){
    ## test during development
    input.id = 286;
    #input.id = 292;
    outfolder = "/tmp/";
    pkg = "PEcAn.data.atmosphere"
    fcn = "met2CF.Ameriflux"
  }
  
  ## Query inputs, site, dbfiles, machine
  dbparms <- list(driver="PostgreSQL" , user = "bety", dbname = "bety", password = "bety")
  con     <- db.open(dbparms)
  
  input = db.query(paste("SELECT * from inputs where id =",input.id),con)
  if(nrow(input)==0){print(c("input not found",input.id));return(NULL)}
  
  site  = db.query(paste("SELECT * from sites where id =",input$site_id),con)
  if(nrow(site)==0){print(c("site not found",input$site_id));return(NULL)}
  
  dbfile = db.query(paste("SELECT * from dbfiles where container_id =",input.id," and container_type = 'Input'"),con)
  if(nrow(dbfile)==0){print(c("dbfile not found",input.id));return(NULL)}
  
  machine = db.query(paste("SELECT * from machines where id = ",dbfile$machine_id),con)
  if(nrow(machine)==0){print(c("machine not found",dbfile$machine_id));return(NULL)}
  
  host = system("hostname",intern=TRUE)
  args = c(pkg,fcn,dbfile$file_path,dbfile$file_name,outfolder)#,...)  
  cmdArgs = paste(args,collapse=" ")
#  Rfcn = system.file("scripts/Rfcn.R", package = "PEcAn.all")
  Rfcn = "pecan/scripts/Rfcn.R"


  if(machine$hostname %in% c("localhost",host)){
    ## if the machine is local, run conversion function
    system(paste(Rfcn,cmdArgs))
  } else {
    ## if the machine is remote, run conversion remotely
    system2("ssh",paste0("jam2767@",paste(machine$hostname,Rfcn,cmdArgs)))
  }

### NOTE: We will eventually insert Brown Dog REST API calls here

  ## Add a check to insert only if the conversion was successful
  
  ## insert new record into database
  #formatname <- 'CF Meteorology'
  #mimetype <- 'application/x-netcdf'
  #dbfile.input.insert(outfolder, site$id, input$start_date, input$end_date, 
  #                   mimetype, formatname,input$id,con=con,machine$hostname) 
  
}