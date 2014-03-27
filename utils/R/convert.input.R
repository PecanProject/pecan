##' Convert input by applying fcn and insert new record into database
##'
##'
convert.input <- function(input.id,outfolder,pkg,fcn,...){
  
  ## Query inputs, site, dbfiles, machine
  dbparms <- list(driver="PostgreSQL" , user = "bety", dbname = "bety", password = "bety")
  con     <- db.open(dbparms)
  input = db.query(paste("SELECT * from inputs where id =",input.id),con)
  site  = db.query(paste("SELECT * from sites where id =",input$site.id),con)
  dbfile = db.query(paste("SELECT * from dbfiles where container.id =",input.id," and container.type = INPUT"),con)
  machine = db.query(paste("SELECT * from machines where id = ",dbfile$machine,id),con)
  
  ## if the machine is local, run conversion function
  system2("pecan/scripts/Rfcn.R",paste(machine,pkg,fcn,...))
  
  ## if the machine is remote, run conversion remotely
  system2("ssh",paste("pecan/scripts/Rfcn.R",machine,pkg,fcn,...))

  ## insert new record into database
  formatname <- 'CF Meteorology'
  mimetype <- 'application/x-netcdf'
  dbfile.input.insert(filename, siteid, startdate, enddate, mimetype, formatname, con=con) 
  
}