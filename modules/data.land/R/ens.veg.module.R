.ens.veg.module <- function(getveg.id, dbparms, 
                            input_veg, pfts,
                            outfolder, 
                            dir, machine, model,
                            start_date, end_date,
                            new_site, 
                            host, overwrite){
  
  
  
  #--------------------------------------------------------------------------------------------------#
  # Write model specific IC files
  bety <- dplyr::src_postgres(dbname   = dbparms$bety$dbname, 
                              host     = dbparms$bety$host, 
                              user     = dbparms$bety$user, 
                              password = dbparms$bety$password)
  
  con <- bety$con
  on.exit(db.close(con))
  
  PEcAn.logger::logger.info("Begin IC sampling")
  
  spp.file <- db.query(paste("SELECT * from dbfiles where container_id =", getveg.id), con)
  
  pkg  <- "PEcAn.data.land"
  fcn  <- "sample_ic"
  
  ensveg.id <- convert.input(input.id = getveg.id,
                             outfolder = outfolder, 
                             formatname = "spp.info", 
                             mimetype = "application/rds",
                             site.id = new_site$id, 
                             start_date = start_date, end_date = end_date, 
                             pkg = pkg, fcn = fcn, 
                             con = con, host = host, browndog = NULL, 
                             write = TRUE, 
                             overwrite = overwrite, 
                             ensemble = as.numeric(input$ensemble),
                             # fcn specific args 
                             in.path = spp.file$file_path, 
                             in.name = spp.file$file_name,
                             model = model,
                             new_site = new_site,
                             pfts = pfts,
                             source = input_veg$source)
  
  
  return(ensveg.id)
  
  
}
