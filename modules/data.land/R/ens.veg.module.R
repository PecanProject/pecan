.ens.veg.module <- function(getveg.id, dbparms, 
                            input_veg, 
                            outfolder, 
                            machine, 
                            start_date, end_date,
                            n.ensemble,
                            new_site, 
                            host, machine_host){
  
  
  
  #--------------------------------------------------------------------------------------------------#
  # Write model specific IC files
  bety <- dplyr::src_postgres(dbname   = dbparms$bety$dbname, 
                              host     = dbparms$bety$host, 
                              user     = dbparms$bety$user, 
                              password = dbparms$bety$password)
  
  con <- bety$con
  on.exit(db.close(con))
  
  PEcAn.logger::logger.info("Begin IC sampling, ensemble member: ", n.ensemble)
  
  spp.file <- db.query(paste("SELECT * from dbfiles where container_id =", getveg.id), con)
  
  pkg  <- "PEcAn.data.land"
  fcn  <- "sample_ic"
  
  ensveg.id <- convert.input(input.id = getveg.id,
                             outfolder = paste0(outfolder, "/", input_veg$source, "_ens", n.ensemble, ".", lubridate::year(start_date)), 
                             formatname = "spp.info", 
                             mimetype = "application/rds",
                             site.id = new_site$id, 
                             start_date = start_date, end_date = end_date, 
                             pkg = pkg, fcn = fcn, 
                             con = con, host = host, browndog = NULL, 
                             write = TRUE, 
                             overwrite = FALSE, 
                             pattern = paste0(input_veg$source, "_ens", n.ensemble),
                             forecast = TRUE,
                             ensemble = 1,
                             # fcn specific args 
                             in.path = spp.file$file_path, 
                             in.name = spp.file$file_name,
                             n.ensemble = n.ensemble,
                             machine_host = machine_host,
                             source = input_veg$source)
  
  
  return(ensveg.id)
  
  
}
