##' Sampling/ensemble module
##'
##' @param getveg.id list, input.id and dbfile.id of the IC file in intermediate pecan standard
##' @param dbparms list, settings$database info reqired for opening a connection to DB
##' @param input_veg list, this is a sublist of settings$run$inputs that has info about source, id, metadata of the requested IC file
##' @param outfolder path to where the processed files will be written
##' @param machine data frame, DB info regarding localhost machine id/hostname etc.
##' @param start_date date in "YYYY-MM-DD" format, in case of source==FIA it's the settings$run$start.date, otherwise start_date of the IC file in DB
##' @param end_date date in "YYYY-MM-DD" format, in case of source==FIA it's the settings$run$end.date, otherwise end_date of the IC file in DB
##' @param n.ensemble integer, ensemble member number
##' @param new_site data frame, id/lat/lon/name info about the site
##' @param host list, host info as in settings$host, host$name forced to be "localhost" upstream
##' 
##' @export
##'
##' @author Istem Fer
ens_veg_module <- function(getveg.id, dbparms,
                            input_veg,
                            outfolder,
                            machine,
                            start_date, end_date,
                            n.ensemble,
                            new_site,
                            host){

  machine_host <- machine$hostname

  #--------------------------------------------------------------------------------------------------#
  # Write model specific IC files
  con <- PEcAn.DB::db.open(dbparms$bety)
  on.exit(PEcAn.DB::db.close(con), add = TRUE)
  
  PEcAn.logger::logger.info("Begin IC sampling, ensemble member: ", n.ensemble)
  
  spp.file <- PEcAn.DB::db.query(paste("SELECT * from dbfiles where container_id =", getveg.id), con)
  
  pkg  <- "PEcAn.data.land"
  fcn  <- "sample_ic"
  
  ensveg.id <- PEcAn.DB::convert_input(input.id = getveg.id$input.id,
                                         outfolder = paste0(outfolder, "/", input_veg$source, "_ens", n.ensemble, ".", lubridate::year(start_date)), 
                                         formatname = "spp.info", 
                                         mimetype = "application/rds",
                                         site.id = new_site$id, 
                                         start_date = start_date, end_date = end_date, 
                                         pkg = pkg, fcn = fcn, 
                                         con = con, host = host,
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
