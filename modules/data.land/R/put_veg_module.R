##' Match species to PFTs + veg2model module
##'
##' @param getveg.id list, input.id and dbfile.id of the IC file in intermediate pecan standard
##' @param dbparms list, settings$database info reqired for opening a connection to DB
##' @param input_veg list, this is a sublist of settings$run$inputs that has info about source, id, metadata of the requested IC file
##' @param pfts list, same as settings$pfts
##' @param outfolder path to where the processed files will be written
##' @param n.ensemble integer, ensemble member number
##' @param dir dir path to dbfiles on local machine
##' @param machine data frame, DB info regarding localhost machine id/hostname etc.
##' @param model model name, e.g. "ED2"
##' @param start_date date in "YYYY-MM-DD" format, in case of source==FIA it's the settings$run$start.date, otherwise start_date of the IC file in DB
##' @param end_date date in "YYYY-MM-DD" format, in case of source==FIA it's the settings$run$end.date, otherwise end_date of the IC file in DB
##' @param new_site data frame, id/lat/lon/name info about the site
##' @param host list, host info as in settings$host, host$name forced to be "localhost" upstream
##' @param overwrite logical flag for convert_input
##' 
##' @export
##' 
##' @author Istem Fer
put_veg_module <- function(getveg.id, dbparms,
                            input_veg, pfts,
                            outfolder, n.ensemble,
                            dir, machine, model,
                            start_date, end_date,
                            new_site,
                            host, overwrite){



  #--------------------------------------------------------------------------------------------------#
  # Write model specific IC files
  con <- PEcAn.DB::db.open(dbparms$bety)
  on.exit(PEcAn.DB::db.close(con), add = TRUE)
  
  # Determine IC file format name and mimetype
  if (!is.null(input_veg$output)) {
    model_info <- PEcAn.DB::db.query(paste0("SELECT f.name, f.id, mt.type_string from modeltypes as m", " join modeltypes_formats as mf on m.id = mf.modeltype_id", 
                                            " join formats as f on mf.format_id = f.id", " join mimetypes as mt on f.mimetype_id = mt.id", 
                                            " where m.name = '", model, "' AND mf.tag='", input_veg$output,"'"), con)
    formatname <- model_info[1]
    mimetype   <- model_info[3]
  }else {
    PEcAn.logger::logger.error("Missing required information input_veg$output, this should be the tag identifier from the modeltypes_formats table")
  }
  
  PEcAn.logger::logger.info("Begin Model Specific Conversion")
  
  # spp.file <- db.query(paste("SELECT * from dbfiles where container_id =", getveg.id), con)
  spp.file <- PEcAn.DB::db.query(paste0("SELECT * from dbfiles where id = ", getveg.id$dbfile.id), con)

  pkg  <- "PEcAn.data.land"
  fcn  <- "write_ic"
  
  host.inputargs = host
  
  putveg.id <- PEcAn.DB::convert_input(input.id = getveg.id$input.id,
                                         outfolder = spp.file$file_path, 
                                         formatname = formatname, 
                                         mimetype = mimetype,
                                         site.id = new_site$id, 
                                         start_date = start_date, end_date = end_date, 
                                         pkg = pkg, fcn = fcn, 
                                         con = con, host = host,
                                         write = TRUE, 
                                         overwrite = overwrite,
                                         # fcn specific args 
                                         in.path = spp.file$file_path, 
                                              in.name = spp.file$file_name,
                                              model = model,
                                              new_site = new_site,
                                              pfts = pfts,
                                              source = input_veg$source,
                                              n.ensemble = n.ensemble,
                                              host.inputargs = host.inputargs)
  
  

  return(putveg.id)


}
