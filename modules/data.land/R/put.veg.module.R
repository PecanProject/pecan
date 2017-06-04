.put.veg.module <- function(getveg.id, bety, 
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
  
  # Determine IC file format name and mimetype
  model_info <- db.query(paste0("SELECT f.name, f.id, mt.type_string from modeltypes as m", " join modeltypes_formats as mf on m.id = mf.modeltype_id", 
                                " join formats as f on mf.format_id = f.id", " join mimetypes as mt on f.mimetype_id = mt.id", 
                                " where m.name = '", model, "' AND mf.tag='", input_veg$output,"'"), con)
  
  logger.info("Begin Model Specific Conversion")
  
  formatname <- model_info[1]
  mimetype   <- model_info[3]
  
  ## Set model-specific functions
  pkg <- paste0("PEcAn.", model)
  do.call("library", list(pkg))
  fcn <- paste("veg2model.", model, sep = "")
  if (!exists(fcn)) {
    logger.severe(paste(fcn, "does not exist."))
  }
  
  # NOTE : match_pft is now called in veg2model
  
  spp.file <- db.query(paste("SELECT * from dbfiles where container_id =", getveg.id), con)
  
  putveg.id <- convert.input(input.id = getveg.id,
                             outfolder = outfolder, 
                             formatname = formatname, 
                             mimetype = mimetype,
                             site.id = site_id, 
                             start_date = start_date, end_date = end_date, 
                             pkg = pkg, fcn = fcn, 
                             con = con, host = host, browndog = NULL, 
                             write = TRUE, 
                             overwrite = overwrite, 
                             # fcn specific args 
                             in.path = spp.file$file_path, 
                             in.name = spp.file$file_name,
                             new_site = new_site,
                             source = input_veg$source)
  
  return(putveg.id)
  

}