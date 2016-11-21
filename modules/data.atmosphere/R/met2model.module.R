.met2model.module <- function(ready.id, model, con, host, dir, met, str_ns, site, start_date, end_date, 
                              browndog, new.site, overwrite = FALSE, exact.dates) {
  
  # Determine output format name and mimetype
  model_info <- db.query(paste0("SELECT f.name, f.id, mt.type_string from modeltypes as m", " join modeltypes_formats as mf on m.id = mf.modeltype_id", 
                                " join formats as f on mf.format_id = f.id", " join mimetypes as mt on f.mimetype_id = mt.id", 
                                " where m.name = '", model, "' AND mf.tag='met'"), con)
  
  if (model_info[1] == "CF Meteorology") {
    model.id <- ready.id
    outfolder <- file.path(dir, paste0(met, "_site_", str_ns))
  } else {
    logger.info("Begin Model Specific Conversion")
    
    formatname <- model_info[1]
    mimetype <- model_info[3]
    
    print("Convert to model format")
    
    input.id <- ready.id$input.id[1]
    outfolder <- ifelse(host$name == "localhost", 
                        file.path(dir, paste0(met, "_", model, "_site_", str_ns)), 
                        file.path(host$dbfiles, paste0(met, "_", model, "_site_", str_ns)))
    
    pkg <- paste0("PEcAn.", model)
    fcn <- paste0("met2model.", model)
    lst <- site.lst(site, con)
    
    model.id <- convert.input(input.id, 
                              outfolder,
                              formatname, mimetype, 
                              site.id = site$id, 
                              start_date, end_date, 
                              pkg, fcn, con = con, host = host, browndog,
                              write = TRUE,
                              lst = lst, 
                              lat = new.site$lat, lon = new.site$lon, 
                              overwrite = overwrite,
                              exact.dates = exact.dates)
  }
  
  logger.info(paste("Finished Model Specific Conversion", model.id[1]))
  return(list(outfolder = outfolder, model.id = model.id))
} # .met2model.module
