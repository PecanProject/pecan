##' @export
.met2model.module <- function(ready.id, model, con, host, dir, met, str_ns, site, start_date, end_date, 
                               new.site, overwrite = FALSE, exact.dates,spin, register, ensemble_name) {
  
  # Determine output format name and mimetype
  model_info <- PEcAn.DB::db.query(paste0("SELECT f.name, f.id, mt.type_string from modeltypes as m", " join modeltypes_formats as mf on m.id = mf.modeltype_id", 
                                " join formats as f on mf.format_id = f.id", " join mimetypes as mt on f.mimetype_id = mt.id", 
                                " where m.name = '", model, "' AND mf.tag='met'"), con)
  
  if (model_info[1] == "CF Meteorology") {
    model.id <- ready.id
    outfolder <- file.path(dir, paste0(met, "_site_", str_ns))
  } else {
    PEcAn.logger::logger.info("Begin Model Specific Conversion")
    
    formatname <- model_info[1]
    mimetype <- model_info[3]
    
    print("Convert to model format")
    
    input.id <- ready.id$input.id[1]
    
    if(host$name == "localhost"){
      outfolder <- file.path(dir, paste0(met, "_", model, "_site_", str_ns))
    } else {
      if(is.null(host$folder)){
        PEcAn.logger::logger.severe("host$folder required when running met2model.module for remote servers")
      } else {
        outfolder <- file.path(host$folder, paste0(met, "_", model, "_site_", str_ns))
      }
    }
    
    #Some data products can be forecasts instead of real time data.
    #Not all of the registration.xml files for each data source contains a <forecast> tag.
    forecast = FALSE
    if (!is.null(register$forecast)) {
      forecast = as.logical(register$forecast)
    }
    
    pkg <- paste0("PEcAn.", model)
    fcn <- paste0("met2model.", model)
    lst <- site.lst(site.id=site$id, con=con)
   
    # we add the ensemble number to the input name
    if (!is.null(register$ensemble)) {
      outfolder <- paste0(outfolder,"_",ensemble_name)
    }

    
    model.id <- PEcAn.DB::convert_input(input.id = input.id,
                              outfolder = outfolder,
                              formatname = formatname, mimetype = mimetype, 
                              site.id = site$id, 
                              start_date = start_date, end_date = end_date, 
                              pkg = pkg, fcn = fcn, con = con, host = host,
                              write = TRUE,
                              lst = lst, 
                              lat = new.site$lat, lon = new.site$lon, 
                              overwrite = overwrite,
                              exact.dates = exact.dates,
                              spin_nyear = spin$nyear,
                              spin_nsample = spin$nsample,
                              spin_resample = spin$resample,
                              forecast = forecast,
                              ensemble = !is.null(register$ensemble) && as.logical(register$ensemble),
                              ensemble_name = ensemble_name,
                              dbfile.id=ready.id$dbfile.id)
  }
  
  PEcAn.logger::logger.info(paste("Finished Model Specific Conversion", model.id[1]))
  return(list(outfolder = outfolder, model.id = model.id))
} # .met2model.module
