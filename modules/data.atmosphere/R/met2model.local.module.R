##' @export
.met2model.local.module <- function(inputfiles, model, con, host, dir, met, str_ns, site, start_date, end_date, format.vars, 
                                    browndog, new.site, overwrite = FALSE, exact.dates,spin) {
  # Determine output format name and mimetype
  model_info <- PEcAn.DB::db.query(paste0("SELECT f.name, f.id, mt.type_string from modeltypes as m", " join modeltypes_formats as mf on m.id = mf.modeltype_id", 
                                          " join formats as f on mf.format_id = f.id", " join mimetypes as mt on f.mimetype_id = mt.id", 
                                          " where m.name = '", model, "' AND mf.tag='met'"), con)
  
  if (model_info[1] == "CF Meteorology") {
    outfolder <- file.path(dir, paste0(met, "_site_", str_ns))
  } else {
    PEcAn.logger::logger.info("Begin Model Specific Conversion")
    
    formatname <- model_info[1]
    mimetype <- model_info[3]
    
    print("Convert to model format")
    
    if(host$name == "localhost"){
      outfolder <- file.path(dir, paste0(met, "_", model, "_site_", str_ns))
    } else {
      if(is.null(host$folder)){
        PEcAn.logger::logger.severe("host$folder required when running met2model.module for remote servers")
      } else {
        outfolder <- file.path(host$folder, paste0(met, "_", model, "_site_", str_ns))
      }
    }
    
    pkg <- paste0("PEcAn.", model)
    fcn <- paste0("met2model.", model)
    lst <- site.lst(site, con)
    
    fcn.args <- list(lst = lst, 
                     lat = new.site$lat, lon = new.site$lon, 
                     spin_nyear = spin$nyear,
                     spin_nsample = spin$nsample,
                     spin_resample = spin$resample)
    fcn.args$overwrite  <- overwrite
    fcn.args$in.path    <- dirname(inputfiles[1])
    fcn.args$in.prefix  <- unlist(strsplit(basename(inputfiles[1]), "[.]"))[1]
    fcn.args$outfolder  <- outfolder
    fcn.args$start_date <- start_date
    fcn.args$end_date   <- end_date
    arg.string <- PEcAn.utils::listToArgString(fcn.args)
    
    if (!missing(format.vars)) {
      arg.string <- paste0(arg.string, ", format=", paste0(list(format.vars)))
    }
    
    cmdFcn <- paste0(pkg, "::", fcn, "(", arg.string, ")")
    Rbinary <- ifelse(!exists("settings") || is.null(settings$host$Rbinary),"R",settings$host$Rbinary)
    
    PEcAn.logger::logger.debug(paste0("convert.input executing the following function:\n", cmdFcn))
    
    result <- PEcAn.remote::remote.execute.R(script = cmdFcn, host, user = NA, verbose = TRUE, R = Rbinary, scratchdir = outfolder)
  }
  
  PEcAn.logger::logger.info(paste("Finished Model Specific Conversion"))
  return(result$file[1])
} # .met2model.local.module