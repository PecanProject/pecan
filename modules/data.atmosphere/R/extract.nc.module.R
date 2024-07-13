##' @export
.extract.nc.module <- function(cf.id, register, dir, met, str_ns, site, new.site, con, 
                               start_date, end_date, host, overwrite = FALSE) {
  PEcAn.logger::logger.info("Site Extraction")
  
  input.id <- cf.id[1]
  if(host$name == "localhost"){
    outfolder <- file.path(dir, paste0(met, "_CF_site_", str_ns))
  } else {
    if(is.null(host$folder)){
      PEcAn.logger::logger.severe("host$folder required when running extract.nc.module for remote servers")
    } else {
      outfolder <- file.path(host$folder, paste0(met, "_CF_site_", str_ns))
    }
  }

  pkg        <- "PEcAn.data.atmosphere"
  fcn        <- "extract.nc"
  formatname <- "CF Meteorology"
  mimetype   <- "application/x-netcdf"
  
if (exists(paste0("extract.nc.", met))) fcn <- paste0("extract.nc.", met)
  

  ready.id <- PEcAn.DB::convert_input(input.id = input.id, 
                            outfolder = outfolder, 
                            formatname = formatname, 
                            mimetype = mimetype, 
                            site.id = site$id, 
                            start_date = start_date,
                            end_date = end_date,
                            pkg = pkg, 
                            fcn = fcn, 
                            con = con, host = host, browndog = NULL, 
                            write = TRUE, 
                            slat = new.site$lat, slon = new.site$lon,
                            newsite = new.site$id, 
                            overwrite = overwrite,
                            exact.dates = FALSE, 
                            ensemble = register$ensemble %>% as.numeric())
  
  PEcAn.logger::logger.info("Finished Extracting Met")
  
  return(ready.id)
} # .extract.nc.module

