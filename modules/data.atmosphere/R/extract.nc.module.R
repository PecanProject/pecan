##' @export
##' @import dplyr
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
  

  #-- Double checking the start/end date for everyone and fixing them for ERA5
  # Why? bc for all sources the downloaded nc file has multiple years in it (the file can span multiple years)
  # But there are one input/tile per year of ERA5 . So two years has two inputs therefore it's all the tiles that covers the start_date and end_date
  # not just one. But met.process iterates over each input/tile thefore the start/end is not was it is sent and need to be fixed for ERA5.
  dates <- dplyr::tbl(con, "inputs") %>%
    dplyr::filter(id == cf.id$input.id) %>%
    dplyr::collect() %>%
    dplyr::select(start_date, end_date)
  
 
  ready.id <- PEcAn.utils::convert.input(input.id = input.id, 
                            outfolder = outfolder, 
                            formatname = formatname, 
                            mimetype = mimetype, 
                            site.id = site$id, 
                            start_date = dates$start_date,
                            end_date = dates$end_date,
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

