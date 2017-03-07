.get.veg.module <- function(dir = dir,
                            met = met, 
                            machine = machine, 
                            start_date = start_date, end_date = end_date,
                            str_ns = str_ns, con = con, 
                            input_veg = input, 
                            site.id = new.site$id, 
                            lat.in = new.site$lat, lon.in = new.site$lon, 
                            host = host, 
                            overwrite = overwrite$download,
                            site = site){
  
  # which sources require load_data
  load_sources <- c("GapMacro", "NASA_FFT_Plot_Inventory", "NASA_TE_FIA")
  
  # determine function
  if(input_veg$source == "FIA"){
    pkg <- "PEcAn.data.land"
    fcn <- "extract_FIA"
  }else if(input_veg$source %in% load_sources){
    pkg <- "PEcAn.benchmark"
    fcn <- "load_data"
  }
  
  obs <- extract.FIA(inputinfo, lat, lon, year = start_year, dbparms)
  obs <- load_data(data.path, format, site = runinfo$site)

  raw.id <- convert.input(input.id = NA,
                          outfolder = outfolder, 
                          formatname = register$format$name, 
                          mimetype = register$format$mimetype,
                          site.id = site.id, 
                          start_date = start_date, end_date = end_date, 
                          pkg = pkg, fcn = fcn, 
                          con = con, host = host, browndog = NULL, 
                          write = TRUE, 
                          format.vars = ,
                          overwrite = overwrite, 
                          sitename = site$name)
  
  
  return(raw.id)
  
}