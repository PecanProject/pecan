##' Convert STICS output into the NACP Intercomparison format (ALMA using netCDF)
##' 
##' @name model2netcdf.STICS
##' @title Code to convert STICS' output into netCDF format
##'
##' @param outdir Location of model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @param overwrite Whether or not to overwrite existing output files 
##' @export
##'
##' @author Istem Fer
##' 
model2netcdf.STICS <- function(outdir, sitelat, sitelon, start_date, end_date, overwrite = FALSE) {
  
  ### Read in model output in STICS format
  out_files <- list.files(outdir)
  
  stics_out_file <- file.path(outdir, out_files[grepl("mod_s.*", out_files)])
  stics_output   <- lapply(stics_out_file, utils::read.table, header = TRUE, sep = ";")
  stics_output  <- do.call("rbind", stics_output)
  # probably already ordered, but order by year and DoY
  stics_output <-  stics_output[order(stics_output[,1], stics_output[,4]), ]

  
  simulation_years <- unique(stics_output$ian)
  
  # get all years that we want data from
  year_seq <- seq(lubridate::year(start_date), lubridate::year(end_date))
  
  # check that specified years and output years match
  if (!all(year_seq %in% simulation_years)) {
    # if not fail altogether, so that it won't break ensemble analysis 
    PEcAn.logger::logger.error("Years selected for model run and STICS output years do not match.")
  }
  
  # determine time step?
  
  for (y in simulation_years) {
    
    if (file.exists(file.path(outdir, paste(y, "nc", sep = "."))) & overwrite == FALSE) {
      next
    }
    
    thisyear <- stics_output[ , "ian"] == y
    
    outlist <- list()
    outlist[[length(outlist)+1]] <- stics_output[thisyear, "lai.n."]  # LAI in (m2 m-2)
    
    # daily amount of CO2-C emitted due to soil mineralisation (humus and organic residues) (kg ha-1 d-1)
    HeteroResp <- PEcAn.utils::ud_convert(stics_output[thisyear, "CO2sol"],  "ha-1 day-1", "m-2 s-1")
    
    outlist[[length(outlist)+1]] <- HeteroResp
    
    
    # dltams(n): daily growth rate of the plant (t.ha-1.d-1) 
    dltams      <- PEcAn.utils::ud_convert(stics_output[thisyear, "dltams.n."], "ton", "kg") * 0.48 # ton to kgC
    # dltaremobil: daily amount of perennial reserves remobilised (t.ha-1.d-1)
    dltaremobil <- PEcAn.utils::ud_convert(stics_output[thisyear, "dltaremobil"], "ton", "kg") * 0.48 # ton to kgC

    NPP <- dltams - dltaremobil # kgC ha-1 d-1
    NPP[NPP<0] <- 0
    
    # double checking that this is all NPP (above and below)
    ## this:
    #stics_output[thisyear, "dltams.n."]  # t.ha-1.d-1
    ## should be roughly equal to this:
    #diff(stics_output[thisyear, "masec.n."])+ diff(stics_output[thisyear, "msrac.n."]) # t.ha-1
    
    NPP <- PEcAn.utils::ud_convert(NPP, "ha-1 day-1", "m-2 s-1") # kg C m-2 s-1  
    outlist[[length(outlist)+1]] <- NPP
    
    NEE <- -1*(NPP-HeteroResp)
    outlist[[length(outlist)+1]] <- NEE
      
    # other vars
    # Cr: amount of C in organic residues mixed with soil (kg.ha-1)
    # Crac: amount of C in roots at harvest (kg.ha-1)
    # Chumt: amount of C in humified organic matter (active + inert fractions) (kg.ha-1)
    
    # ******************** Declare netCDF dimensions and variables ********************#
    t <- ncdf4::ncdim_def(name = "time", 
                          units = paste0("days since ", y, "-01-01 00:00:00"), 
                          stics_output[stics_output[,1] == y, 4], # allow partial years, this info is already in matrix_weather
                          calendar = "standard", 
                          unlim = TRUE)
    
    
    lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "station_latitude")
    lon <- ncdf4::ncdim_def("lon", "degrees_east",  vals = as.numeric(sitelon), longname = "station_longitude")
    
    dims <- list(lon = lon, lat = lat, time = t)
    
    nc_var <- list()
    nc_var[[length(nc_var)+1]] <- PEcAn.utils::to_ncvar("LAI", dims)
    nc_var[[length(nc_var)+1]] <- PEcAn.utils::to_ncvar("HeteroResp", dims)
    nc_var[[length(nc_var)+1]] <- PEcAn.utils::to_ncvar("NPP", dims)
    nc_var[[length(nc_var)+1]] <- PEcAn.utils::to_ncvar("NEE", dims)
    
    # ******************** Declare netCDF variables ********************#
    
    ### Output netCDF data
    nc <- ncdf4::nc_create(file.path(outdir, paste(y, "nc", sep = ".")), nc_var)
    varfile <- file(file.path(outdir, paste(y, "nc", "var", sep = ".")), "w")
    for (i in seq_along(nc_var)) {
      # print(i)
      ncdf4::ncvar_put(nc, nc_var[[i]], outlist[[i]])
      cat(paste(nc_var[[i]]$name, nc_var[[i]]$longname), file = varfile, sep = "\n")
    }
    close(varfile)
    ncdf4::nc_close(nc)
    
  } ### End of year loop

  
} # model2netcdf.STICS
