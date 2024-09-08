##' Convert LPJ-GUESS output to netCDF
##' 
##' @name model2netcdf.LPJGUESS
##' @title Function to convert LPJ-GUESS model output to standard netCDF format
##'
##' @param outdir Location of LPJ-GUESS model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @export
##'
##' @author Istem Fer
model2netcdf.LPJGUESS <- function(outdir, sitelat, sitelon, start_date, end_date) {
  
  ### Read in model output in LPJ-GUESS format
  lpjguess.out.files <- list.files(outdir, pattern = "\\.out$")
  
  if (length(lpjguess.out.files) == 0) {
    PEcAn.logger::logger.error("No output files found at ", outdir)
  }
  
  lpjguess.output <- lapply(file.path(outdir, lpjguess.out.files), read.table, header = TRUE, sep = "")
  # n.outputs <- length(lpjguess.output)
  m.to.s <- 2592000
  
  years <- seq(lubridate::year(start_date), lubridate::year(end_date))
  
  ### Unit conversions
  
  # mgpp 'monthly gross primary production' in kgC/m2/month to GPP kgC/m2/s
  if ("mgpp.out" %in% lpjguess.out.files) {
    gpp <- lpjguess.output[[which(lpjguess.out.files == "mgpp.out")]][, 4:15] / m.to.s
  }
  
  # mnpp 'monthly net primary production' in kgC/m2/month to NPP kgC/m2/s
  if ("mnpp.out" %in% lpjguess.out.files) {
    npp <- lpjguess.output[[which(lpjguess.out.files == "mnpp.out")]][, 4:15] / m.to.s
  }
  
  # mra 'monthly autotrophic respiration' in kgC/m2/month to AutoResp kgC/m2/s
  if ("mra.out" %in% lpjguess.out.files) {
    arp <- lpjguess.output[[which(lpjguess.out.files == "mra.out")]][, 4:15] / m.to.s
  }
  
  # mrh 'monthly heterotrophic respiration' in kgC/m2/month to HeteroResp kgC/m2/s
  if ("mrh.out" %in% lpjguess.out.files) {
    hrp <- lpjguess.output[[which(lpjguess.out.files == "mrh.out")]][, 4:15] / m.to.s
  }
  
  # mnee 'monthly net ecosystem C exchange' in kgC/m2/month to NEE kgC/m2/s
  if ("mnee.out" %in% lpjguess.out.files) {
    nee <- lpjguess.output[[which(lpjguess.out.files == "mnee.out")]][, 4:15] / m.to.s
  }
  
  # mlai 'monthly Leaf Area Index' in m2/m2 to LAI m2/m2
  if ("mnee.out" %in% lpjguess.out.files) {
    lai <- lpjguess.output[[which(lpjguess.out.files == "mlai.out")]][, 4:15]
  }
  
  ### Loop over years in LPJ-GUESS output to create separate netCDF outputs
  for (y in years) {
    
    if (file.exists(file.path(outdir, paste(y, "nc", sep = ".")))) {
      next
    }
    
    print(paste("---- Processing year: ", y))
    
    ## Set up outputs for netCDF file in appropriate units
    
    ## TODO: generalize for all possible outputs, both yearly and monthly
    
    output <- list()
    output[[1]] <- gpp[which(years == y), ]  # GPP in kgC/m2/s
    output[[2]] <- npp[which(years == y), ]  # NPP in kgC/m2/s
    output[[3]] <- arp[which(years == y), ]  # AutoResp in kgC/m2/s
    output[[4]] <- hrp[which(years == y), ]  # HeteroResp in kgC/m2/s
    output[[5]] <- nee[which(years == y), ]  # NEE in kgC/m2/s
    output[[6]] <- lai[which(years == y), ]  # LAI in m2/m2
    
    if(lubridate::leap_year(y)){
      month_days <- c(001, 032, 061, 092, 122, 153, 183, 214, 245, 275, 306, 336)
    } else {
      month_days <- c(001, 032, 060, 091, 121, 152, 182, 213, 244, 274, 305, 335)
    }

    # ******************** Declare netCDF dimensions and variables ********************#
    t <- ncdf4::ncdim_def(name = "time", 
                   units = paste0("days since ", y, "-01-01 00:00:00"), 
                   month_days,
                   calendar = "standard", 
                   unlim = TRUE)
    lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "station_latitude")
    lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = as.numeric(sitelon), longname = "station_longitude")
    
    mstmipvar <- PEcAn.utils::mstmipvar
    
    dims <- list(lon = lon, lat = lat, time = t)
    
    nc_var <- list()
    nc_var[[1]] <- PEcAn.utils::to_ncvar("GPP", dims)
    nc_var[[2]] <- PEcAn.utils::to_ncvar("NPP", dims)
    nc_var[[3]] <- PEcAn.utils::to_ncvar("AutoResp", dims)
    nc_var[[4]] <- PEcAn.utils::to_ncvar("HeteroResp", dims)
    nc_var[[5]] <- PEcAn.utils::to_ncvar("NEE", dims)
    nc_var[[6]] <- PEcAn.utils::to_ncvar("LAI", dims)
    
    # ******************** Declare netCDF variables ********************#
    
    ### Output netCDF data
    nc <- ncdf4::nc_create(file.path(outdir, paste(y, "nc", sep = ".")), nc_var)
    varfile <- file(file.path(outdir, paste(y, "nc", "var", sep = ".")), "w")
    for (i in seq_along(nc_var)) {
      # print(i)
      ncdf4::ncvar_put(nc, nc_var[[i]], output[[i]])
      cat(paste(nc_var[[i]]$name, nc_var[[i]]$longname), file = varfile, sep = "\n")
    }
    close(varfile)
    ncdf4::nc_close(nc)
  }  ### End of year loop
} # model2netcdf.LPJGUESS
