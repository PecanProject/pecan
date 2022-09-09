#' met2CF.ECMWF
#'
#' Converts ECMWF Open Data variables to CF format.
#'
#' Variables present in the output netCDF files:
#' air_temperature, air_temperature, air_pressure, 
#' precipitation_flux, eastward_wind, northward_wind
#'
#' @param lat.in latitude
#' @param lon.in longitude
#' @param outfolder Path to directory where nc files need to be saved.
#' @param overwrite Logical if files needs to be overwritten.
#' @param verbose Logical flag defining if ouput of function be extra verbose.
#'
#' @return list of files in a dataframe
#' @export
#'
#' @author Swarnalee Mazumder
#'
met2CF.ECMWF <- function(lat.in,
                         lon.in,
                         outfolder,
                         overwrite = FALSE,
                         verbose = TRUE) {
  
  results <-
    PEcAn.data.atmosphere::mergenc_ECMWF(lat.in,
                                         lon.in,
                                         outfolder,
                                         overwrite = overwrite)
  return(results)
}