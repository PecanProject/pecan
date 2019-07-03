#' met2cf.ERA5
#'
#' @param lat latitude
#' @param long longitude
#' @param years years to be extracted
#' @param overwrite Logical flag definign if the files need to be overwriten in case they were downloaded before.
#' @param verbose Logical flag defining if ouput of function be extra verbose.
#' @param sitename The name of the site used for making the identifier.
#' @param data.folder Path to the directory where ERA5 nc files are located.
#' @param outfolder Path to directory where nc files need to be saved.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' files.era5 <- met2cf.ERA5(years=2018,
#'                   data.folder=".",
#'                   outfolder = "./metfiles")
#'
#' }
met2cf.ERA5 <- function(lat,
                        long,
                        start_date,
                        end_date,
                        sitename,
                        data.folder,
                        outfolder,
                        overwrite = FALSE,
                        verbose = TRUE) {
  ensemblesN <- seq(1, 10)

  years <- seq(lubridate::year(start_date),
               lubridate::year(end_date))
  # Extracting the raw data - The output would be a list of xts objects for each ensemble
  out <- ERA5_extract(
    lat = lat,
    long = long,
    years = years,
    data.folder = data.folder
  )
  
 
  #filter based on start and end date
  out <- out %>%
    purrr::map( ~ .x[paste0(as.Date(start_date), "/", as.Date(end_date))])
  
  
  start_date <- start_date %>% as.Date()
  end_date <- end_date %>% as.Date()
  # adding RH and converting rain
 
  out.new <- ensemblesN %>%
    purrr::map(function(ensi) {
      tryCatch({

        ens <- out[[ensi]]
        # Solar radation conversions
        #https://confluence.ecmwf.int/pages/viewpage.action?pageId=104241513
        #For ERA5 daily ensemble data, the accumulation period is 3 hours. Hence to convert to W/m2:
        ens[, "ssrd"] <- ens[, "ssrd"] / (3 * 3600)
        ens[, "strd"] <- ens[, "strd"] / (3 * 3600)
        #precipitation it's originaly in meters. Meters times the density will give us the kg/m2
        ens[, "tp"] <-
          ens[, "tp"] * 1000 / 3 # divided by 3 because we have 3 hours data
        ens[, "tp"] <-
          udunits2::ud.convert(ens[, "tp"], "kg m-2 hr-1", "kg m-2 6 s-1")  #There are 21600 seconds in 6 hours
        #RH
        #Adopted from weathermetrics/R/moisture_conversions.R
        t <-
          udunits2::ud.convert(ens[, "t2m"] %>% as.numeric(), "K", "degC")
        dewpoint  <-
          udunits2::ud.convert(ens[, "d2m"] %>% as.numeric(), "K", "degC")
        beta <- (112 - (0.1 * t) + dewpoint) / (112 + (0.9 * t))
        relative.humidity <- 100 * beta ^ 8
        #specific humidity
        specific_humidity <-
          PEcAn.data.atmosphere::rh2qair(relative.humidity,
                                         ens[, "t2m"] %>% as.numeric(),
                                         ens[, "sp"] %>% as.numeric()) # Pressure in Pa
      },
      error = function(e) {
        PEcAn.logger::logger.severe("Something went wrong during the unit conversion in met2cf ERA5.",
                                    conditionMessage(e))
      })
      
      
      #adding humidity
      xts::merge.xts(ens[, -c(3)], (specific_humidity)) %>%
        `colnames<-`(
          c(
            "air_temperature",
            "air_pressure",
            "precipitation_flux",
            "eastward_wind",
            "northward_wind",
            "surface_downwelling_shortwave_flux_in_air",
            "surface_downwelling_longwave_flux_in_air",
            "specific_humidity"
          )
        )
      
    })
  
  
  #These are the cf standard names
  cf_var_names = colnames(out.new[[1]])
  cf_var_units = c("K", "Pa", "kg m-2 s-1", "m s-1", "m s-1", "W m-2", "W m-2", "1")  #Negative numbers indicate negative exponents
  

  # Create a data frame with information about the file.  This data frame's format is an internal PEcAn standard, and is stored in the BETY database to
  # locate the data file. 
  results <- data.frame(
    file = "",
    #Path to the file (added in loop below).
    host = PEcAn.remote::fqdn(),
    mimetype = "application/x-netcdf",
    formatname = "CF Meteorology",
    startdate = paste0(format(
      start_date , "%Y-%m-%dT%H:%M:00 %z"
    )),
    enddate = paste0(format(
      end_date , "%Y-%m-%dT%H:%M:00 %z"
    )),
    dbfile.name = "ERA5",
    stringsAsFactors = FALSE
  )
  
  #Each ensemble gets its own file.
  time_dim = ncdf4::ncdim_def(
    name = "time",
    paste(units = "hours since", format(start_date, "%Y-%m-%dT%H:%M")),
    seq(0, (length(zoo::index(
      out.new[[1]]
    )) * 3) - 1 , length.out = length(zoo::index(out.new[[1]]))),
    create_dimvar = TRUE
  )
  lat_dim = ncdf4::ncdim_def("latitude", "degree_north", lat, create_dimvar = TRUE)
  lon_dim = ncdf4::ncdim_def("longitude", "degree_east", long, create_dimvar = TRUE)
  
  #create a list of all ens
  nc_var_list <- purrr::map2(cf_var_names,
                             cf_var_units,
                             ~ ncdf4::ncvar_def(.x, .y, list(time_dim, lat_dim, lon_dim), missval = NA_real_))
  
  #For each ensemble
  results_list <- purrr::map(ensemblesN,
                             function(i) {
                               # i is the ensemble number
                               #Generating a unique identifier string that characterizes a particular data set.
                               identifier <-
                                 paste(
                                   "ERA5",
                                   sitename,
                                   i,
                                   format(start_date, "%Y-%m-%dT%H:%M"),
                                   format(end_date, "%Y-%m-%dT%H:%M"),
                                   sep = "."
                                 )
                               
                               ensemble_folder <- file.path(outfolder, identifier)
                               
                               #Each file will go in its own folder.
                               if (!dir.exists(ensemble_folder)) {
                                 dir.create(ensemble_folder,
                                            recursive = TRUE,
                                            showWarnings = FALSE)
                               }
                               
                               flname <-
                                 file.path(ensemble_folder, paste(identifier, "nc", sep = "."))
                               
                               #Each ensemble member gets its own unique data frame, which is stored in results_list
                               results$file <- flname
                               results$dbfile.name <- flname
                               
                               
                               if (!file.exists(flname) || overwrite) {
                                 tryCatch({
                                   nc_flptr <- ncdf4::nc_create(flname, nc_var_list, verbose = verbose)
                                   
                                   #For each variable associated with that ensemble
                                   for (j in seq_along(cf_var_names)) {
                                     # "j" is the variable number.  "i" is the ensemble number.
                                     ncdf4::ncvar_put(nc_flptr, nc_var_list[[j]], zoo::coredata(out.new[[i]])[, nc_var_list[[j]]$name])
                                   }
                                   
                                   ncdf4::nc_close(nc_flptr)  #Write to the disk/storage
                                 },
                                 error = function(e) {
                                   PEcAn.logger::logger.severe("Something went wrong during the writing of the nc file.",
                                                               conditionMessage(e))
                                 })
                                 
                               } else {
                                 PEcAn.logger::logger.info(paste0("The file ",
                                                                  flname,
                                                                  " already exists.  It was not overwritten."))
                               }
                               results
                               
                             })
  
  return(results_list)
}
