#' met2cf.ERA5
#'
#' @param lat latitude
#' @param long longitude
#' @param start_date start date
#' @param end_date end date
#' @param sitename The name of the site used for making the identifier.
#' @param outfolder Path to directory where nc files need to be saved.
#' @param out.xts Output of the extract.nc.ERA5 function which is a list of time series of met variables for each ensemble member.
#' @param overwrite Logical if files needs to be overwritten.
#' @param verbose Logical flag defining if ouput of function be extra verbose.


#'
#' @return list of dataframes
#' @export
#'
met2CF.ERA5<- function(lat,
                        long,
                        start_date,
                        end_date,
                        sitename,
                        outfolder,
                        out.xts,
                        overwrite = FALSE,
                        verbose = TRUE) {

  years <- seq(lubridate::year(start_date),
               lubridate::year(end_date),
               1
  )
  
  ensemblesN <- seq(1, 10)

  start_date <- paste0(lubridate::year(start_date),"-01-01")  %>% as.Date()
  end_date <- paste0(lubridate::year(end_date),"-12-31") %>% as.Date()
  # adding RH and converting rain

  # Define variable mapping 
  cf_mapping <- c(
    "t2m" = "air_temperature",
    "sp" = "air_pressure", 
    "tp" = "precipitation_flux",
    "u10" = "eastward_wind",
    "v10" = "northward_wind",
    "ssrd" = "surface_downwelling_shortwave_flux_in_air",
    "strd" = "surface_downwelling_longwave_flux_in_air"
  )
 
  out.new <- ensemblesN %>%
    purrr::map(function(ensi) {
      tryCatch({
  
        ens <- out.xts[[ensi]]
        # Solar radation conversions
        #https://confluence.ecmwf.int/pages/viewpage.action?pageId=104241513
        available_vars <- colnames(ens)
        
        # Detect timestep dynamically (use median for robustness)
        time_diffs <- diff(as.numeric(zoo::index(ens)))
        if (length(time_diffs) > 0) {
          timestep_seconds <- as.numeric(median(time_diffs))
        } else {
          timestep_seconds <- 3 * 3600 # Fallback to 3-hourly
          if (verbose) PEcAn.logger::logger.info("Only one timestamp found. Defaulting to 3-hour timestep for conversion.")
        }
        timestep_hours <- timestep_seconds / 3600
        
        # Solar radiation conversions (J/m2 to W/m2)
        if ("ssrd" %in% available_vars) ens[, "ssrd"] <- ens[, "ssrd"] / timestep_seconds
        if ("strd" %in% available_vars) ens[, "strd"] <- ens[, "strd"] / timestep_seconds
        
        # Precipitation - m to kg/m2/s
        if ("tp" %in% available_vars) {
          ens[, "tp"] <- (ens[, "tp"] * 1000) / timestep_seconds
        }
        #Adopted from weathermetrics/R/moisture_conversions.R
        # Relative and specific humidity (only if all required vars present)
        specific_humidity <- NULL
        if (all(c("t2m", "d2m", "sp") %in% available_vars)) {
          t <-
            PEcAn.utils::ud_convert(ens[, "t2m"] %>% as.numeric(), "K", "degC")
          dewpoint  <-
            PEcAn.utils::ud_convert(ens[, "d2m"] %>% as.numeric(), "K", "degC")
          beta <- (112 - (0.1 * t) + dewpoint) / (112 + (0.9 * t))
          relative.humidity <- beta ^ 8
          #specific humidity
          specific_humidity <-
            PEcAn.data.atmosphere::rh2qair(relative.humidity,
                                           ens[, "t2m"] %>% as.numeric(),
                                           ens[, "sp"] %>% as.numeric()) # Pressure in Pa
        }
        
        # Select available ERA5 variables and convert to CF naming
        available_era5_vars <- intersect(names(cf_mapping), available_vars)
        ens_cf <- ens[, available_era5_vars, drop = FALSE]
        colnames(ens_cf) <- cf_mapping[available_era5_vars]
        
        # Add specific humidity if calculated successfully
        if (!is.null(specific_humidity)) {
          colnames(specific_humidity) <- "specific_humidity"
          ens_cf <- xts::merge.xts(ens_cf, specific_humidity)
        }
        
        # Attach timestep as attribute for downstream use
        attr(ens_cf, "timestep_hours") <- timestep_hours
        return(ens_cf)
        
      },
      error = function(e) {
        PEcAn.logger::logger.severe("Something went wrong during the unit conversion in met2cf ERA5.",
                                    conditionMessage(e))
        return(NULL)
      })
      
    })
  
  # Filter out NULL results from failed ensembles
  out.new <- out.new[!sapply(out.new, is.null)]
  
  if (length(out.new) == 0) {
    PEcAn.logger::logger.severe("No valid ensembles processed")
    return(NULL)
  }

  # Define units mapping for CF variables
  cf_units_mapping <- c(
    "air_temperature" = "K",
    "air_pressure" = "Pa", 
    "precipitation_flux" = "kg m-2 s-1",
    "eastward_wind" = "m s-1",
    "northward_wind" = "m s-1",
    "surface_downwelling_shortwave_flux_in_air" = "W m-2",
    "surface_downwelling_longwave_flux_in_air" = "W m-2",
    "specific_humidity" = "1"
  )
  
  #These are the cf standard names
  cf_var_names = colnames(out.new[[1]])
  cf_var_units = cf_units_mapping[cf_var_names]
  

  results_list <-  ensemblesN %>%
    purrr::map(function(i) {
      
      start_date <- min(zoo::index(out.new[[i]]))
      end_date <- max(zoo::index(out.new[[i]]))
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
        dbfile.name = paste0("ERA5.", i),
        stringsAsFactors = FALSE
      )
      
      # i is the ensemble number
      #Generating a unique identifier string that characterizes a particular data set.
      identifier <- paste("ERA5", sitename, i, sep = "_")
      
      identifier.file <- paste("ERA5",
                               i,
                               lubridate::year(start_date),
                               sep = ".")
      
      ensemble_folder <- file.path(outfolder, identifier)
      
      #Each file will go in its own folder.
      if (!dir.exists(ensemble_folder)) {
        dir.create(ensemble_folder,
                   recursive = TRUE,
                   showWarnings = FALSE)
      }
      
      flname <-file.path(ensemble_folder, paste(identifier.file, "nc", sep = "."))
      
      #Each ensemble member gets its own unique data frame, which is stored in results_list
      results$file <- flname
      
      years %>%
        purrr::map(function(year) {
          #
          identifier.file <- paste("ERA5",
                                   i,
                                   year,
                                   sep = ".")
          
          flname <-file.path(ensemble_folder, paste(identifier.file, "nc", sep = "."))
          # Spliting it for this year
          data.for.this.year.ens <- out.new[[i]]
          data.for.this.year.ens <- data.for.this.year.ens[year %>% as.character]
          
          if (nrow(data.for.this.year.ens) == 0) return(NULL)
          
          # Use actual timestamps for time dimension (robust, CF-compliant)
          time_vals <- as.numeric(zoo::index(data.for.this.year.ens))
          time_dim <- ncdf4::ncdim_def(
            name = "time",
            units = "seconds since 1970-01-01 00:00:00",
            vals = time_vals,
            create_dimvar = TRUE
          )
          lat_dim = ncdf4::ncdim_def("latitude", "degree_north", lat, create_dimvar = TRUE)
          lon_dim = ncdf4::ncdim_def("longitude", "degree_east", long, create_dimvar = TRUE)
          
          #create a list of all ens
          nc_var_list <- purrr::map2(cf_var_names,
                                     cf_var_units,
                                     ~ ncdf4::ncvar_def(.x, .y, list(time_dim, lat_dim, lon_dim), missval = NA_real_))
       
          #results$dbfile.name <- flname
          
          
          if (!file.exists(flname) || overwrite) {
            tryCatch({
              nc_flptr <- ncdf4::nc_create(flname, nc_var_list, verbose = verbose)
              
              #For each variable associated with that ensemble
              for (j in seq_along(cf_var_names)) {
                # "j" is the variable number.  "i" is the ensemble number.
                ncdf4::ncvar_put(nc_flptr,
                                 nc_var_list[[j]],
                                 zoo::coredata(data.for.this.year.ens)[, j])
              }
              
              ncdf4::nc_close(nc_flptr)  #Write to the disk/storage
            },
            error = function(e) {
              PEcAn.logger::logger.severe("Something went wrong during the writing of the nc file.",
                                          conditionMessage(e))
            })
            
          } else {
            PEcAn.logger::logger.info(paste0(
              "The file ",
              flname,
              " already exists.  It was not overwritten."
            ))
          }
        }) 
      return(results)
    })
  #For each ensemble
  return(results_list )
}
