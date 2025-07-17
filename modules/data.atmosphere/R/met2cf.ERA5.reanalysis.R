#' met2CF.ERA5.reanalysis - function for ERA5 reanalysis data processing
#'
#' @param slat latitude
#' @param slon longitude
#' @param in.path path to the directory containing the ERA5 reanalysis NetCDF files
#' @param start_date start date
#' @param end_date end date
#' @param sitename the name of the site used for making the identifier
#' @param outfolder path to directory where CF-compliant nc files need to be saved
#' @param in.prefix initial portion of the filename that does not vary by date 
#' @param vars variables to be extracted. If NULL all available variables will be returned
#' @param overwrite logical if files need to be overwritten
#' @param verbose logical flag defining if output of function be extra verbose
#'
#' @return dataframe with file information for processed NetCDF files
#' @export
#' @author Akash
met2CF.ERA5.reanalysis <- function(slat, slon, in.path, start_date, end_date, sitename, outfolder,
                                   in.prefix, vars = NULL, overwrite = FALSE, verbose = TRUE) {

  if (is.na(slat) || is.na(slon)) {
    PEcAn.logger::logger.severe("Invalid latitude or longitude provided")
    return(NULL)
  }
  years <- seq(lubridate::year(start_date),
               lubridate::year(end_date),
               1)
  
  # define variable mapping from ERA5 to CF standard names
  cf_mapping <- c(
    "t2m" = "air_temperature",
    "sp" = "air_pressure", 
    "tp" = "precipitation_flux",
    "u10" = "eastward_wind",
    "v10" = "northward_wind",
    "ssrd" = "surface_downwelling_shortwave_flux_in_air",
    "strd" = "surface_downwelling_longwave_flux_in_air"
  )

  # define units mapping for CF variables
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
  
  tryCatch({
    # process each year
    out.xts <- years %>%
      purrr::map(function(year) {
        ncfile <- file.path(in.path, paste0(in.prefix, year, ".nc"))
        if (!file.exists(ncfile)) {
          PEcAn.logger::logger.warn(paste0("File not found: ", ncfile))
          return(NULL)
        }
        nc_data <- ncdf4::nc_open(ncfile)
        on.exit(ncdf4::nc_close(nc_data), add = TRUE)
        
        time_var <- if ("time" %in% names(nc_data$var)) "time" else "valid_time"
        t <- ncdf4::ncvar_get(nc_data, time_var)
        tunits <- ncdf4::ncatt_get(nc_data, time_var)
        tustr <- strsplit(tunits$units, " ")
        
        if(time_var == "time") {
          # legacy format - "hours since YYYY-MM-DD HH:MM:SS"
          timestamp <- as.POSIXct(t * 3600, tz = "UTC", origin = tustr[[1]][3])
        } else {
          # new format - "seconds since YYYY-MM-DD HH:MM:SS"
          timestamp <- as.POSIXct(t, tz = "UTC", origin = tustr[[1]][3])
        }

        lon <- ncdf4::ncvar_get(nc_data, "longitude")
        lat <- ncdf4::ncvar_get(nc_data, "latitude")
        lon_ix <- which.min(abs(lon - slon))
        lat_iy <- which.min(abs(lat - slat))
        
        if(is.null(vars)) {
          all_vars <- names(nc_data$var)
          vars <- all_vars[sapply(all_vars, function(v) {
            var_info <- nc_data$var[[v]]
            var_info$ndims == 3 && 
              var_info$prec %in% c("float", "double", "integer") &&
              !v %in% c("longitude", "latitude", "time", "valid_time")
          })]
        } else {
          vars <- vars
        }
        
        if(length(vars) == 0) {
          PEcAn.logger::logger.warn(paste0("No valid variables found in ", ncfile))
          return(NULL)
        }
        
        extracted_data <- vars %>%
          purrr::set_names(vars) %>%
          purrr::map_dfc(function(vname) {
            vals <- ncdf4::ncvar_get(nc_data, vname)
            if(!is.null(nc_data$var[[vname]]$misval)){
              missval <- nc_data$var[[vname]]$missval
            } else {
              missval <- NA
            }
            if (length(dim(vals)) == 3) {
              point_vals <- vals[lon_ix, lat_iy, ]
            } else {
              PEcAn.logger::logger.warn(paste0("Unexpected dimensions for variable ", vname))
              point_vals <- rep(NA, length(timestamp))
            }
            point_vals[point_vals == missval] <- NA
            return(point_vals)
          })
        # create xts object
        ens <- xts::xts(extracted_data, order.by = timestamp)
        return(ens)
      }) %>%
      purrr::discard(is.null)
    if(length(out.xts) == 0) {
      PEcAn.logger::logger.severe("No data successfully extracted from any files")
      return(NULL)
    }
    rbind.xts <- do.call("::", list("xts", "rbind.xts"))
    out.new <- do.call("rbind.xts", out.xts)
    vars <- colnames(out.new)

    time_diffs <- diff(as.numeric(zoo::index(out.new)))
    if(length(time_diffs) > 0) {
      timestep_seconds <- as.numeric(median(time_diffs))
    } else {
      timestep_seconds <- 3600 
    }
    timestep_hours <- timestep_seconds / 3600

    # solar radiation conversions(J/m2 to W/m2)
    if("ssrd" %in% vars) {
      out.new[, "ssrd"] <- out.new[, "ssrd"] / timestep_seconds
    }
    if("strd" %in% vars) {
      out.new[, "strd"] <- out.new[, "strd"] / timestep_seconds
    }
    # precipitation conversion(m to kg/m2/s)
    if("tp" %in% vars) {
      out.new[, "tp"] <- (out.new[, "tp"] * 1000) / timestep_seconds
    }
    specific_humidity <- NULL
    if(all(c("t2m", "d2m", "sp") %in% vars)) {
      t <- PEcAn.utils::ud_convert(out.new[, "t2m"] %>% as.numeric(), "K", "degC")
      dewpoint <- PEcAn.utils::ud_convert(out.new[, "d2m"] %>% as.numeric(), "K", "degC")
      beta <- (112 - (0.1 * t) + dewpoint) / (112 + (0.9 * t))
      relative.humidity <- beta ^ 8
      
      specific_humidity <- PEcAn.data.atmosphere::rh2qair(
        relative.humidity,
        out.new[, "t2m"] %>% as.numeric(),
        out.new[, "sp"] %>% as.numeric()
      )
    }
    era5_vars <- intersect(names(cf_mapping), vars)
    if(length(era5_vars) == 0) {
      PEcAn.logger::logger.severe("No mappable ERA5 variables found")
      return(NULL)
    }
    cf_data <- out.new[, era5_vars, drop = FALSE]
    colnames(cf_data) <- cf_mapping[era5_vars]
    if(!is.null(specific_humidity)) {
      colnames(specific_humidity) <- "specific_humidity"
      cf_data <- xts::merge.xts(cf_data, specific_humidity)
    }
    cf_var_names <- colnames(cf_data)
    cf_var_units <- cf_units_mapping[cf_var_names]
    identifier <- paste("ERA5", sitename, "reanalysis", sep = "_")
    reanalysis_folder <- file.path(outfolder, identifier)
    
    if(!dir.exists(reanalysis_folder)) {
      dir.create(reanalysis_folder, recursive = TRUE, showWarnings = FALSE)
    }
    
    start_date <- min(zoo::index(cf_data))
    end_date <- max(zoo::index(cf_data))
    
    results <- data.frame(
      file = "",
      host = PEcAn.remote::fqdn(),
      mimetype = "application/x-netcdf",
      formatname = "CF Meteorology",
      startdate = paste0(format(start_date, "%Y-%m-%dT%H:%M:00 %z")),
      enddate = paste0(format(end_date, "%Y-%m-%dT%H:%M:00 %z")),
      dbfile.name = "ERA5.reanalysis",
      stringsAsFactors = FALSE
    )

    years %>%
      purrr::walk(function(year) {
        year_data <- cf_data[year %>% as.character]
        if(nrow(year_data) == 0) {
          return(NULL)
        }
        identifier_file <- paste("ERA5", "reanalysis", year, sep = ".")
        flname <- file.path(reanalysis_folder, paste(identifier_file, "nc", sep = "."))
        if(!file.exists(flname) || overwrite) {
          tryCatch({
            time_vals <- as.numeric(zoo::index(year_data))
            time_dim <- ncdf4::ncdim_def(
              name = "time",
              units = "seconds since 1970-01-01 00:00:00",
              vals = time_vals,
              create_dimvar = TRUE
            )
            lat_dim <- ncdf4::ncdim_def("latitude", "degree_north", slat, create_dimvar = TRUE)
            lon_dim <- ncdf4::ncdim_def("longitude", "degree_east", slon, create_dimvar = TRUE)
            nc_vars <- purrr::map2(cf_var_names, cf_var_units,
                                    ~ ncdf4::ncvar_def(.x, .y, list(time_dim, lat_dim, lon_dim), 
                                                      missval = NA_real_))
            nc_flptr <- ncdf4::nc_create(flname, nc_vars, verbose = FALSE)

            for(j in seq_along(cf_var_names)) {
              ncdf4::ncvar_put(nc_flptr, nc_vars[[j]], 
                               zoo::coredata(year_data)[, j])
            }
            ncdf4::nc_close(nc_flptr)
          }, error = function(e) {
            PEcAn.logger::logger.severe(paste0("Error writing NetCDF file for year ", year, ": ", conditionMessage(e)))
          })
        }
      })

    year <- years[1]
    identifier.file <- paste("ERA5", "reanalysis", year, sep = ".")
    results$file <- file.path(reanalysis_folder, paste(identifier.file, "nc", sep = "."))
    return(results)
  }, error = function(e) {
    PEcAn.logger::logger.severe(paste0("Error in met2CF.ERA5.rea: ", conditionMessage(e)))
    return(NULL)
  })
}