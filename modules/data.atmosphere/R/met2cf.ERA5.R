#' met2cf.ERA5
#'
#' @param lat latitude
#' @param long longitude
#' @param start_date start date
#' @param end_date end date
#' @param sitename The name of the site used for making the identifier.
#' @param outfolder Path to directory where nc files need to be saved.
#' @param out.xts Output of the extract.nc.ERA5 function which is a list of time series of met variables for each ensemble member
#'   or single reanalysis dataset.
#' @param overwrite Logical if files needs to be overwritten.
#' @param verbose Logical flag defining if ouput of function be extra verbose.
#' @param ens_size Number of ensemble members to process. Default is 1.

#'
#' @return list of dataframes
#' @importFrom rlang .data
#' @export
#' @author Hamze Dokohaki, Akash
met2CF.ERA5<- function(lat,
                       long,
                       start_date,
                       end_date,
                       sitename,
                       outfolder,
                       out.xts,
                       overwrite = FALSE,
                       verbose = TRUE,
                       ens_size = 1) {
  
  years <- seq(lubridate::year(start_date),
               lubridate::year(end_date),
               1
  )
  
  ensemblesN <- seq(1, ens_size)
  
  start_date <- paste0(lubridate::year(start_date),"-01-01")  %>% as.Date()
  end_date <- paste0(lubridate::year(end_date),"-12-31") %>% as.Date()

  era5_tbl <- pecan_standard_met_table %>%
    dplyr::filter(!is.na(.data$era5) & nzchar(.data$era5))
  era5_to_cf <- stats::setNames(era5_tbl$cf_standard_name, era5_tbl$era5)
  cf_units_map <- stats::setNames(era5_tbl$units, era5_tbl$cf_standard_name)
  
  out.new <- ensemblesN %>%
    purrr::map(function(ensi) {
      tryCatch({
        ens <- out.xts[[ensi]]
        if (is.null(ens) || nrow(ens) == 0) {
          PEcAn.logger::logger.warn(paste("Empty ensemble", ensi))
          return(NULL)
        }
        
        available_vars <- colnames(ens)
        native_vars <- intersect(names(era5_to_cf), available_vars)
        if (!length(native_vars)) {
          PEcAn.logger::logger.warn("No mappable ERA5 vars in ensemble member.")
          return(NULL)
        }
        # detect timestep dynamically
        time_diffs <- diff(as.numeric(zoo::index(ens)))
        if (length(time_diffs) > 0) {
          timestep_seconds <- as.numeric(stats::median(time_diffs))
        } else {
          timestep_seconds <- 3 * 3600 # fallback to 3-hourly
          if (verbose) PEcAn.logger::logger.info("Only one timestamp found. Defaulting to 3-hour timestep for conversion.")
        }
        timestep_hours <- timestep_seconds / 3600
        # solar radiation conversions - J/m2 to W/m2
        if ("ssrd" %in% available_vars) {
          ens[, "ssrd"] <- as.numeric(ens[, "ssrd"]) / timestep_seconds
        }
        if ("strd" %in% available_vars) {
          ens[, "strd"] <- as.numeric(ens[, "strd"]) / timestep_seconds
        }
        # precipitation - m to kg/m2/s
        if ("tp" %in% available_vars) {
          ens[, "tp"] <- (as.numeric(ens[, "tp"]) * 1000) / timestep_seconds
        }

        # relative and specific humidity (only if all required vars present)
        specific_humidity <- NULL
        if (all(c("t2m", "d2m", "sp") %in% available_vars)) {
          # Vectorized RH via Magnus formula over water (Kelvin inputs)
          T_k  <- as.numeric(ens[, "t2m"])   # K
          Td_k <- as.numeric(ens[, "d2m"])   # K
          T_c  <- T_k  - 273.15
          Td_c <- Td_k - 273.15
          es <- 6.112 * exp((17.62 * T_c)  / (243.12 + T_c))    # hPa
          e  <- 6.112 * exp((17.62 * Td_c) / (243.12 + Td_c))   # hPa
          rh_prop <- pmin(pmax(e / es, 0), 1)                   # [0,1]
          
          specific_humidity <- PEcAn.data.atmosphere::rh2qair(rh_prop, T_k, as.numeric(ens[, "sp"]))
        }
        
        # select available ERA5 variables and convert to CF naming
        available_era5_vars <- intersect(names(era5_to_cf), available_vars)
        ens_cf <- ens[, available_era5_vars, drop = FALSE]
        colnames(ens_cf) <- era5_to_cf[available_era5_vars]
        if (!is.null(specific_humidity)) {
          specific_humidity_xts <- xts::xts(specific_humidity, order.by = zoo::index(ens))
          colnames(specific_humidity_xts) <- "specific_humidity"
          ens_cf <- xts::merge.xts(ens_cf, specific_humidity_xts)
        }
        # attach timestep as attribute for downstream use
        attr(ens_cf, "timestep_hours") <- timestep_hours
        return(ens_cf)
        
      },
      error = function(e) {
        PEcAn.logger::logger.severe("Something went wrong during the unit conversion in met2cf ERA5.",
                                    conditionMessage(e))
      })
      
    })
  
  # filter out NULL results from failed ensembles
  out.new <- out.new[!sapply(out.new, is.null)]
  if (length(out.new) == 0) {
    PEcAn.logger::logger.severe("No valid ensembles processed")
    return(NULL)
  }
  
  cf_var_names <- colnames(out.new[[1]])
  cf_var_units <- purrr::map_chr(cf_var_names, function(nm) {
    u <- unname(cf_units_map[nm])
    if (length(u) == 0 || is.na(u)) {
      if (identical(nm, "specific_humidity")) return("1")  # unitless (mass ratio)
      return(NA_character_)
    }
    as.character(u)
  })
  names(cf_var_units) <- cf_var_names
  
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
      
      if (ens_size > 1) {
        identifier <- paste("ERA5", sitename, i, sep = "_") 
      } else {
        identifier <- paste("ERA5", sitename, "Mean", sep = "_")
      }
      identifier.file <- paste("ERA5", i, lubridate::year(start_date), sep = ".")
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
          identifier.file <- paste("ERA5", i, year, sep = ".")
          
          flname <-file.path(ensemble_folder, paste(identifier.file, "nc", sep = "."))
          # Spliting it for this year
          data.for.this.year.ens <- out.new[[i]]
          data.for.this.year.ens <- data.for.this.year.ens[year %>% as.character]
          
          if (nrow(data.for.this.year.ens) == 0) return(NULL)
          
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
          
          if (!file.exists(flname) || overwrite) {
            tryCatch({
              nc_flptr <- ncdf4::nc_create(flname, nc_var_list, verbose = verbose)
              
              #For each variable associated with that ensemble
              for (j in seq_along(cf_var_names)) {
                # "j" is the variable number.  "i" is the ensemble number.
                ncdf4::ncvar_put(nc_flptr,
                                 nc_var_list[[j]],
                                 zoo::coredata(data.for.this.year.ens)[, nc_var_list[[j]]$name])
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