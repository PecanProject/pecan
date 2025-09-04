#' Generate AmeriFlux meteorological ensembles
#'
#' Downloads AmeriFlux data, applies ERA5 fallback for missing radiation and soil moisture,
#' performs gap-filling, and generates ensembles.
#' This function provides a complete pipeline from raw AmeriFlux data to CF-compliant
#' ensemble meteorological files.
#'
#' @param site_id character. AmeriFlux site identifier (e.g, "US-Ha1")
#' The 'SITE_ID' field in \href{http://ameriflux.lbl.gov/sites/site-list-and-pages/}{list of Ameriflux sites}
#' @param start_date character or date. Start date in "YYYY-MM-DD" format
#' @param end_date character or date. End date in "YYYY-MM-DD" format
#' @param outfolder character. Output directory path for ensemble files
#' @param ameriflux_username character. AmeriFlux username for data access.
#' @param ameriflux_useremail character. Email address for AmeriFlux authentication (must contain "@")
#' @param overwrite logical. Whether to overwrite existing files. Default: FALSE
#' @param verbose logical. Whether to print detailed logs.  Default: FALSE
#' @param format data frame or List. format specifications for CF conversion. Default: NULL
#' The AmerifluxLBL format is Bety record 5000000002.
#' which could be returned from PEcAn.DB::query.format.vars(format.id=5000000002, bety = con)
#' @param n_ens integer. Number of ensemble members to generate. Default: 10
#' @param w_len integer. Window length in days. Default: 20
#' @param era5_user character. CDS user ID (UID) from your CDS profile. Required for authentication.
#' @param era5_key character. CDS API key from your CDS profile. Required for authentication.
#' @param threshold numeric. Coverage threshold (0-1) for triggering ERA5 fallback. Default: 0.5
#' @param dirs named list. Optional configuration for existing data directory paths. Default: NULL
#' @param ... additional arguments passed to download.AmerifluxLBL
#' @return A data frame with the paths to the generated ensemble files and their metadata.
#' @examples
#' \dontrun{
#' result <- AmeriFlux_met_process(
#'   site_id = "US-Ha1", 
#'   start_date = "2010-01-01",
#'   end_date = "2010-12-31",
#'   outfolder = "/path/to/output",
#'   ameriflux_username = "your_username",
#'   ameriflux_useremail = "your.email@domain.com", 
#'   format = format,
#'   era5_user = "your_cds_user",
#'   era5_key = "your_cds_api_key",
#'   n_ens = 5,
#'   verbose = TRUE
#' )
#' }
#'
#' @author Akash
#' @export

AmeriFlux_met_ensemble <- function(site_id,
                                   start_date,
                                   end_date, 
                                   outfolder,
                                   ameriflux_username = "pecan", 
                                   ameriflux_useremail = "@",
                                   overwrite = FALSE, 
                                   verbose = FALSE, 
                                   format = NULL,
                                   n_ens = 10, 
                                   w_len = 30,
                                   era5_user = NULL,
                                   era5_key = NULL,
                                   threshold = 0.5,
                                   dirs = NULL,
                                   ...) {
  
  # input validation
  if (!grepl("@", ameriflux_useremail)) {
    PEcAn.logger::logger.severe("ameriflux_useremail must contain '@' for AmeriFlux authentication")
  }
  
  if (!dir.exists(outfolder)) {
    dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  }
  # setup directory structure
  default_paths <- list(
    amf_downloads = file.path(outfolder, "amf_downloads"),
    amf_extracted = file.path(outfolder, "amf_extracted"),
    amf_cf = file.path(outfolder, "amf_cf"),
    amf_gapfilled = file.path(outfolder, "amf_gapfilled"),
    era5_downloads = file.path(outfolder, "era5_downloads"),
    era5_cf = file.path(outfolder, "era5_cf"),
    ensembles = file.path(outfolder, "ensembles")
  )
  
  if (!is.null(dirs)) {
    dirs <- utils::modifyList(default_paths, dirs)
  } else {
    dirs <- default_paths
  }
  
  sapply(dirs, function(x) {
    if (!is.null(x) && !dir.exists(x)) {
      dir.create(x, showWarnings = FALSE, recursive = TRUE)
    }
  })
  
  tryCatch({
    # check for existing AmeriFlux data using site_id pattern
    amf_pattern <- paste0("^AMF_", site_id, "_.*\\.csv$")
    amf_files <- list.files(dirs$amf_downloads, pattern = amf_pattern, full.names = TRUE)
    
    if (!overwrite && length(amf_files) > 0) {
      if(verbose) {
        PEcAn.logger::logger.info(paste("Found existing AmeriFlux file:", basename(amf_files[1]), "- using existing data"))
      }
      csv_file <- amf_files[1]
    } else {
      if(verbose) {
        PEcAn.logger::logger.info(paste("Downloading AmeriFlux data for site", site_id))
      }
      download_results <- 
        PEcAn.data.atmosphere::download.AmerifluxLBL(
          sitename = site_id,
          outfolder = dirs$amf_downloads,
          start_date = start_date,
          end_date = end_date,
          ameriflux_username = ameriflux_username,
          ameriflux_useremail = ameriflux_useremail,
          overwrite = overwrite,
          verbose = verbose,
          ...
        )
      csv_file <- download_results$file
    }
    
    # extract state variables
    if(verbose) {
      PEcAn.logger::logger.info("Extracting state variables")
    }
    flux_data <- utils::read.csv(
      csv_file,
      skip = 2,
      na.strings = c("-9999", "NA"),
      stringsAsFactors = FALSE
    )
    
    # variable patterns
    input_names <- list(
      datetime = c("^TIMESTAMP_START$", "^TIMESTAMP_END$"),
      air_temp = c("^TA_", "^T_SONIC$"),
      soil_temp = c("^TS_"),
      soil_moisture = c("^SWC_"),
      humidity = c("^RH_"),
      pressure = c("^PA$"),
      wind = c("^WS$", "^WD$"),
      precip = c("^P$"),
      radiation = c("^SW_IN", "^Rg", "^PPFD_IN", "^PAR")
    )
    selected_cols <- unique(unlist(sapply(input_names, function(p) {
      unlist(sapply(p, function(x) grep(x, names(flux_data), value = TRUE)))
    })))
    state_vars <- flux_data[, selected_cols, drop = FALSE]
    extracted_file <- file.path(dirs$amf_extracted, paste0(site_id, "_state_drivers.csv"))
    utils::write.csv(
      state_vars,
      extracted_file,
      row.names = FALSE,
      na = "NA"
    )
    
    # prepare CF conversion
    site_info <- amerifluxr::amf_site_info()
    format$lat <- site_info$LOCATION_LAT[site_info$SITE_ID == site_id]
    format$lon <- site_info$LOCATION_LONG[site_info$SITE_ID == site_id]
    format$skip <- 0 # No header lines in extracted ameriflux csv 
    
    # convert to CF format
    if(verbose) {   
      PEcAn.logger::logger.info("Converting to CF format")
    }
    cf_results <- 
      PEcAn.data.atmosphere::met2CF.AmerifluxLBL(
        in.path = dirs$amf_extracted,
        in.prefix = tools::file_path_sans_ext(basename(extracted_file)),
        outfolder = dirs$amf_cf,
        start_date = start_date,
        end_date = end_date,
        format = format,
        overwrite = overwrite
      )
    
    # ERA5 fallback 
    if (verbose) PEcAn.logger::logger.info("Checking data coverage for ERA5 fallback")
    # check coverage of radiation variables (PAR and Rg) needed for metgapfill
    nc <- ncdf4::nc_open(cf_results$file)
    time_dim <- ncdf4::ncvar_get(nc, "time")
    n_total <- length(time_dim)
    # check shortwave radiation(Rg) coverage
    has_rg <- "surface_downwelling_shortwave_flux_in_air" %in% names(nc$var)
    rg_coverage <- 0
    if (has_rg) {
      rg_data <- ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")
      rg_coverage <- sum(!is.na(rg_data)) / n_total
    }
    # check PAR coverage
    has_par <- "surface_downwelling_photosynthetic_photon_flux_in_air" %in% names(nc$var)
    par_coverage <- 0
    if (has_par) {
      par_data <- ncdf4::ncvar_get(nc, "surface_downwelling_photosynthetic_photon_flux_in_air")
      par_coverage <- sum(!is.na(par_data)) / n_total
    }
    # check soil moisture coverage since this is not filled by metgapfill
    has_swc <- "volume_fraction_of_condensed_water_in_soil" %in% names(nc$var)
    swc_coverage <- 0
    if (has_swc) {
      swc_data <- ncdf4::ncvar_get(nc, "volume_fraction_of_condensed_water_in_soil")
      swc_coverage <- sum(!is.na(swc_data)) / n_total
    }
    ncdf4::nc_close(nc)
    
    if(verbose) {
      PEcAn.logger::logger.info(paste("Shortwave radiation (Rg) coverage:", round(rg_coverage * 100, 1), "%"))
      PEcAn.logger::logger.info(paste("PAR coverage:", round(par_coverage * 100, 1), "%"))
      PEcAn.logger::logger.info(paste("Soil moisture coverage:", round(swc_coverage * 100, 1), "%"))
    }
    
    fill_vars <- c()
    # if BOTH PAR and Rg have insufficient coverage
    if ((!has_rg || rg_coverage < threshold) && 
        (!has_par || par_coverage < threshold)) {
      fill_vars <- c(fill_vars, "surface_solar_radiation_downwards")
      if(verbose) {
        PEcAn.logger::logger.info("Adding shortwave radiation to ERA5 fallback (insufficient PAR and Rg coverage)")
      }
    }
    # if variable exists but has ANY missing values
    if (has_swc && swc_coverage < 1.0) {
      fill_vars <- c(fill_vars, "volumetric_soil_water_layer_1")
      if(verbose) {
        PEcAn.logger::logger.info("Adding soil moisture to ERA5 fallback (missing data detected)")
      }
    }
    if (length(fill_vars) > 0) {
      start_year <- lubridate::year(as.Date(start_date))
      end_year <- lubridate::year(as.Date(end_date))
      req_years <- start_year:end_year
      
      # find existing ERA5 files
      era5_files <- list.files(dirs$era5_downloads, pattern = "^ERA5_\\d{4}\\.nc$", full.names = TRUE)
      exist_years <- as.numeric(gsub(".*ERA5_(\\d{4})\\.nc", "\\1", basename(era5_files)))
      
      # check which years need download
      dl_years <- c()
      if (overwrite) {
        dl_years <- req_years
      } else {
        dl_years <- req_years[!req_years %in% exist_years]
        era5_var_map <- list(
          "surface_solar_radiation_downwards" = "ssrd",
          "volumetric_soil_water_layer_1" = "swvl1"
        )
        for (f in era5_files) {
          year <- as.numeric(gsub(".*ERA5_(\\d{4})\\.nc", "\\1", basename(f)))
          if (year %in% req_years) {  
            tryCatch({
              nc <- ncdf4::nc_open(f)
              avail_vars <- names(nc$var)
              ncdf4::nc_close(nc)
              req_vars <- sapply(fill_vars, function(v) era5_var_map[[v]])
              miss_vars <- req_vars[!req_vars %in% avail_vars]
              if (length(miss_vars) > 0) {
                dl_years <- c(dl_years, year)
                if(verbose) {
                  PEcAn.logger::logger.info(paste("ERA5", year, "missing vars:", paste(miss_vars, collapse=", ")))
                }
              }
            }, error = function(e) {
              dl_years <<- c(dl_years, year)
              if(verbose) PEcAn.logger::logger.warn(paste("Cannot read ERA5", year, "- redownloading"))
            })
          }
        }
      }
      dl_years <- unique(dl_years)
      if (length(dl_years) == 0) {
        if(verbose) PEcAn.logger::logger.info("All ERA5 files exist with required variables")
      } else {
        if(verbose) {
          PEcAn.logger::logger.info(paste("Downloading ERA5 for years:", paste(sort(dl_years), collapse=", ")))
        }
        
        dl_start_date <- paste0(min(dl_years), "-01-01")
        dl_end_date <- paste0(max(dl_years), "-12-31")
        lat <- format$lat
        lon <- format$lon
        
        era5_files <- 
          PEcAn.data.atmosphere::download.ERA5_cds(
            outfolder = dirs$era5_downloads,
            start_date = dl_start_date,
            end_date = dl_end_date,
            extent = c(lon - 0.375, lon + 0.375, lat - 0.375, lat + 0.375), # 3*3 grid
            variables = fill_vars,
            product_type = "reanalysis",
            user = era5_user,
            key = era5_key
          )
      }
      if(verbose) {
        PEcAn.logger::logger.info("Processing ERA5 data to CF format")
      }
      era5_cf_dirs <- 
        PEcAn.data.atmosphere::extract.nc.ERA5(
          slat = format$lat,
          slon = format$lon,
          in.path = dirs$era5_downloads,
          start_date = start_date, 
          end_date = end_date,    
          outfolder = dirs$era5_cf,
          in.prefix = "ERA5_",
          newsite = site_id,
          overwrite = TRUE,
          verbose = verbose
        )  
      # merge ERA5 data with AmeriFlux CF file
      if(verbose) {
        PEcAn.logger::logger.info("Merging ERA5 data with AmeriFlux data")
      }
      era5_cf_file <- list.files(era5_cf_dirs[[1]], pattern = "\\.nc$", full.names = TRUE)[1]
      # variable mapping from ERA5 to CF names
      era5_map <- list(
        "surface_solar_radiation_downwards" = "surface_downwelling_shortwave_flux_in_air",
        "volumetric_soil_water_layer_1" = "volume_fraction_of_condensed_water_in_soil"
      )
      
      nc_amf <- ncdf4::nc_open(cf_results$file, write = TRUE)
      nc_era5 <- ncdf4::nc_open(era5_cf_file)
      tryCatch({
        amf_time <- ncdf4::ncvar_get(nc_amf, "time")
        era5_time <- ncdf4::ncvar_get(nc_era5, "time") 
        # convert AmeriFlux time (days since 1700-01-01) to seconds since 1970-01-01
        amf_time_sec <- as.numeric(as.POSIXct(amf_time * 86400, origin = "1700-01-01", tz = "UTC"))
        for (era5_var in fill_vars) {
          cf_var <- era5_map[[era5_var]]
          if (!cf_var %in% names(nc_era5$var)) {
            if(verbose) PEcAn.logger::logger.warn(paste("ERA5 variable not found:", cf_var))
            next
          }
          era5_data <- ncdf4::ncvar_get(nc_era5, cf_var)
          if (cf_var %in% names(nc_amf$var)) {
            amf_data <- ncdf4::ncvar_get(nc_amf, cf_var)
            
            na_idx <- which(is.na(amf_data))
            if (length(na_idx) > 0) {
              era5_interp <- stats::approx(era5_time, era5_data, 
                                           xout = amf_time_sec[na_idx], 
                                           rule = 2)$y
              
              # fill missing values with interpolated ERA5 data
              amf_data[na_idx] <- era5_interp
              ncdf4::ncvar_put(nc_amf, cf_var, amf_data)
              
              if(verbose) {
                filled_count <- length(na_idx)
                PEcAn.logger::logger.info(paste("Filled", filled_count, "missing values for", cf_var, "using ERA5 data"))
                PEcAn.logger::logger.info(paste("Interpolated range:", paste(range(era5_interp, na.rm=TRUE), collapse=" to ")))
              }
            }
          } else {
            if(verbose) PEcAn.logger::logger.info(paste("Adding new variable from ERA5:", cf_var))
            lat_dim <- nc_amf$dim$latitude
            lon_dim <- nc_amf$dim$longitude
            time_dim <- nc_amf$dim$time
            var_units <- nc_era5$var[[cf_var]]$units
            new_var <- ncdf4::ncvar_def(name = cf_var, units = var_units, 
                                        dim = list(lon_dim, lat_dim, time_dim), 
                                        missval = -999)
            
            nc_amf <- ncdf4::ncvar_add(nc_amf, new_var)
            era5_interp <- stats::approx(era5_time, era5_data, 
                                         xout = amf_time_sec, 
                                         rule = 2)$y
            ncdf4::ncvar_put(nc_amf, cf_var, era5_interp)
            
            if(verbose) {
              PEcAn.logger::logger.info(paste("Added complete", cf_var, "variable from ERA5 data"))
              PEcAn.logger::logger.info(paste("Added data range:", paste(range(era5_interp, na.rm=TRUE), collapse=" to ")))
            }
          }
        }
      }, finally = {
        ncdf4::nc_close(nc_amf)
        ncdf4::nc_close(nc_era5)
      })
    }
    
    # gap filling
    if(verbose) {
      PEcAn.logger::logger.info("Running gap filling")
    }
    gapfill_results <- 
      PEcAn.data.atmosphere::metgapfill(
        in.path = dirs$amf_cf,
        in.prefix = sub("\\.\\d+$", "", tools::file_path_sans_ext(basename(cf_results$file))),
        outfolder = dirs$amf_gapfilled,
        start_date = start_date,
        end_date = end_date,
        overwrite = overwrite
      )
    
    tryCatch({
      # remove extra variables from gapfilled file that are not in CF file
      gapfill_file <- gapfill_results$file
      nc_cf <- ncdf4::nc_open(cf_results$file)
      cf_vars <- names(nc_cf$var)
      ncdf4::nc_close(nc_cf)
      nc_gap <- ncdf4::nc_open(gapfill_file)
      gap_vars <- names(nc_gap$var)
      extra_vars <- setdiff(gap_vars, cf_vars)
      ncdf4::nc_close(nc_gap)
      
      if (length(extra_vars) > 0) {
        if (verbose) {
          PEcAn.logger::logger.info(paste("removing variables from gapfill file:",
                                          paste(extra_vars, collapse = ", ")))
        }
        temp_file <- tempfile(tmpdir = dirs$amf_gapfilled, fileext = ".nc")
        nc_in <- ncdf4::nc_open(gapfill_file)
        nc_out <- ncdf4::nc_create(
          temp_file,
          vars = nc_in$var[setdiff(names(nc_in$var), extra_vars)],
          force_v4 = TRUE
        )
        global_atts <- ncdf4::ncatt_get(nc_in, 0)
        for (att in names(global_atts)) {
          ncdf4::ncatt_put(nc_out, 0, att, global_atts[[att]])
        }
        for (dim in names(nc_in$dim)) {
          if (!dim %in% names(nc_out$dim)) {
            ncdf4::ncvar_add(nc_out, nc_in$dim[[dim]])
          }
        }
        for (v in names(nc_out$var)) {
          data <- ncdf4::ncvar_get(nc_in, v)
          ncdf4::ncvar_put(nc_out, v, data)
          var_atts <- ncdf4::ncatt_get(nc_in, v)
          for (att in names(var_atts)) {
            ncdf4::ncatt_put(nc_out, v, att, var_atts[[att]])
          }
        }
        ncdf4::nc_close(nc_in)
        ncdf4::nc_close(nc_out)
        file.remove(gapfill_file)
        file.rename(temp_file, gapfill_file)
      }
    }, error = function(e) {
      if (file.exists(temp_file)) file.remove(temp_file)
      PEcAn.logger::logger.severe("variable filtering failed:", e$message)
    })
    
    # generate ensembles
    if(verbose) {
      PEcAn.logger::logger.info(paste("Generating", n_ens, "ensemble members"))
    }
    ensemble_results <- 
      PEcAn.data.atmosphere::met_temporal_downscale.Gaussian_ensemble(
        in.path = dirs$amf_gapfilled,
        in.prefix = sub("\\.\\d+$", "", tools::file_path_sans_ext(basename(gapfill_results$file))),
        outfolder = dirs$ensembles,
        input_met = gapfill_file,
        train_met = gapfill_file,
        overwrite = overwrite,
        verbose = verbose,
        n_ens = n_ens,
        w_len = w_len,
        force_v4 = TRUE
      )
    
    # return ensemble paths with metadata
    results <- do.call(rbind, lapply(seq_along(ensemble_results), function(e) {
      data.frame(
        file = ensemble_results[[e]]$file,
        host = rep(PEcAn.remote::fqdn(), 1),
        mimetype = "application/x-netcdf",
        formatname = "CF Meteorology",
        startdate = format(as.Date(start_date), "%Y-01-01 00:00:00"),
        enddate = format(as.Date(end_date), "%Y-12-31 23:59:59"),
        dbfile.name = paste0(site_id, ".AmeriFlux.ens", e),
        stringsAsFactors = FALSE
      )
    }))
    if(verbose) PEcAn.logger::logger.info("Processing complete")
    return(results)
    
  }, error = function(e) {
    PEcAn.logger::logger.severe("Processing failed: ", e$message)
    return(NULL)
  })
}