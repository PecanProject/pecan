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
    
    if (is.null(format)) {
      format <- list()
    }

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
    cf_results$file <- metgapfill_with_fallback(
      primary_cf         = cf_results$file,
      vars               = NULL,
      fallback_cf        = NULL,
      out_file           = file.path(
        dirs$amf_gapfilled,
        basename(cf_results$file)
      ),
      coverage_threshold = threshold,
      align_time         = FALSE
    )

    gapfill_file <- cf_results$file

    ensemble_results <- 
      PEcAn.data.atmosphere::met_temporal_downscale.Gaussian_ensemble(
        in.path = dirs$amf_gapfilled,
        in.prefix = sub("\\.\\d+$", "", tools::file_path_sans_ext(basename(gapfill_file))),
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