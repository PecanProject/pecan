#' ERA5_extract
#'
#' @param slat numeric: vector of latitudes.
#' @param slon numeric: vector of longitudes.
#' @param in.path character: path to the directory containing the file to be inserted
#' @param start_date character: start date (in YYYY-MM-DD format).
#' @param end_date character: end date (in YYYY-MM-DD format).
#' @param outfolder character: Path to directory where nc files need to be saved.
#' @param in.prefix character: initial portion of the filename that does not vary by date.
#'  Does not include directory; specify that as part of in.path.
#' @param newsite character: vector of site names. 
#'  The length should match with that of slat and slon.
#' @param ncores numeric: the number of CPUs for the parallel compute. Default is 1.
#' @param vars character: names of variables to be extracted. If NULL all the variables will be
#'  returned. Default is NULL.
#' @param overwrite Logical if files needs to be overwritten.
#' @param verbose Decide if we want to stop printing info.
#' @param ... other inputs.
#' @details For the list of variables check out the documentation at
#'  \url{https://confluence.ecmwf.int/display/CKB/ERA5\%3A+data+documentation}
#'
#' @return a list of xts objects with all the variables for the requested years
#' @export
#' @examples
#' \dontrun{
#' point.data <- extract.nc.ERA5(
#'   slat = 43.25,
#'   slon = -83.25,
#'   in.path = "path/to/era5/files",
#'   start_date = "1990-01-01",
#'   end_date = "1995-12-31",
#'   outfolder = "path/to/output",
#'   in.prefix = "ERA5_",
#'   newsite = "my_site",
#'   vars = NULL,
#'   overwrite = FALSE,
#'   verbose = TRUE
#' )
#'
#' }
#' @author Dongchen Zhang, Akash
#' @importFrom dplyr %>%
#' @importFrom foreach %dopar%

extract.nc.ERA5 <-
  function(slat,
           slon,
           in.path,
           start_date,
           end_date,
           outfolder,
           in.prefix,
           newsite,
           ncores = 1,
           vars = NULL,
           overwrite = FALSE,
           verbose = FALSE,
           ...) {
    
    
    years <- seq(lubridate::year(start_date),
                 lubridate::year(end_date),
                 1
    )
    sample_file <- file.path(in.path, paste0(in.prefix, years[1], ".nc"))
    if (!file.exists(sample_file)) {
      PEcAn.logger::logger.severe(paste0("ERA5 input file not found: ", sample_file,
                                         ". please check the input path and file prefix."))
    }
    
    # Determine data type (ensemble vs reanalysis)
    nc_test <- ncdf4::nc_open(sample_file)
    # initialize variables
    is_ensemble <- FALSE
    ens_size <- 1
    if ("number" %in% names(nc_test$dim)) {
      is_ensemble <- TRUE
      ens_size <- nc_test$dim$number$len
      if (verbose) PEcAn.logger::logger.info(paste0("detected new ERA5 format with ", ens_size, " ensemble members"))
    } else if (any(sapply(nc_test$var, function(v) v$ndims == 4))) {
      is_ensemble <- TRUE
      # new ERA5 ens format [longitude, latitude, valid_time/time, number]
      var_4d <- names(nc_test$var)[sapply(nc_test$var, function(v) v$ndims == 4)][1]
      ens_size <- nc_test$var[[var_4d]]$size[4]
      if (verbose) PEcAn.logger::logger.info(paste0("detected new ERA5 format with ", ens_size, " ensemble members"))
    } else {
      # old ERA5 ens format [longitude, latitude, time*ens]
      var_3d <- names(nc_test$var)[sapply(nc_test$var, function(v) v$ndims == 3)][1]
      if (!is.na(var_3d) && !is.null(var_3d)) {
        tryCatch({
          # Check if time dimension exists
          if ("time" %in% names(nc_test$dim) && !is.null(nc_test$dim$time$len)) {
            test_brick <- raster::brick(sample_file, varname = var_3d)
            total_layers <- raster::nlayers(test_brick)
            time_size <- nc_test$dim$time$len

            if (!is.na(total_layers) && !is.na(time_size) && 
                total_layers > time_size && total_layers %% time_size == 0) {
              is_ensemble <- TRUE
              ens_size <- total_layers / time_size
              if (verbose) PEcAn.logger::logger.info(paste0("detected old ERA5 format with ", ens_size, " ensemble members"))
            }
          }
        }, error = function(e) {
          if (verbose) PEcAn.logger::logger.debug(paste("Error during format detection:", e$message))
        })
      }
    }
    if (!is_ensemble && verbose) {
      PEcAn.logger::logger.info("processing ERA5 reanalysis data")
    }
    
    ensemblesN <- if (is_ensemble) seq(1, ens_size) else 1
    ncdf4::nc_close(nc_test)
    
    # initialize parallel.
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    doSNOW::registerDoSNOW(cl)
    
    # initialize progress bar.
    pb <- utils::txtProgressBar(min=0, max=length(slat), style=3)
    on.exit(close(pb), add = TRUE)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress=progress)
    # Distributing the job between whatever core is available. 
    final.nc.files <- vector("list", length = length(years))
    for (i in seq_along(years)) {
      # report progress.
      PEcAn.logger::logger.info(paste0("\nProcessing year ", years[i], ".\n"))
      year <- years[i]
      year_start <- if (year == lubridate::year(start_date)) {
        start_date
      } else {
        paste0(year, "-01-01")
      }
      year_end <- if (year == lubridate::year(end_date)) {
        end_date
      } else {
        paste0(year, "-12-31")
      }
      ncfile <- file.path(in.path, paste0(in.prefix, year, ".nc"))
      # open the file
      nc_data <- ncdf4::nc_open(ncfile)
      time_var <- if ("time" %in% names(nc_data$dim)) "time" else "valid_time"
      t <- ncdf4::ncvar_get(nc_data, time_var)
      tunits <- ncdf4::ncatt_get(nc_data, time_var)
      tustr <- strsplit(tunits$units, " since ")
      
      # handle different time units: 'time' uses hours, 'valid_time' uses seconds
      if (time_var == "time") {
        # traditional format: "hours since YYYY-MM-DD HH:MM:SS"
        timestamp <- as.POSIXct(t * 3600, tz = "UTC", origin = tustr[[1]][2])
      } else {
        # new format: "seconds since YYYY-MM-DD HH:MM:SS" (typically 1970-01-01)
        timestamp <- as.POSIXct(t, tz = "UTC", origin = tustr[[1]][2])
      }
      
      # set the vars - filter for valid variables
      if (is.null(vars)) {
        all_vars <- names(nc_data$var)
        if (is_ensemble) {
          # for ensemble data, keep variables with 4 dimensions (lon, lat, time, ensemble)
          vars <- all_vars[sapply(all_vars, function(v) {
            var_info <- nc_data$var[[v]]
            var_info$ndims == 4 && 
              var_info$prec %in% c("float", "double", "integer", "short") &&
              !v %in% c("expver") 
          })]
        } else {
          # for reanalysis data, keep variables with 3 dimensions (lon, lat, time)
          vars <- all_vars[sapply(all_vars, function(v) {
            var_info <- nc_data$var[[v]]
            var_info$ndims == 3 && 
              var_info$prec %in% c("float", "double", "integer") &&
              !v %in% c("longitude", "latitude", "time", "valid_time")
          })]
        }
        if (verbose && length(vars) < length(all_vars)) {
          skipped <- setdiff(all_vars, vars)
          PEcAn.logger::logger.info(paste0("Processing variables: ", paste(vars, collapse=", ")))
          PEcAn.logger::logger.info(paste0("Skipped metadata variables: ", paste(skipped, collapse=", ")))
        }
      }
      ncdf4::nc_close(nc_data) 
      
      # for the variables extract the data
      if (verbose) {
        PEcAn.logger::logger.info("Extracting NC file.\n")
      }
      vname <- NULL
      all.data.point <- 
        foreach::foreach(vname = vars, 
                         .packages=c("Kendall", "ncdf4")) %dopar% {
                           nc_data <- ncdf4::nc_open(ncfile)
                           on.exit(ncdf4::nc_close(nc_data), add = TRUE) 
                           ens.out <- vector("list", length = length(ensemblesN))
                           for (ens in ensemblesN) {
                             if (is_ensemble) {
                               brick.tmp <-
                                 raster::brick(ncfile, varname = vname, level = ens)
                             } else {
                               # Direct brick creation for reanalysis
                               brick.tmp <- 
                                 raster::brick(ncfile, varname = vname)
                             }
                             raster::setZ(brick.tmp, timestamp)
                             nn <-
                               raster::extract(brick.tmp,
                                               sp::SpatialPoints(cbind(slon, slat)),
                                               method = 'simple')
                             # replacing the missing/filled values with NA
                             nn[nn == nc_data$var[[vname]]$missval] <- NA
                             # send out the extracted var as a new col
                             ens.out[[ens]] <- t(nn)
                           }
                           ens.out
                         } %>% 
        purrr::set_names(vars)
      # progress bar.
      # TODO wrap into a large matrix (2928*8000*10 rows and 8 columns), and then split them into the foreach.
      if (verbose) {
        PEcAn.logger::logger.info("Converting multi-site time series to by-site data frames.\n")
      }
      pb <- utils::txtProgressBar(min = 0, max = length(slat), style = 3)
      all.site.data.point <- vector("list", length = length(slat))
      for (s.ind in seq_along(all.site.data.point)) {
        pbi <- s.ind
        utils::setTxtProgressBar(pb, pbi)
        all.site.data.point[[s.ind]] <- ensemblesN %>%
          purrr::map(function(ens) {
            s.all.data <- vars %>% 
              purrr::set_names(vars) %>% 
              purrr::map_dfc(function(vname){
                all.data.point[[vname]][[ens]][,s.ind]
              })
            s.all.data <- xts::xts(s.all.data, order.by = timestamp)
            s.all.data
          })
      }
      # Write into NC files.
      if (verbose) {
        PEcAn.logger::logger.info("Writing NC files.\n")
      }
      data.point <- NULL
      final.nc.files[[i]] <- 
        foreach::foreach(data.point = all.site.data.point, 
                         s.ind = seq_along(slat),
                         .packages=c("Kendall", "ncdf4", "PEcAn.data.atmosphere", "purrr", "xts", "lubridate"),
                         .options.snow=opts,
                         .export = c("met2CF.ERA5")) %dopar% {
                           # Calling the met2CF inside extract bc in met process met2CF comes before extract !
                           out <- met2CF.ERA5(
                             slat[s.ind],
                             slon[s.ind],
                             year_start,
                             year_end,
                             sitename=newsite[s.ind],
                             outfolder,
                             data.point,
                             overwrite = FALSE,
                             verbose = verbose,
                             ens_size = ens_size
                           )
                           out %>% purrr::map(~.x[['file']]) %>% unlist
                         }
    }
    # we only need the by-site ensemble folders for the met2model function.
    final.nc.files <- final.nc.files[[1]] %>% purrr::map(dirname)
    return(final.nc.files)
}