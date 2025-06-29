#' ERA5_extract
#'
#' @param slat latitude
#' @param slon longitude
#' @param in.path path to the directory containing the file to be inserted
#' @param start_date start date
#' @param end_date end date
#' @param outfolder Path to directory where nc files need to be saved.
#' @param in.prefix initial portion of the filename that does not vary by date.
#'  Does not include directory; specify that as part of in.path.
#' @param newsite site name.
#' @param vars variables to be extracted. If NULL all the variables will be
#'  returned.
#' @param overwrite Logical if files needs to be overwritten.
#' @param verbose Decide if we want to stop printing info.
#' @param ... other inputs.
#' @details For the list of variables check out the documentation at \url{
#'  https://confluence.ecmwf.int/display/CKB/ERA5+data+documentation#ERA5datadocumentation-Spatialgrid}
#'
#' @return a list of xts objects with all the variables for the requested years
#' @export
#' @examples
#' \dontrun{
#' point.data <- ERA5_extract(sslat=40, slon=-120, years=c(1990:1995), vars=NULL)
#' 
#  point.data %>% 
#'  purrr::map(~xts::apply.daily(.x, mean))
#'
#' }
extract.nc.ERA5 <-
  function(slat ,
           slon ,
           in.path ,
           start_date,
           end_date ,
           outfolder,
           in.prefix,
           newsite,
           vars = NULL,
           overwrite = FALSE,
           verbose = FALSE,
           ...) {
    
    # library(xts)
    # Distributing the job between whatever core is available. 
    
    # Ensure valid inputs
    if (is.na(slat) || is.na(slon)) PEcAn.logger.severe("Invalid latitude or longitude provided")

    years <- seq(lubridate::year(start_date),
                 lubridate::year(end_date),
                 1
    )
    ensemblesN <- seq(1, 10)
    
    
    tryCatch({
      #for each ensemble
      one.year.out <- years %>%
        purrr::map(function(year) {
          
          # for each year
          point.data <-  ensemblesN %>%
            purrr::map(function(ens) {
              
              
              ncfile <- file.path(in.path, paste0(in.prefix, year, ".nc"))
              
              #printing out initial information.
              if (verbose) {
                PEcAn.logger::logger.info(paste0("Trying to open :", ncfile, " "))
                
                if (!file.exists(ncfile))
                  PEcAn.logger::logger.severe("The nc file was not found.")
                
                #msg
                PEcAn.logger::logger.info(paste0(year, " is being processed ", "for ensemble #", ens, " "))
              }   
              #open the file
              nc_data <- ncdf4::nc_open(ncfile)
              # Ensure file closure
              on.exit(ncdf4::nc_close(nc_data), add = TRUE)
              # time stamp - handle both 'time' and 'valid_time' variables
              time_var <- if ("time" %in% names(nc_data$var)) "time" else "valid_time"
              t <- ncdf4::ncvar_get(nc_data, time_var)
              tunits <- ncdf4::ncatt_get(nc_data, time_var)
              tustr <- strsplit(tunits$units, " ")
              
              # Handle different time units: 'time' uses hours, 'valid_time' uses seconds
              if (time_var == "time") {
                # Traditional format: "hours since YYYY-MM-DD HH:MM:SS"
                timestamp <- as.POSIXct(t * 3600, tz = "UTC", origin = tustr[[1]][3])
              } else {
                # New format: "seconds since YYYY-MM-DD HH:MM:SS" (typically 1970-01-01)
                timestamp <- as.POSIXct(t, tz = "UTC", origin = tustr[[1]][3])
              }
              
              # Get spatial coordinates 
              lon <- ncdf4::ncvar_get(nc_data, "longitude")
              lat <- ncdf4::ncvar_get(nc_data, "latitude")
              lon_ix <- which.min(abs(lon - slon))
              lat_iy <- which.min(abs(lat - slat))
              
              # set the vars - filter for valid geophysical variables
              if (is.null(vars)) {
                all_vars <- names(nc_data$var)
                vars <- all_vars[sapply(all_vars, function(v) {
                  var_info <- nc_data$var[[v]]
                  var_info$ndims %in% c(3,4) && 
                    var_info$prec %in% c("float", "double", "integer")
                })]
                if (verbose && length(vars) < length(all_vars)) {
                  skipped <- setdiff(all_vars, vars)
                  PEcAn.logger::logger.info(paste0("Skipped metadata variables: ", paste(skipped, collapse=", ")))
                }
              }
              
              # for the variables extract the data
              all.data.point <- vars %>%
                purrr::set_names(vars) %>%
                purrr::map_dfc(function(vname) {
                  if (verbose) {
                    PEcAn.logger::logger.info(paste0(" \t ",vname, " is being extracted ! "))
                  }
                  
                  # Use direct ncvar_get for better performance
                  vals <- ncdf4::ncvar_get(nc_data, vname)
                  missval <- if (!is.null(nc_data$var[[vname]]$missval)) nc_data$var[[vname]]$missval else NA
                  
                  # Dynamic dimension handling
                  if (length(dim(vals)) == 4) {
                    # 4D: lon, lat, time, ensemble
                    nn <- vals[lon_ix, lat_iy, , ens]
                  } else if (length(dim(vals)) == 3) {
                    # 3D: lon, lat, time (no ensemble dimension)
                    nn <- vals[lon_ix, lat_iy, ]
                  } else {
                    PEcAn.logger::logger.severe(paste0("Unexpected dimension in variable ", vname))
                    nn <- rep(NA, length(timestamp))
                  }
                  
                  if (verbose) {
                    if (!is.numeric(nn)) {
                      PEcAn.logger::logger.severe(paste0(
                        "Expected raster object to be numeric, but it has type `",
                        paste0(typeof(nn), collapse = " "),
                        "`"
                      ))
                    }
                  }
                  # replacing the missing/filled values with NA
                  nn[nn == missval] <- NA
                  # send out the extracted var as a new col
                  nn
                })
              
              #close the connection - handled by on.exit

              # send out as xts object
              xts::xts(all.data.point, order.by = timestamp)
            }) %>%
            stats::setNames(paste0("ERA_ensemble_", ensemblesN))

          #Merge mean and spread
          return(point.data)
          
        }) %>%
        stats::setNames(years)
      
      # The order of one.year.out is year and then Ens - Mainly because of the spead  / I wanted to touch each file just once.
      # This now changes the order to ens - year
      point.data <- ensemblesN %>%
        purrr::map(function(Ensn) {
          rbind.xts <- do.call("::", list("xts", "rbind.xts"))
          one.year.out %>% 
            purrr::map( ~ .x [[Ensn]]) %>%
            do.call("rbind.xts", .)
        })
      
      # Calling the met2CF inside extract bc in met process met2CF comes before extract !
      out <-met2CF.ERA5(
        slat,
        slon,
        start_date,
        end_date,
        sitename=newsite,
        outfolder,
        point.data,
        overwrite = FALSE,
        verbose = verbose
      )
      return(out)
    }, error = function(e) {
      PEcAn.logger::logger.severe(paste0(conditionMessage(e)))
    })
    
  }