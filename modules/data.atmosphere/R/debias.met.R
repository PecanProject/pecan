substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

##' debias.met takes input_met and debiases it based on statistics from a train_met dataset
##' @name debias_met
##' @title debias_met
##' @export
##' @param outfolder
##' @param input_met - the source_met dataset that will be altered by the training dataset in NC format. 
##' @param train_met - the observed dataset that will be used to train the modeled dataset in NC format
##' @param de_method - select which debias method you would like to use, options are 'normal', 'linear regression'
##' @param site.id
##' @param overwrite
##' @param verbose
##' @author James Simkins
debias.met <- function(outfolder, input_met, train_met, site_id, de_method = "linear", 
                       overwrite = FALSE, verbose = FALSE, ...) {
  
  outfolder <- paste0(outfolder, "_site_", paste0(site_id%/%1e+09, "-", site_id%%1e+09))
  
  var <- data.frame(CF.name = c("air_temperature", "air_temperature_max", "air_temperature_min", 
                                "surface_downwelling_longwave_flux_in_air", "air_pressure", "surface_downwelling_shortwave_flux_in_air", 
                                "eastward_wind", "northward_wind", "specific_humidity", "precipitation_flux"), 
                    units = c("Kelvin", "Kelvin", "Kelvin", "W/m2", "Pascal", "W/m2", "m/s", 
                              "m/s", "g/g", "kg/m2/s"))
  
  sub_str <- substrRight(input_met, 7)
  year <- substr(sub_str, 1, 4)
  
  # Load in the data that will be used to train the source. Most of the time this
  # will be observed data.
  train <- list()
  tem <- ncdf4::nc_open(train_met)
  for (j in seq_along(var$CF.name)) {
    if (exists(as.character(var$CF.name[j]), tem$var) == FALSE) {
      train[[j]] <- NA
    } else {
      train[[j]] <- ncdf4::ncvar_get(tem, as.character(var$CF.name[j]))
    }
  }
  lat_train <- as.numeric(ncdf4::ncvar_get(tem, "latitude"))
  lon_train <- as.numeric(ncdf4::ncvar_get(tem, "longitude"))
  ncdf4::nc_close(tem)
  
  train <- data.frame(train)
  colnames(train) <- var$CF.name
  if (all(is.na(train$air_temperature_max))) {
    train$air_temperature_max <- train$air_temperature
  }
  if (all(is.na(train$air_temperature_min))) {
    train$air_temperature_min <- train$air_temperature
  }
  
  # Load the source dataset
  source <- list()
  tem <- ncdf4::nc_open(input_met)
  for (j in seq_along(var$CF.name)) {
    if (exists(as.character(var$CF.name[j]), tem$var) == FALSE) {
      source[[j]] = NA
    } else {
      source[[j]] <- ncdf4::ncvar_get(tem, as.character(var$CF.name[j]))
    }
  }
  year <- as.numeric(year)
  ncdf4::nc_close(tem)
  
  source <- data.frame(source)
  colnames(source) <- var$CF.name
  
  reso <- 24/(nrow(source)/365)
  reso_len <- nrow(source)
  # Grab the means/medians of the source and train, find the difference, and
  # correct the source dataset accordingly The following separate variables based
  # on properties so we can appropriately debias based on means/medians
  source_add <- data.frame(source$air_temperature, source$air_temperature_max, 
                           source$air_temperature_min, source$air_pressure, source$eastward_wind, source$northward_wind)
  source_mult <- data.frame(source$surface_downwelling_longwave_flux_in_air, source$surface_downwelling_shortwave_flux_in_air, 
                            source$specific_humidity, source$precipitation_flux)
  train_add <- data.frame(train$air_temperature, train$air_temperature_max, train$air_temperature_min, 
                          train$air_pressure, train$eastward_wind, train$northward_wind)
  train_mult <- data.frame(train$surface_downwelling_longwave_flux_in_air, train$surface_downwelling_shortwave_flux_in_air, 
                           train$specific_humidity, train$precipitation_flux)
  add_var <- c("air_temperature", "air_temperature_max", "air_temperature_min", 
               "air_pressure", "eastward_wind", "northward_wind")
  mult_var <- c("surface_downwelling_longwave_flux_in_air", "surface_downwelling_shortwave_flux_in_air", 
                "specific_humidity", "precipitation_flux")
  # These are for the linear regression argument, the for loop upscales the
  # training dataset to match the length of the source dataset because they must be
  # equal lengths
  step <- floor(nrow(train)/nrow(source))
  lin_train <- data.frame()
  for (n in 1:length(var$CF.name)) {
    for (x in 1:reso_len) {
      lin_train[x, n] <- mean(train[(x * step - step + 1):(x * step), n])
    }
  }
  colnames(lin_train) <- var$CF.name
  
  ### De_method routines!!!!! ###
  for (n in 1:length(var$DAP.name)) {
    if (de_method == "mean") {
      mean_source <- apply(source, 2, mean)
      mean_train <- apply(train, 2, mean)
      mean_diff <- mean_train - mean_source
      mean_ratio <- mean_train/mean_source
      debi_add <- list()
      debi_mult <- list()
      for (k in 1:length(add_var)) {
        debi_add[[k]] <- (source_add[[k]] + mean_diff[[k]])
      }
      for (k in 1:length(mult_var)) {
        debi_mult[[k]] <- (source_mult[[k]] * mean_ratio[[k]])
      }
      debi_add <- data.frame(debi_add)
      colnames(debi_add) <- add_var
      debi_mult <- data.frame(debi_mult)
      colnames(debi_mult) <- mult_var
      debi <- data.frame(debi_add, debi_mult)
    } else {
      if (de_method == "median") {
        med_source <- apply(source_add, 2, median)
        med_train <- apply(train_add, 2, median)
        med_diff <- med_train - med_source
        med_ratio <- med_train/med_source
        debi_add <- list()
        debi_mult <- list()
        for (k in 1:length(add_var)) {
          debi_add[[k]] <- (source_add[[k]] + med_diff[[k]])
        }
        for (k in 1:length(mult_var)) {
          debi_mult[[k]] <- (source_mult[[k]] * med_ratio[[k]])
        }
        debi_add <- data.frame(debi_add)
        colnames(debi_add) <- add_var
        debi_mult <- data.frame(debi_mult)
        colnames(debi_mult) <- mult_var
        debi <- data.frame(debi_add, debi_mult)
      } else {
        if (de_method == "linear") {
          debi <- list()
          for (i in 1:length(var$CF.name)) {
            if (all(is.na(source[[i]])) == FALSE & all(is.na(lin_train[[i]])) == 
                FALSE) {
              lin <- lm(lin_train[[i]] ~ source[[i]])
              x <- as.numeric(lin$coefficients[2])
              b <- as.numeric(lin$coefficients[1])
              m <- source[[i]]
              if (var$CF.name[[i]] == "precipitation_flux") {
                b <- 0
              }
              if (var$CF.name[[i]] == "specific_humidity") {
                b <- 0
              }
              debi[[i]] <- (m * x + b)
            } else {
              if (all(is.na(source[[i]])) == TRUE | all(is.na(lin_train[[i]])) == 
                  TRUE) {
                debi[[i]] <- NA
              }
            }
          }
          debi <- data.frame(debi)
          colnames(debi) <- var$CF.name
        }
      }
    }
  }
  
  # This step just ensures that we aren't breaking laws of nature by having
  # negative precipitation or negative specific humidity
  debi$precipitation_flux[debi$precipitation_flux < 0] <- 0
  debi$specific_humidity[debi$specific_humidity < 0] <- 0
  
  train.list <- list()
  lat <- ncdf4::ncdim_def(name = "latitude", units = "degree_north", vals = lat_train, 
                          create_dimvar = TRUE)
  lon <- ncdf4::ncdim_def(name = "longitude", units = "degree_east", vals = lon_train, 
                          create_dimvar = TRUE)
  time <- ncdf4::ncdim_def(name = "time", units = "sec", vals = (1:reso_len) * 
                             reso * 3600, create_dimvar = TRUE, unlim = TRUE)
  dim <- list(lat, lon, time)
  
  for (j in seq_along(var$CF.name)) {
    train.list[[j]] <- ncdf4::ncvar_def(name = as.character(var$CF.name[j]), 
                                        units = as.character(var$units[j]), dim = dim, missval = -999, verbose = verbose)
  }
  
  rows <- 1
  dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  results <- data.frame(file = character(rows), host = character(rows), mimetype = character(rows), 
                        formatname = character(rows), startdate = character(rows), enddate = character(rows), 
                        dbfile.name = paste("debias_met", sep = "."), stringsAsFactors = FALSE)
  
  loc.file = file.path(outfolder, paste("debias", year, "nc", sep = "."))
  loc <- nc_create(filename = loc.file, vars = train.list, verbose = verbose)
  
  for (j in seq_along(var$CF.name)) {
    ncdf4::ncvar_put(nc = loc, varid = as.character(var$CF.name[j]), vals = debi[[j]])
  }
  ncdf4::nc_close(loc)
  
  results$file <- loc.file
  results$host <- fqdn()
  results$startdate <- paste0(year, "-01-01 00:00:00", tz = "UTC")
  results$enddate <- paste0(year, "-12-31 23:59:59", tz = "UTC")
  results$mimetype <- "application/x-netcdf"
  results$formatname <- "CF Meteorology"
  
  return(invisible(results))
}
# debias_met('debi','GFDL.CM3.rcp45.r1i1p1.2006.nc', 'US-WCr.2006.nc', 4,
# de_method = 'linear')