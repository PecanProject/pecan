##' nc.merge
##' Parses multiple netCDF files into one central document for temporal downscaling procedure
# -----------------------------------
# Description
# -----------------------------------
##' @title nc.merge
##' @family tdm - Temporally Downscale Meteorology
##' @author James Simkins, Christy Rollinson
##' @description This is the 1st function for the tdm (Temporally Downscale Meteorology) workflow. The nc2dat.train function
##'              parses multiple netCDF files into one central training data file called 'dat.train_file'. This netCDF
##'              file will be used to generate the subdaily models in the next step of the workflow, generate.subdaily.models(). 
##'              It is also called in tdm_predict_subdaily_met which is the final step of the tdm workflow. 
# -----------------------------------
# Parameters
# -----------------------------------
##' @param outfolder - directory where output will be stored
##' @param in.path - path of coarse model (e.g. GCM output)
##' @param in.prefix - prefix of model string as character (e.g. IPSL.r1i1p1.rcp85)
##' @param start_date - yyyy-mm-dd
##' @param end_date - yyyy-mm-dd
##' @param upscale - Upscale can either be set for FALSE (leave alone) or to the temporal resolution you want to aggregate to
#                    options are: "year", "doy" (day of year), or "hour"
##' @param overwrite logical: replace output file if it already exists?
##' @param verbose logical: should \code{\link[ncdf4:ncdf4-package]{ncdf4}}
##'   functions print debugging information as they run?
##' @param ... further arguments, currently ignored
##'
##' @export
# -----------------------------------
#----------------------------------------------------------------------
# Begin Function
#----------------------------------------------------------------------
nc.merge <- function(outfolder, in.path, in.prefix, start_date, end_date, 
    upscale = FALSE, overwrite = FALSE, verbose = FALSE, ...) {
    
    start_date <- as.POSIXlt(start_date, tz = "UTC")
    end_date <- as.POSIXlt(end_date, tz = "UTC")
    
    start_year <- lubridate::year(start_date)
    end_year <- lubridate::year(end_date)
    
    yr_seq <- seq(start_year, end_year)

    #------------ Read in the data
    input_met <- list()
    for (j in seq_along(yr_seq)) {
        input_met[[j]] <- file.path(in.path, paste0(in.prefix, ".", yr_seq[j], 
            ".nc", sep = ""))
    }
    
    
    vars.info <- data.frame(CF.name = c("air_temperature", "air_temperature_max", 
        "air_temperature_min", "surface_downwelling_longwave_flux_in_air", 
        "air_pressure", "surface_downwelling_shortwave_flux_in_air", "eastward_wind", 
        "northward_wind", "specific_humidity", "precipitation_flux"), units = c("Kelvin", 
        "Kelvin", "Kelvin", "W/m2", "Pascal", "W/m2", "m/s", "m/s", "g/g", 
        "kg/m2/s"))
    
    stepby <- list()  # list of time stepbys just in case they vary
    
    # Have to do 1 go first to initialize the dataframe
    raw_train_data <- list()
    tem <- ncdf4::nc_open(input_met[[1]])
    dim <- tem$dim
    for (j in seq_along(vars.info$CF.name)) {
        if (exists(as.character(vars.info$CF.name[j]), tem$var) == FALSE) {
            raw_train_data[[j]] <- NA
        } else {
            raw_train_data[[j]] <- ncdf4::ncvar_get(tem, as.character(vars.info$CF.name[j]))
        }
    }
    names(raw_train_data) <- vars.info$CF.name
    train_df <- data.frame(raw_train_data)
    
    # Figure out what temporal resolution the given data is in
    # stepby[] helps us create the data sequence length
    raw_train_data <- data.frame(raw_train_data)
    if ((nrow(raw_train_data) == 17520) | (nrow(raw_train_data) == 17568)) {
        stepby[1] <- 2
    }
    if ((nrow(raw_train_data) == 8760) | (nrow(raw_train_data) == 8784)) {
        stepby[1] <- 1
    }
    if ((nrow(raw_train_data) == 1460) | (nrow(raw_train_data) == 1464)) {
      stepby[1] <- (1/6)
    }
    # Add a time stamp
    start_time <- as.POSIXlt(paste0(yr_seq[1], "-01-01"), tz = "UTC")
    end_time <- as.POSIXlt(paste0(yr_seq[1] + 1, "-01-01"), tz = "UTC")
    train_df$date <- seq.POSIXt(from = start_time, by = 60 * 60/stepby[[1]], 
        length.out = nrow(train_df))
    summary(train_df)
    
    lat_raw_train_data <- as.numeric(ncdf4::ncvar_get(tem, "latitude"))
    lon_raw_train_data <- as.numeric(ncdf4::ncvar_get(tem, "longitude"))
    ncdf4::nc_close(tem)
    
    # Loop through remaining files and format Just a safe guard in case we
    # run this for a single year
    if (length(input_met) > 1) {
        for (i in 2:length(input_met)) {
            if (file.exists(input_met[[i]])) {
                raw_train_data <- list()
                tem <- ncdf4::nc_open(input_met[[i]])
                dim <- tem$dim
                for (j in seq_along(vars.info$CF.name)) {
                  if (exists(as.character(vars.info$CF.name[j]), tem$var) == 
                    FALSE) {
                    raw_train_data[[j]] <- NA
                  } else {
                    raw_train_data[[j]] <- ncdf4::ncvar_get(tem, as.character(vars.info$CF.name[j]))
                  }
                }
                names(raw_train_data) <- vars.info$CF.name
                raw_train_data <- data.frame(raw_train_data)
                
                # Figure out what temporal resolution the given data is in
                # stepby[] helps us create the data sequence length
                if ((nrow(raw_train_data) == 17520) | (nrow(raw_train_data) == 
                  17568)) {
                  stepby[i] <- 2
                }
                if ((nrow(raw_train_data) == 8760) | (nrow(raw_train_data) == 
                  8784)) {
                  stepby[i] <- 1
                }
                if ((nrow(raw_train_data) == 1460) | (nrow(raw_train_data) == 1464)) {
                  stepby[1] <- (1/6)
                }
                
                # Add a time stamp
                start_time <- as.POSIXlt(paste0(yr_seq[i], "-01-01"), tz = "UTC")
                end_time <- as.POSIXlt(paste0(yr_seq[i] + 1, "-01-01"), 
                  tz = "UTC")
                raw_train_data$date <- seq.POSIXt(from = start_time, by = 60 * 
                  60/stepby[[i]], length.out = nrow(raw_train_data))
                summary(raw_train_data)
                
                train_df <- rbind(train_df, raw_train_data)
            } else {
                stepby[i] <- NA
            }
            ncdf4::nc_close(tem)
        }  # End year loop
    }
    
    # Quick & dirty way of gap-filling any lingering NAs
    for (i in 1:ncol(train_df)) {
        train_df[is.na(train_df[, i]), i] <- mean(train_df[, i], na.rm = TRUE)
    }

    if (upscale == FALSE) {
        dat.train <- train_df
    } else {
      # Need to create column of each of these for the aggregate function to work
      time.vars <- c("year", "doy", "hour")
      agg.ind <- which(time.vars==upscale)
      time.vars <- time.vars[1:agg.ind]
      train_df["year"] = lubridate::year(train_df$date)
      train_df["doy"] = lubridate::yday(train_df$date)
      train_df["hour"] = lubridate::hour(train_df$date)
        # Figure out which temporal variables we're aggregating over
      if (upscale == "year") {
        upscale_timestep = 365
      }
      if (upscale == "doy") {
        upscale_timestep = 1
      }
      if (upscale == "hour") {
        upscale_timestep = 1/24
      }
        dat.train <- stats::aggregate(train_df[, names(train_df)[!names(train_df) %in% 
            c("year", "doy", "hour")]], by = train_df[time.vars], FUN = mean, 
            na.rm = FALSE)
        dat.train <- dat.train[order(dat.train$date), ]
    }
    # ---------------------------------
    
    # Add dataset name
    
    dat.train$dataset <- paste0(in.prefix)

    # Create dimensions for NC file
    ntime = nrow(dat.train)
    days_elapsed <- (1:ntime) * upscale_timestep - .5*upscale_timestep
    time <- ncdf4::ncdim_def(name = "time", units = paste0("days since ", start_year, "-01-01T00:00:00Z"),
                             vals = as.array(days_elapsed), create_dimvar = TRUE, unlim = TRUE)
    dim$time = time
    # Create var.list for the NC file
    var.list <- list()
    for (j in seq_along(vars.info$CF.name)) {
        var.list[[j]] <- ncdf4::ncvar_def(name = as.character(vars.info$CF.name[j]), 
            units = as.character(vars.info$units[j]), dim = dim, missval = -9999, 
            verbose = verbose)
    }
    
    # Create NC file
    dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
    
    loc.file <- file.path(outfolder, paste0(in.prefix, "_dat.train.nc"))
    loc <- ncdf4::nc_create(filename = loc.file, vars = var.list, verbose = verbose)
    
    for (j in vars.info$CF.name) {
        ncdf4::ncvar_put(nc = loc, varid = as.character(j), vals = dat.train[[j]][seq_len(nrow(dat.train))])
    }
    ncdf4::nc_close(loc)
}
