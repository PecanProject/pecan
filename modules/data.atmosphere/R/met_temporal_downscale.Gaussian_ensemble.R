# substr function from right side 
substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

##' met_temporal_downscale.Gaussian_ensemble
##'
##' takes source data and a training dataset from the same site and temporally
##'    downscales the source dataset to the resolution of the training dataset based on statistics of the training dataset.
##'
##' @export
##' @param in.path ignored
##' @param in.prefix ignored
##' @param outfolder path to directory in which to store output. Will be created if it does not exist
##' @param input_met - the source dataset that will temporally downscaled by the train_met dataset
##' @param train_met - the observed dataset that will be used to train the modeled dataset in NC format. i.e. Flux Tower dataset 
##'                    (see download.Fluxnet2015 or download.Ameriflux) 
##' @param overwrite logical: replace output file if it already exists? 
##' @param verbose logical: should \code{\link[ncdf4:ncdf4-package]{ncdf4}} functions
##'   print debugging information as they run?
##' @param swdn_method - Downwelling shortwave flux in air downscaling method (options are "sine", "spline", and "Waichler")
##' @param n_ens - numeric value with the number of ensembles to run
##' @param w_len - numeric value that is the window length in days  
##' @param utc_diff - numeric value in HOURS that is local standard time difference from UTC time. CST is -6
##' @param ... further arguments, currently ignored
##' @author James Simkins, Akash


met_temporal_downscale.Gaussian_ensemble <- function(in.path, in.prefix, outfolder, 
                                                     input_met, train_met, overwrite = FALSE, verbose = FALSE, 
                                                     swdn_method = "sine", n_ens = 10, w_len = 20, utc_diff = -6, ... ) {
  
  sub_str <- substrRight(input_met, 7)
  year <- substr(sub_str, 1, 4)
  year <- as.numeric(year)
  eph_year <- year
  source_name <- substr(input_met, 1, nchar(input_met) - 8)
  
  # Get meteorological variables from PEcAn's met-specific standard table
  processed_vars <- c(
    "air_temperature",
    "air_temperature_max", 
    "air_temperature_min",
    "surface_downwelling_longwave_flux_in_air",
    "air_pressure",
    "surface_downwelling_shortwave_flux_in_air",
    "eastward_wind",
    "northward_wind", 
    "specific_humidity",
    "precipitation_flux",
    "soil_temperature",
    "relative_humidity",
    "volume_fraction_of_condensed_water_in_soil",
    "surface_downwelling_photosynthetic_photon_flux_in_air"
  )
  
  # Filter pecan_standard_met_table for only variables processed by your function
  var <- pecan_standard_met_table %>% 
    dplyr::filter(.data$cf_standard_name %in% processed_vars) %>%
    dplyr::select(CF.name = .data$cf_standard_name, 
                  .data$units)
  
  # Reading in the training data
  train <- list()
  tem <- ncdf4::nc_open(train_met)
  dim <- tem$dim
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
  # Reading in the source data
  source <- list()
  tem <- ncdf4::nc_open(input_met)
  for (j in seq_along(var$CF.name)) {
    if (exists(as.character(var$CF.name[j]), tem$var) == FALSE) {
      source[[j]] <- NA
    } else {
      source[[j]] <- ncdf4::ncvar_get(tem, as.character(var$CF.name[j]))
    }
  }
  ncdf4::nc_close(tem)
  
  source <- data.frame(source)
  colnames(source) <- var$CF.name
  
  # Default downscale will be to the resolution of the training dataset
  reso <- 24/(nrow(train)/365)
  reso_len <- nrow(train)
  
  # If a source doesn't have leap days, we need to remove them to ensure equal
  # lengths
  if (lubridate::leap_year(year) == TRUE) 
  {
    if (length(source$air_temperature)%%366 > 0) {
      if (length(train$air_temperature)%%366 == 0) {
        train <- train[1:365 * (nrow(train)/366)]
      }
      eph_year <- year - 1
    }
  }  #chose a non-leap year to use for daylength calculations if we don't have the 
  if (lubridate::leap_year(eph_year) == TRUE) {
    sp <- 366
  } else {
    sp <- 365
  }
  
  # Estimate soil thermal parameters amplitude damping factor and phase lag hours 
  # to characterize soil-air thermal coupling
  if (!all(is.na(train$soil_temperature)) && !all(is.na(train$air_temperature))) {
    # Calculate daily temperature ranges and parameters
    nsteps <- ceiling(length(train$air_temperature) / (24/reso))
    idx <- rep(seq_len(nsteps), each = 24/reso, length.out = length(train$air_temperature))
    air_range  <- tapply(train$air_temperature,  idx, function(x) diff(range(x, na.rm=TRUE)))
    soil_range <- tapply(train$soil_temperature, idx, function(x) diff(range(x, na.rm=TRUE)))
    # The 0.5 K threshold filters out sensor noise and ensures only days with
    # good diurnal temperature cycles are included in calculations.
    valid_days <- which(!is.na(air_range) & !is.na(soil_range) & air_range > 0.5 & soil_range > 0)
    
    # Damping factor calculation
    if (length(valid_days) >= 10) {
      damping_factor <- stats::median(soil_range[valid_days] / air_range[valid_days], na.rm = TRUE)
      # soil temperature amplitude is always reduced compared to air temperature (lower bound 0.3)
      # but never exceeds it (upper bound 1.0) at shallow depths.
      damping_factor <- min(max(damping_factor, 0.3), 1.0)
    } else {
      damping_factor <- NA
    }
    
    # Phase lag calculation
    air_detrend  <- train$air_temperature  - mean(train$air_temperature,  na.rm = TRUE)
    soil_detrend <- train$soil_temperature - mean(train$soil_temperature, na.rm = TRUE)
    valid_idx <- !is.na(air_detrend) & !is.na(soil_detrend)
    
    # 48 hours represents 2 complete diurnal cycles, which is the minimum for reliable 
    # cross-correlation analysis in temperature time series with 24-hour periodicity
    if (sum(valid_idx) >= 48) {
      air_clean  <- air_detrend[valid_idx]
      soil_clean <- soil_detrend[valid_idx]
      max_lag <- min(48/reso, length(air_clean)/4)
      if (max_lag >= 1) {
        ccf_res <- stats::ccf(air_clean, soil_clean, lag.max = max_lag, plot = FALSE)
        lag_hr <- abs(ccf_res$lag[which.max(ccf_res$acf)]) * reso
        # depths of 5, 10, 20 and 30 cm the delay amounts to 1, 2, 4 and to about 8 h, respectively
        phase_lag_hr <- min(max(lag_hr,0),8)
      } else {
        phase_lag_hr <- NA
      }
    } else {
      phase_lag_hr <- NA
    }
  } else {
    damping_factor <- NA
    phase_lag_hr <- NA
  }
  
  # Now we start a for loop for the ensemble members and begin downscaling. A
  # random normal distribution is used to downscale as so;
  # (mean <- value of source data) (sd <- +/- window_days of train data at the
  # same time intervals) 
  results <- list()
  for (e in seq_len(n_ens)) {
    
    div <- nrow(train)/nrow(source)  #tells us how many values need to be generated (via downscaling) from each source value
    sd_step <- nrow(train)/sp  #allows us to step through each window at specific times
    df <- data.frame()
    # Temperature - use spline interpolation 
    sourtemp <- source$air_temperature
    temper <- vector()
    tem.met <- vector()
    mean_val <- vector()
    
    # since we begin our temper vec to min temperature, we want this to coincide with the normal
    # low value
    for (l in seq_len(30)){
      mean_val[l] <- which.min(train$air_temperature[1*l:sd_step*l])
    }
    mean_val <- floor(mean(mean_val))
    
    # Daily products typically have tmin and tmax, probably need to make version in case it doesn't
    if (length(sourtemp) <= 366){
      for (i in seq_along(sourtemp)){
        a <- source$air_temperature_min[i]
        b <- source$air_temperature[i]
        c <- source$air_temperature_max[i]
        d <- source$air_temperature[i]
        vec <- c(a,b,c,d)
        temper <- append(temper,vec)
      }
      seq_by = 24/reso/length(vec)
      sourtemp <- temper 
      for (x in seq(from=mean_val, to=reso_len, by=seq_by)){
        tem.met[x] <- sourtemp[x / seq_by]
      }
      len_diff <- reso_len - length(tem.met)
      tem.met <- append(tem.met,values = rep(NA,len_diff)) 
    } else {
      for (x in seq(from=0, to=reso_len, by=div)){
        tem.met[x] <- sourtemp[x / div]
      }
    }
    
    spline.temp = zoo::na.spline(tem.met)
    df[1:reso_len, "air_temperature"] <- spline.temp
    
    # air temperature max and min downscaling with Gaussian ensemble
    if (all(is.na(source$air_temperature_max)) || all(is.na(source$air_temperature_min))) {
      daily_temps <- split(df$air_temperature, ceiling(seq_along(df$air_temperature)/(24/reso)))
      daily_max <- sapply(daily_temps, max)
      daily_min <- sapply(daily_temps, min)
      
      df$air_temperature_max <- rep(daily_max, each = 24/reso)
      df$air_temperature_min <- rep(daily_min, each = 24/reso)
    } else {
      temp_max <- vector()
      temp_min <- vector()
      
      for (x in seq_along(source$air_temperature_max)) {
        lowday <- (x - w_len) * div
        highday <- (x + w_len) * div
        if (lowday < 0) {
          lowday <- 0
        }
        if (highday > reso_len) {
          highday <- reso_len
        }
        
        if (!is.na(source$air_temperature_max[x])) {
          dwnsc_max <- vector()
          for (n in seq_len(div)) {
            dwnsc_max[n] <- stats::rnorm(1, 
                                         mean = source$air_temperature_max[x], 
                                         sd = stats::sd(train$air_temperature_max[lowday:highday], na.rm = TRUE))
          }
          temp_max <- append(temp_max, dwnsc_max)
        }
        
        if (!is.na(source$air_temperature_min[x])) {
          dwnsc_min <- vector()
          for (n in seq_len(div)) {
            dwnsc_min[n] <- stats::rnorm(1, 
                                         mean = source$air_temperature_min[x], 
                                         sd = stats::sd(train$air_temperature_min[lowday:highday], na.rm = TRUE))
          }
          temp_min <- append(temp_min, dwnsc_min)
        }
      }
      df$air_temperature_max <- temp_max[1:reso_len]
      df$air_temperature_min <- temp_min[1:reso_len]
    }
    df$air_temperature_max <- pmax(df$air_temperature_max, df$air_temperature, na.rm = TRUE)
    df$air_temperature_min <- pmin(df$air_temperature_min, df$air_temperature, na.rm = TRUE)
    
    # soil temperature
    soursoiltemp <- source$soil_temperature
    if (!all(is.na(soursoiltemp))) {
      soil.met <- vector()
      
      # handles the common case where source data is daily but target is sub-daily
      if (length(soursoiltemp) <= 366) {
        for (i in seq_along(soursoiltemp)) {
          soil.met <- append(soil.met, rep(soursoiltemp[i], div))
        }
      } else {
        # source data is sub-daily; used direct sampling
        for (x in seq(from=0, to=reso_len, by=div)) {
          soil.met[x] <- soursoiltemp[x / div]
        }
      }
      
      # phase lag from hours to time steps based on target resolution
      lag_step <- round(phase_lag_hr / reso)
      # baseline temperature values for soil-air coupling
      tair_mean <- mean(df$air_temperature, na.rm = TRUE)
      soil_base <- mean(soursoiltemp, na.rm = TRUE)
      if (lag_step > 0 && lag_step < length(df$air_temperature)) {
        # soil temperature responds to air temperature with a delay (typically 1-4 hours)
        # pad beginning with mean value to maintain series length
        tair_lag <- c(rep(tair_mean, lag_step), 
                      df$air_temperature[1:(length(df$air_temperature) - lag_step)])
      } else {
        # this occurs when lag is too large (>data length) or non-positive
        # still reasonable due to damping factor
        tair_lag <- df$air_temperature
      }
      soil_proc <- soil_base + 
        damping_factor * (tair_lag - tair_mean)
      
      if (!all(is.na(train$soil_temperature))) {
        soil_residual_sd <- stats::sd(train$soil_temperature - train$air_temperature, na.rm = TRUE)
        if (!is.na(soil_residual_sd) && soil_residual_sd > 0) {
          soil_noise <- stats::rnorm(length(soil_proc), 0, soil_residual_sd * 0.5)
          soil_proc <- soil_proc + soil_noise
        }
      }
      
      df[1:reso_len, "soil_temperature"] <- soil_proc[1:reso_len]
    } else {
      df[1:reso_len, "soil_temperature"] <- rep(NA, reso_len)
    }
    
    # after this maybe we can run it through the random norm to add variation
    # but not sure how models will react 
    
    # Precipitation_flux this takes the daily total of precipitation and uses that as
    # a total possible amount of precip.  It randomly distributes the values of
    # precipitation
    rand_vect_cont <- function(N, M, sd = 1) {
      vec <- truncnorm::rtruncnorm(N, a = 0, b = Inf, M/N, sd)
      vec/sum(vec) * M
    }
    precip <- vector()
    for (x in seq_along(source$precipitation_flux)) {
      lowday <- (x - w_len) * div
      highday <- (x + w_len) * div
      if (lowday < 0) {
        lowday <- 0
      }
      if (highday > reso_len) {
        highday <- reso_len
      }
      dwnsc_day <- rand_vect_cont(
        div,
        source$precipitation_flux[x],
        sd = stats::sd(train$precipitation_flux[lowday:highday]))
      precip <- append(precip, dwnsc_day)
    }
    df$precipitation_flux <- precip
    
    wnd <- c("specific_humidity", "eastward_wind", "northward_wind", "surface_downwelling_longwave_flux_in_air", 
             "air_pressure", "relative_humidity", "volume_fraction_of_condensed_water_in_soil")
    for (u in wnd) {
      train_vec <- vector()
      a <- as.numeric(train[[u]])
      sour <- as.numeric(source[[u]])
      if (all(is.na(sour)) == TRUE) {
        train_vec <- rep(NA, reso_len)
      }
      if (all(is.na(sour)) == FALSE) {
        for (x in seq_along(sour)) {
          lowday <- (x - w_len) * div
          highday <- (x + w_len) * div
          if (lowday < 0) {
            lowday <- 0
          }
          if (highday > reso_len) {
            highday <- reso_len
          }
          dwnsc_day <- vector()
          for (n in seq_len(div)) {
            if (u == "volume_fraction_of_condensed_water_in_soil") {
              idx <- (x - 1) * div + n
              current_precip <- ifelse(idx <= length(precip) && idx >= 1, 
                                       precip[idx], 0)
              if (is.na(current_precip)) current_precip <- 0
              antecedent_sm <- ifelse(idx > 1, 
                                      sour[max(1, x-1)], 
                                      sour[x])
              base_sd <- stats::sd(a[lowday:highday], na.rm = TRUE)
              sm_cv <- stats::sd(sour, na.rm = TRUE) / mean(sour, na.rm = TRUE)
              if (!is.na(sm_cv) && is.finite(sm_cv)) {
                sm_fc <- stats::quantile(sour, 0.75, na.rm = TRUE)
                moisture_stress <- abs(antecedent_sm - sm_fc) / sm_fc
                uncertainty_factor <- 1.0 + sm_cv * moisture_stress
                if (current_precip > 0) {
                  uncertainty_factor <- uncertainty_factor * 1.2  # 20% increase during precipitation
                }
              } else {
                uncertainty_factor <- 1.0
              }
              uncertainty_factor <- pmax(0.7, pmin(uncertainty_factor, 1.8))
              sd_adj <- base_sd * uncertainty_factor
              dwnsc_day[n] <- truncnorm::rtruncnorm(1, a = 0, b = 1, mean = sour[x], sd = sd_adj)
            } else if (u == "relative_humidity") {
              base_sd <- stats::sd(a[lowday:highday], na.rm = TRUE)
              if (is.na(base_sd) || base_sd <= 0) {
                base_sd <- 5.0  
              }
              temp_idx <- (x - 1) * div + n
              if (temp_idx > 0 && temp_idx <= length(df$air_temperature) && x <= length(source$air_temperature)) {
                current_temp_c <- PEcAn.utils::ud_convert(df$air_temperature[temp_idx], "kelvin", "celsius")
                source_temp_c <- PEcAn.utils::ud_convert(source$air_temperature[x], "kelvin", "celsius")

                if (current_temp_c > -40 && current_temp_c < 50 && source_temp_c > -40 && source_temp_c < 50) {
                  # magnus formula for saturation vapor pressure (kPa)
                  es_current <- PEcAn.data.atmosphere::t2es(current_temp_c, method = "Magnus")
                  es_source <- PEcAn.data.atmosphere::t2es(source_temp_c, method = "Magnus")
                  saturation_ratio <- es_source / es_current
                  adjusted_rh <- sour[x] * saturation_ratio
                } else {
                  adjusted_rh <- sour[x]
                }
              } else {
                adjusted_rh <- sour[x]
              }
              dwnsc_day[n] <- truncnorm::rtruncnorm(1, a = 0, b = 100, mean = adjusted_rh, sd = base_sd) 
            } else {
              dwnsc_day[n] <- stats::rnorm(1, mean = sour[x], sd = stats::sd(a[lowday:highday], na.rm = TRUE))
            }
          }
          train_vec <- append(train_vec, dwnsc_day)
        }
      }
      df[1:length(train_vec), u] <- train_vec
    }
    
    df$specific_humidity[df$specific_humidity < 0] <- 0
    
    if ("volume_fraction_of_condensed_water_in_soil" %in% names(df)) {
      df$volume_fraction_of_condensed_water_in_soil[
        df$volume_fraction_of_condensed_water_in_soil < 0] <- 0
      df$volume_fraction_of_condensed_water_in_soil[
        df$volume_fraction_of_condensed_water_in_soil > 1] <- 1
    }
    
    # Downwelling shortwave radiation flux Ephemeris is a function to calculate
    # sunrise/sunset times and daylength for SW calculations in sine swdn_method
    ephemeris <- function(lat, lon, date, span = 1, tz = "UTC") {
      
      lon.lat <- matrix(c(lon, lat), nrow = 1)
      
      # using noon gets us around daylight saving time issues
      day <- as.POSIXct(sprintf("%s 12:00:00", date), tz = tz)
      sequence <- seq(from = day, length.out = span, by = "days")
      
      sunrise <- suntools::sunriset(lon.lat, sequence, direction = "sunrise", 
                                    POSIXct.out = TRUE)
      sunset <- suntools::sunriset(lon.lat, sequence, direction = "sunset", 
                                   POSIXct.out = TRUE)
      solar_noon <- suntools::solarnoon(lon.lat, sequence, POSIXct.out = TRUE)
      
      data.frame(date = as.Date(sunrise$time), sunrise = as.numeric(format(sunrise$time, "%H%M")), 
                 solarnoon = as.numeric(format(solar_noon$time, "%H%M")), 
                 sunset = as.numeric(format(sunset$time, "%H%M")),
                 day_length = as.numeric(sunset$time - sunrise$time))
    }
    
    swsource <- source$surface_downwelling_shortwave_flux_in_air
    swdn <- vector()
    
    # The sine swdn_method produces an hourly sine wave of
    if (swdn_method == "sine") {
      
      eph <- ephemeris(lat_train, lon_train, date = paste0(eph_year, "-01-01", tz = "UTC"), 
                       span = sp)
      day_len <- eph$day_length
      
      # Need to have average daily values for this swdn_method, so this upscales the
      # source data to daily resolution if needed
      daily_row <- nrow(source)
      daily_step <- daily_row/sp
      daily.swdn <- vector()
      for (x in seq_len(sp)) {
        daily.swdn[x] <- mean(swsource[(x * daily_step - daily_step + 1):(x * daily_step)])
      }
      
      # creating the sine wave
      for (i in seq_along(daily.swdn)) {
        t <- seq(from = pi/day_len[i], to = pi, by = pi/day_len[i])
        wav <- ((daily.swdn[i] * (24/day_len[i]))/0.637) * sin(t)
        
        # swdn = 0 without sunlight
        srs <- eph$sunrise
        srs_hr <- floor(srs[i] / 100)  # extract hours (works for both 430 -> 4 and 1215 -> 12)
        srs_min <- (srs[i] %% 100) / 60  # convert minutes to fraction (30 -> 0.5)
        # utc_diff must be used so we can begin the sine wave at local sunrise
        hr <- srs_hr + srs_min + utc_diff
        hr <- max(0, min(23, hr))
        
        l <- vector()
        for (n in seq_len(hr)) {
          l[n] <- 0
        }
        for (n in seq_along(wav)) {
          l[n + hr] <- wav[n]
        }
        for (n in seq_len(floor(24 - (length(wav) + hr)))) {
          l[n + hr + length(wav)] <- 0
        }
        
        swdn <- append(swdn, l)
      }
      
      swflux <- vector()
      sw_step <- length(swdn)/reso_len
      for (x in seq_len(reso_len)) {
        swflux[x] <- mean(swdn[(x * sw_step - sw_step + 1):(x * sw_step)])
      }
      swflux[swflux < 0] <- 0
    }
    
    # The spline swdn_method uses spline interpolation to connect existing values and
    # downscale
    if (swdn_method == "spline") {
      tem.met <- vector()
      for (x in seq(from = 0, to = nrow(train), by = div)) {
        tem.met[x] <- swsource[x/div]
      }
      
      swflux <- vector()
      swflux <- zoo::na.spline(tem.met)
      swflux[swflux < 0] <- 0
    }
    
    # The Waichler swdn_method doesn't need averaged SW train values, it sources SW
    # downwelling flux based on Tmax-Tmin and Precipitation Reference is Waichler and
    # Wigtosa 2003. Our no-precip coefficient is 2 instead of 1 becuase this better
    # matches our observations (1 significantly undervalues SW downwelling flux)
    if (swdn_method == "Waichler") {
      inter <- paste0(reso, " hour")
      days <- seq(as.POSIXct(paste0(eph_year, "-01-01 00:00:00"),tz="UTC"), 
                  as.POSIXct(paste0(eph_year, "-12-31 18:00:00"),tz="UTC"), 
                  by = inter)
      days.doy <- as.numeric(format(days,"%j"))
      days.hour <- as.numeric(lubridate::hour(days) + lubridate::minute(days) / 60 + lubridate::second(days) / 3600)
      cosZ <- PEcAn.data.atmosphere::cos_solar_zenith_angle(days.doy, lat_train, lon_train, reso*3600, days.hour)
      I <- 1000 * cosZ
      m <- vector()
      for (i in seq_len(12)) {
        m[i] <- lubridate::days_in_month(as.Date(paste0(year, "-", i, "-01")))
      }
      bmlist <- vector()
      
      Bm <- c(0.2089, 0.2857, 0.2689, 0.2137, 0.1925, 0.2209, 0.2527, 0.2495, 
              0.2232, 0.1728, 0.1424, 0.1422)
      for (x in seq_along(Bm)) {
        mlen <- list()
        mlen <- rep(Bm[x], m[x] * 24/reso)
        bmlist <- append(bmlist, mlen)
      }
      A <- 0.73
      C <- 0.7
      hdry <- vector()
      for (i in seq_along(precip)) {
        if (i <= length(df$air_temperature_max) && 
            i <= length(df$air_temperature_min) && 
            i <= length(bmlist)) {
          p <- ifelse(precip[i] > 0, 0.65, 2)
          if (!is.na(df$air_temperature_max[i]) && !is.na(df$air_temperature_min[i])) {
            temp_range <- df$air_temperature_max[i] - df$air_temperature_min[i]
            hdry[i] <- A * p * (1 - exp(-1 * bmlist[i] * (temp_range^C)))
          } else {
            hdry[i] <- 0
          }
        } else {
          hdry[i] <- 0
        }
      }
      hdry[hdry < 0] <- 0
      swflux <- hdry * I
      swflux[swflux < 0] <- 0  
    }
    # Waichler method is the only method with ensembles for downwelling shortwave flux
    
    df$surface_downwelling_shortwave_flux_in_air <- swflux
    
    # PPFD downscaling
    ppfd_source <- source$surface_downwelling_photosynthetic_photon_flux_in_air
    if (all(is.na(ppfd_source))) {
      ppfd_flux <- PEcAn.data.atmosphere::sw2ppfd(swflux) / 1000  # Convert umol to mol
      ppfd_flux[ppfd_flux < 0] <- 0
      df$surface_downwelling_photosynthetic_photon_flux_in_air <- ppfd_flux
    } else {
      inter <- paste0(reso, " hour")
      days <- seq(as.POSIXct(paste0(year, "-01-01 00:00:00"), tz = "UTC"), 
                  length.out = reso_len, by = inter)
      days.doy <- as.numeric(format(days, "%j"))
      days.hour <- as.numeric(format(days, "%H")) + as.numeric(format(days, "%M"))/60
      cosZ <- PEcAn.data.atmosphere::cos_solar_zenith_angle(days.doy, lat_train, lon_train, 
                                                            reso*3600, days.hour)
      is_daylight <- cosZ > 0
      
      train_ppfd <- train$surface_downwelling_photosynthetic_photon_flux_in_air
      if (all(is.na(train_ppfd)) && !all(is.na(train$surface_downwelling_shortwave_flux_in_air))) {
        train_ppfd <- PEcAn.data.atmosphere::sw2ppfd(train$surface_downwelling_shortwave_flux_in_air) / 1000 # Convert to mol
      }
      
      train_vec <- vector()
      for (x in seq_along(ppfd_source)) {
        lowday <- (x - w_len) * div
        highday <- (x + w_len) * div
        if (lowday < 0) {
          lowday <- 0
        }
        if (highday > reso_len) {
          highday <- reso_len
        }
        if (length(train_ppfd) >= highday && !all(is.na(train_ppfd[lowday:highday]))) {
          base_sd <- stats::sd(train_ppfd[lowday:highday], na.rm = TRUE)
        } else {
          base_sd <- NA
        }
        if (is.na(base_sd) || base_sd < 1e-6) {
          cv_ppfd <- stats::sd(ppfd_source, na.rm = TRUE) / mean(ppfd_source, na.rm = TRUE)
          if (!is.na(cv_ppfd) && is.finite(cv_ppfd)) {
            base_sd <- cv_ppfd * abs(ppfd_source[x])
          } else {
            base_sd <- 0.15 * abs(ppfd_source[x])
          }
        }
        
        dwnsc_day <- vector()
        for (n in seq_len(div)) {
          idx <- (x - 1) * div + n
          if (idx > 0 && idx <= length(is_daylight) && is_daylight[idx]) {
            dwnsc_day[n] <- max(0, stats::rnorm(1, mean = ppfd_source[x], sd = base_sd)) # daytime using gaussian downscaling
          } else {
            dwnsc_day[n] <- 0 # nighttime ppfd must be zero
          }
        }
        train_vec <- c(train_vec, dwnsc_day)
      }
      df$surface_downwelling_photosynthetic_photon_flux_in_air <- train_vec[1:reso_len]
    }
    df$surface_downwelling_photosynthetic_photon_flux_in_air[
      df$surface_downwelling_photosynthetic_photon_flux_in_air < 0] <- 0
    # maximum PPFD is ~0.0025 mol m-2 s-1 (2500 umol m-2 s-1) under full sunlight
    df$surface_downwelling_photosynthetic_photon_flux_in_air[
      df$surface_downwelling_photosynthetic_photon_flux_in_air > 0.0025] <- 0.0025
    
    # Putting all the variables together in a data frame
    downscaled.met <- data.frame(df)
    
    train.list <- list()
    lat <- ncdf4::ncdim_def(name = "latitude", units = "degree_north", vals = lat_train, 
                            create_dimvar = TRUE)
    lon <- ncdf4::ncdim_def(name = "longitude", units = "degree_east", vals = lon_train, 
                            create_dimvar = TRUE)
    time <- ncdf4::ncdim_def(name = "time", units = "sec", vals = seq_len(reso_len) * 
                               reso * 3600, create_dimvar = TRUE, unlim = TRUE)
    dim <- list(lat, lon, time)
    
    for (j in seq_along(var$CF.name)) {
      train.list[[j]] <- ncdf4::ncvar_def(name = as.character(var$CF.name[j]), 
                                          units = as.character(var$units[j]), dim = dim, missval = -999, verbose = verbose)
    }
    
    rows <- 1
    dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
    source_name <- basename(input_met)  # extracts "US-Ha1.2004.nc" from full path
    loc.file <- file.path(outfolder, paste0(tools::file_path_sans_ext(source_name), ".dwnsc.gauss.ens", 
                                            e, ".", year, ".nc"))
    
    loc <- ncdf4::nc_create(filename = loc.file, vars = train.list, force_v4 = TRUE, verbose = verbose)
    for (j in seq_along(var$CF.name)) {
      var_name <- as.character(var$CF.name[j])
      ncdf4::ncvar_put(nc = loc, varid = var_name, vals = downscaled.met[[var_name]])
    }
    ncdf4::nc_close(loc)
    
    results[[e]] <- data.frame(file = loc.file, 
                               host = rep(PEcAn.remote::fqdn(),rows), 
                               mimetype = rep("application/x-netcdf",rows), 
                               formatname = rep("CF Meteorology",rows),
                               startdate = paste0(year, "-01-01 00:00:00", tz = "UTC"), 
                               enddate = paste0(year, "-12-31 23:59:59", tz = "UTC"),
                               dbfile.name = paste0(source_name, ".dwnsc.ens"),
                               stringsAsFactors = FALSE)
    
  }
  
  return(invisible(results))    
}

# met_temporal_downscale.Gaussian_ensemble( '~', '~',
# 'dwnsc','MACA.IPSL-CM5A-LR.rcp85.r1i1p1.2006.nc', 'US-WCr.2006.nc')
# met_temporal_downscale.Gaussian_ensemble( '~', '~', 'dwnsc','MACA.IPSL-CM5A-LR.rcp85.r1i1p1.2006.nc', 'US-WCr.2006.nc')