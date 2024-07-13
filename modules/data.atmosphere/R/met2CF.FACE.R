#' convert FACE files to CF files
#'
#' Note: `in.path` and `in.prefix` together must identify exactly one file,
#'   or this function returns NULL.
#' Further note that despite its name, `in.prefix` will match anywhere in
#'   the filename: met2CF.FACE("dir", "a", ...)` will find both `dir/a_b.nc`
#'   and `dir/b_a.nc`!
#'
#' @param in.path directory in which to find inputs (as `*.nc`)
#' @param in.prefix pattern to match to select a file within `in.path`
#'
#' @param outfolder path to write output.
#'   Should contain the substring "FACE", which will be rewritten to "FACE_a" and "FACE_e"
#'    for the corresponding treatments.
#' @param start_date,end_date ignored. Time is taken from the input files.
#' @param input.id ignored
#' @param site list[like]. Only components `lat` and `lon` (both in decimal degrees) are currently used
#' @param format specification of variable names and units in the format returned by `PEcAn.DB::query.format.vars`
#' @param ... other arguments, currently ignored
#' @author Elizabeth Cowdery
#'
#' @export
met2CF.FACE <- function(in.path,in.prefix,outfolder,start_date,end_date,input.id,site,format, ...) {
  
  files <- dir(in.path, in.prefix)
  file <- files[grep(pattern = "*.nc", files)]
  if (!(length(file) == 1)) {
    return(NULL)
  }
  f <- gsub("//","/",file.path(in.path, file))
  
  for (treatment in c("a", "e")) {
    
    t.outfolder <- paste(unlist(strsplit(outfolder, "FACE")), collapse = paste0("FACE_", treatment))
    # t.outfolder <- paste0(outfolder,"_",treatment)
    if (!file.exists(t.outfolder)) {
      dir.create(t.outfolder)
    }
    f.cf <- file.path(t.outfolder, file)
    if (!file.exists(f.cf)) {
      # file.copy(f,f.cf)
      
      # paste('ncks -x -v', paste0(rm.vars,collapse = ','), f.cf, f.cf)
      
      #---------------------------------------------------------------------#
      # Latitude and Longitude
      
      nc1 <- ncdf4::nc_open(f, write = TRUE)
      
      time_units <- paste0("hours/2", unlist(strsplit(nc1$var$TIMEstp$units, "timesteps"))[2])
      time <- ncdf4::ncdim_def(name = "time", units = time_units, vals = nc1$dim$tstep$vals)
      lon <- ncdf4::ncdim_def("longitude", "degrees_east", as.numeric(site$lon)) # define netCDF dimensions for variables
      lat <- ncdf4::ncdim_def("latitude", "degrees_north", as.numeric(site$lat))
      dim <- list(lat, lon, time)
      
      # convert wind speed and wind direction to eastward_wind and northward_wind
      wd <- 0  # wind direction - not specified so I set to 0???
      ws <- ncdf4::ncvar_get(nc = nc1, varid = "Wind")  #wind speed
      ew <- ws * cos(wd * (pi / 180))
      nw <- ws * sin(wd * (pi / 180))
      
      var <- ncdf4::ncvar_def(name = "eastward_wind", units = "m/s", dim = dim, missval = -6999, verbose = FALSE)
      nc2 <- ncdf4::nc_create(filename = f.cf, vars = var, verbose = FALSE)
      ncdf4::ncvar_put(nc = nc2, varid = "eastward_wind", vals = ew)
      
      var <- ncdf4::ncvar_def(name = "northward_wind", units = "m/s", dim = dim, missval = -6999, verbose = FALSE)
      nc2 <- ncdf4::ncvar_add(nc = nc2, v = var, verbose = FALSE)
      ncdf4::ncvar_put(nc = nc2, varid = "northward_wind", vals = nw)
      
      #---------------------------------------------------------------------#
      # Loop through variables and convert 
      
      vars.used.index.all <- setdiff(seq_along(format$vars$variable_id), format$time.row)
      nt <- setdiff(c("a","e"), treatment) 
      exclude.treatment <- paste0(nt,c("CO2","O3"))
      vars.used.index <- vars.used.index.all[!(format$vars$input_name[vars.used.index.all] %in% exclude.treatment)]
      
      derp <- grep(paste0(treatment,"CO2"), format$vars$input_name[vars.used.index])
      if(length(derp) >1){
        for(i in 2:length(derp)){
          vars.used.index <- vars.used.index[-derp[i]]
        }
      }
      derp <- grep(paste0(treatment,"O3"), format$vars$input_name[vars.used.index])
      if(length(derp) >1){
        for(i in 2:length(derp)){
          vars.used.index <- vars.used.index[-derp[i]]
        }
      }
      vars_used <- format$vars[vars.used.index, ]
      
      # begin loop
      for (i in seq_len(nrow(vars_used))) {
        vals <- ncdf4::ncvar_get(nc1, vars_used$input_name[i])
        
        if (vars_used$input_units[i] == vars_used$pecan_units[i]) {
          print("match")
        } else {
          u1 <- vars_used$input_units[i]
          u2 <- vars_used$pecan_units[i]
          if (units::ud_are_convertible(u1, u2)) {
            print(sprintf("convert %s %s to %s %s",
                          vars_used$input_name[i], vars_used$input_units[i], 
                          vars_used$pecan_name[i], vars_used$pecan_units[i]))
            vals <- PEcAn.utils::ud_convert(vals, u1, u2)
          } else if (PEcAn.utils::misc.are.convertible(u1, u2)) {
            print(sprintf("convert %s %s to %s %s", 
                          vars_used$input_name[i], u1, 
                          vars_used$pecan_name[i], u2))
            vals <- PEcAn.utils::misc.convert(x, u1, u2)
          } else {
            PEcAn.logger::logger.error("Units cannot be converted")
          } 
        }
        
        var <- ncdf4::ncvar_def(name = vars_used$pecan_name[i], 
                         units = vars_used$pecan_units[i], 
                         dim = dim, verbose = FALSE)
        nc2 <- ncdf4::ncvar_add(nc = nc2, v = var, verbose = FALSE)
        ncdf4::ncvar_put(nc = nc2, varid = vars_used$pecan_name[i], vals = vals)
        
        att <- ncdf4::ncatt_get(nc1,vars_used$input_name[i], "long_name")
        if (att$hasatt) {
          val <- att$value
          ncdf4::ncatt_put(nc = nc2, varid = vars_used$pecan_name[i], attname = "long_name", attval = val)
        }
      }
      ncdf4::nc_close(nc2)
    
      
      # Split into annual files
      
      year <- ncdf4::ncvar_get(nc1, "YEAR")
      y <- year[1]:year[length(year)]
      n <- length(y)
      t <- -1
      for (j in seq_len(n)) {
        new.file <- file.path(t.outfolder, paste(in.prefix, y[j],"nc", sep ="."))
        if (!file.exists(new.file)) {
          s <- t + 1
          print(s)
          e <- t + sum(year == y[j])
          print(e)
          if (file.exists(f.cf) == TRUE && file.exists(new.file) == FALSE) {
            system(paste0("ncks -d time,", s, ",", e, " ", f.cf, " ", new.file))
          }
        }
        t <- e
      }
      print(paste("Treatment ", treatment, " done"))

    } else {
      print(paste("Treatment ", treatment, " aleady done"))
    }  # end make new file
    file.remove(f.cf)
  }  # end loop over treatments
} # met2CF.FACE





# #####################################################################
# # HOW I PREVIOUSLY DID CONVERSIONS (I think it contains errors)
# # convert CO2 to mole_fraction_of_carbon_dioxide_in_air
# copyvals(nc1 = nc1, var1 = paste0(treatment, "CO2"), nc2 = nc2, 
#          var2 = "mole_fraction_of_carbon_dioxide_in_air", units2 = "mole/mole", 
#          dim2 = dim, conv = function(x) { x * 1e+06 },
#          verbose = verbose)
# 
# # deal with the rest of the variables
# 
# vars <- c("Rainf", "Tair", "RH", "VPD", "Qair", "Wind", "SWdown", "PAR", "LWdown", "Psurf", 
#           paste0(treatment, "O3"), "SolarElevation")
# 
# nvars <- c("precipitation_flux", "air_temperature", "relative_humidity", "water_vapor_saturation_deficit", 
#            "specific_humidity", "wind_speed", "surface_downwelling_shortwave_flux_in_air", 
#            "surface_downwelling_photosynthetic_radiative_flux_in_air", 
#            "surface_downwelling_longwave_flux_in_air", "air_pressure", 
#            "mass_concentration_of_ozone_in_air", 
#            "solar_elevation_angle")
# 
# if (!(length(nvars) == length(vars))) {
#  PEcAn.logger::logger.error("Variable mismatch")
# }
# 
# l <- length(vars)
# for (k in seq_len(l)) {
#   if (vars[k] %in% nc.vars) {
#     # nc <- tncar_rename(nc,vars[k],nvars[k])
#     
#     vals <- ncdf4::ncvar_get(nc1, vars[k])
#     
#     units <- ncdf4::ncatt_get(nc1, varid = vars[k], attname = "units", verbose = FALSE)$value
#     
#     var <- ncdf4::ncvar_def(name = nvars[k], units = units, dim = dim, verbose = FALSE)
#     nc2 <- ncdf4::ncvar_add(nc = nc2, v = var, verbose = TRUE)
#     ncdf4::ncvar_put(nc = nc2, varid = nvars[k], vals = vals)
#     
#     att <- ncdf4::ncatt_get(nc1, vars[k], "long_name")
#     if (att$hasatt) {
#       val <- att$value
#       ncdf4::ncatt_put(nc = nc2, varid = nvars[k], attname = "long_name", attval = val)
#     }
#   }
# }
# 
# ncdf4::nc_close(nc2)
