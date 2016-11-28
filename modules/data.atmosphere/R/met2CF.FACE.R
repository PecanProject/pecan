##' @name met2CF.FACE
##' @title met2CF.FACE
##' @export
##' 
##' @param in.path
##' @param in.prefix
##' @param outfolder
##' @param convert FACE files to CF files
##' @author Elizabeth Cowdery
##' 
met2CF.FACE <- function(in.path, in.prefix, outfolder, start_date, end_date, bety, ...) {
  
  library(ncdf4.helpers)
  library(PEcAn.utils)
  
  ncvar_get <- ncdf4::ncvar_get
  ncvar_def <- ncdf4::ncvar_def
  ncdim_def <- ncdf4::ncdim_def
  ncatt_get <- ncdf4::ncatt_get
  ncatt_put <- ncdf4::ncatt_put
  ncvar_add <- ncdf4::ncvar_add
  ncvar_put <- ncdf4::ncvar_put
  nc_open   <- ncdf4::nc_open
  nc_close   <- ncdf4::nc_close
  nc_create <- ncdf4::nc_create
  
  nc.get.variable.list <- ncdf4.helpers::nc.get.variable.list
  
  files <- dir(in.path)
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
      
      nc1 <- nc_open(f, write = TRUE)
      
      localhost <- fqdn()
      machineid <- dplyr::tbl(bety, "machines") %>% 
        filter(hostname == localhost) %>% 
        dplyr::select(id) %>% collect() %>% .[[1]]
      input.id <- dplyr::tbl(bety, "dbfiles") %>% filter(file_path == in.path) %>%
        filter(machine_id == machineid) %>% select(container_id) %>% collect() %>% .[[1]]
      site <- query.site(dplyr::tbl(bety, "inputs") %>% filter(id == input.id) %>% 
                           select(site_id) %>% collect() %>% .[[1]], bety$con)
      
      time_units <- paste0("hours/2", unlist(strsplit(nc1$var$TIMEstp$units, "timesteps"))[2])
      time <- ncdim_def(name = "time", units = time_units, vals = nc1$dim$tstep$vals)
      lon <- ncdim_def("longitude", "degrees_east", site$lon)  # define netCDF dimensions for variables
      lat <- ncdim_def("latitude", "degrees_north", site$lat)
      dim <- list(lat, lon, time)
      
      # convert wind speed and wind direction to eastward_wind and northward_wind
      wd <- 0  # wind direction - not specified so I set to 0???
      ws <- ncvar_get(nc = nc1, varid = "Wind")  #wind speed
      ew <- ws * cos(wd * (pi / 180))
      nw <- ws * sin(wd * (pi / 180))
      
      var <- ncvar_def(name = "eastward_wind", units = "m/s", dim = dim, missval = -6999, verbose = FALSE)
      nc2 <- nc_create(filename = f.cf, vars = var, verbose = FALSE)
      ncvar_put(nc = nc2, varid = "eastward_wind", vals = ew)
      
      var <- ncvar_def(name = "northward_wind", units = "m/s", dim = dim, missval = -6999, verbose = FALSE)
      nc2 <- ncvar_add(nc = nc2, v = var, verbose = FALSE)
      ncvar_put(nc = nc2, varid = "northward_wind", vals = nw)
      
      #---------------------------------------------------------------------#
      # Loop through variables and convert 
      
      format.id <- dplyr::tbl(bety, "inputs") %>% filter(id == input.id) %>%
        select(format_id) %>% collect() %>% .[[1]]
      
      format <- query.format.vars(bety,input.id, format.id)
      
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
        vals <- ncvar_get(nc1, vars_used$input_name[i])
        
        if (vars_used$input_units[i] == vars_used$pecan_units[i]) {
          print("match")
        } else {
          u1 <- vars_used$input_units[i]
          u2 <- vars_used$pecan_units[i]
          if (udunits2::ud.are.convertible(u1, u2)) {
            print(sprintf("convert %s %s to %s %s",
                          vars_used$input_name[i], vars_used$input_units[i], 
                          vars_used$pecan_name[i], vars_used$pecan_units[i]))
            vals <- udunits2::ud.convert(vals, u1, u2)
          } else if (misc.are.convertible(u1, u2)) {
            print(sprintf("convert %s %s to %s %s", 
                          vars_used$input_name[i], u1, 
                          vars_used$pecan_name[i], u2))
            vals <- misc.convert(x, u1, u2)
          } else {
            PEcAn.utils::logger.error("Units cannot be converted")
          } 
        }
        
        var <- ncvar_def(name = vars_used$pecan_name[i], 
                         units = vars_used$pecan_units[i], 
                         dim = dim, verbose = FALSE)
        nc2 <- ncvar_add(nc = nc2, v = var, verbose = FALSE)
        ncvar_put(nc = nc2, varid = vars_used$pecan_name[i], vals = vals)
        
        att <- ncatt_get(nc1,vars_used$input_name[i], "long_name")
        if (att$hasatt) {
          val <- att$value
          ncatt_put(nc = nc2, varid = vars_used$pecan_name[i], attname = "long_name", attval = val)
        }
      }
      nc_close(nc2)
    
      
      # Split into annual files
      
      year <- ncvar_get(nc1, "YEAR")
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
#   logger.error("Variable mismatch")
# }
# 
# l <- length(vars)
# for (k in seq_len(l)) {
#   if (vars[k] %in% nc.vars) {
#     # nc <- tncar_rename(nc,vars[k],nvars[k])
#     
#     vals <- ncvar_get(nc1, vars[k])
#     
#     units <- ncatt_get(nc1, varid = vars[k], attname = "units", verbose = FALSE)$value
#     
#     var <- ncvar_def(name = nvars[k], units = units, dim = dim, verbose = FALSE)
#     nc2 <- ncvar_add(nc = nc2, v = var, verbose = TRUE)
#     ncvar_put(nc = nc2, varid = nvars[k], vals = vals)
#     
#     att <- ncatt_get(nc1, vars[k], "long_name")
#     if (att$hasatt) {
#       val <- att$value
#       ncatt_put(nc = nc2, varid = nvars[k], attname = "long_name", attval = val)
#     }
#   }
# }
# 
# ncdf4::nc_close(nc2)
