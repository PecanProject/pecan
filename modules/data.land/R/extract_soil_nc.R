#' Extract soil data from gssurgo
#' @details This function extracts soil property data from the USDA gSSURGO database
#' for a specified area of interest. It can work with either a lat/lon point 
#' (creating a circular buffer) or a custom polygon AOI.
#'
#' @param outdir Output directory for writing NetCDF files
#' @param lat Latitude of center point (optional if aoi provided)
#' @param lon Longitude of center point (optional if aoi provided)
#' @param aoi Custom area of interest as sf or terra polygon (optional)
#' @param size Ensemble size (number of ensemble members to generate)
#' @param radius Buffer radius in meters around lat/lon point (default: 500)
#' @param depths Soil depth breakpoints in meters, must start with 0 (default: c(0, 0.15, 0.30, 0.60))
#'
#' @return It returns the address for the generated soil netcdf file
#'
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#'    outdir  <- "~/paleon/envTest"
#'    lat     <- 40
#'    lon     <- -80
#'    PEcAn.data.land::extract_soil_gssurgo(outdir, lat, lon)
#' }
#' @author Hamze Dokoohaki, Akash
#' @export
#'  
extract_soil_gssurgo <- function(outdir, lat = NULL, lon = NULL, aoi = NULL, 
                                  size = 1, radius = 500, 
                                  depths = c(0, 0.15, 0.30, 0.60)) {
  all.soil.ens <- list()
  
  fetch_result <- gssurgo_fetch_area(
    lat = lat, lon = lon, aoi = aoi, 
    radius = radius, depths = depths
  )
  
  if (is.null(fetch_result)) {
    return(NULL)
  }
  
  soilprop <- fetch_result$soilprop
  mukey_counts <- fetch_result$mukey_counts
  depths_cm <- fetch_result$depths_cm
  
  soilprop.new <- soilprop %>%
    dplyr::select(
      fraction_of_sand_in_soil = "sandtotal_r",
      fraction_of_silt_in_soil = "silttotal_r",
      fraction_of_clay_in_soil = "claytotal_r",
      soil_depth = "hzdept_r",
      soil_depth_bottom = "hzdepb_r",
      organic_matter_pct = "om_r",
      bulk_density = "dbthirdbar_r",
      coarse_fragment_pct = "fragvol_r",
      mukey = "mukey",
      cokey = "cokey",
      comppct_r = "comppct_r"
    ) %>%
    dplyr::mutate(
      dplyr::across(c(dplyr::starts_with("fraction_of"), "coarse_fragment_pct"), 
                    ~ . / 100),
      coarse_fragment_pct = ifelse(is.na(coarse_fragment_pct), 0, coarse_fragment_pct),
      horizon_thickness_cm = .data$soil_depth_bottom - .data$soil_depth,
      soil_organic_carbon_stock = PEcAn.data.land::soc2ocs(
        soc_percent = PEcAn.data.land::om2soc(.data$organic_matter_pct),
        bulk_density = .data$bulk_density,
        thickness = .data$horizon_thickness_cm,
        coarse_fraction = .data$coarse_fragment_pct
      )
    ) %>%
    dplyr::filter(stats::complete.cases(.))
  
  if(nrow(soilprop.new) == 0) {
    PEcAn.logger::logger.error("No valid soil properties after filtering")
    return(NULL)
  }
  
  if(!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  soil.data.gssurgo <- list(
    fraction_of_sand_in_soil = as.numeric(soilprop.new$fraction_of_sand_in_soil),
    fraction_of_silt_in_soil = as.numeric(soilprop.new$fraction_of_silt_in_soil),
    fraction_of_clay_in_soil = as.numeric(soilprop.new$fraction_of_clay_in_soil),
    soil_depth = as.numeric(soilprop.new$soil_depth / 100),  # cm to meters
    soil_organic_carbon_stock = as.numeric(soilprop.new$soil_organic_carbon_stock),
    soil_bulk_density = PEcAn.utils::ud_convert(
      as.numeric(soilprop.new$bulk_density), "g cm-3", "kg m-3"
    )
  )
  all.soil.ens <- c(all.soil.ens, list(soil.data.gssurgo))
  
  # Generate modeled ensembles
  tryCatch({
    depth.levs <- findInterval(soilprop.new$soil_depth_bottom, depths_cm)
    depth.levs[depth.levs == 0] <- 1
    depth.levs[depth.levs > length(depths_cm)] <- length(depths_cm)
    
    valid_indices <- !is.na(depth.levs)
    if(sum(!valid_indices) > 0) {
      soilprop.new <- soilprop.new[valid_indices, ]
      depth.levs <- depth.levs[valid_indices]
    }
    
    soilprop.new.grouped <- soilprop.new %>%
      dplyr::mutate(DepthL = depths_cm[depth.levs])
    
    # Dirichlet modeling per mukey AND depth (component-level)
    simulated.soil.props <- soilprop.new.grouped %>%
      split(list(soilprop.new.grouped$DepthL, soilprop.new.grouped$mukey)) %>%
      purrr::map_df(function(DepthL.Data){
        tryCatch({
          texture_data <- DepthL.Data[, c("fraction_of_sand_in_soil",
                                          "fraction_of_silt_in_soil",
                                          "fraction_of_clay_in_soil")]
          
          # handle single-component soils
          if (nrow(texture_data) == 0) {
            # No data, can't fit anything
            return(NULL)
          } else if (nrow(texture_data) == 1) {
            # Only one component - can't estimate variability
            if (size == 1) {
              # For point estimates, return the mean values directly
              result_df <- data.frame(
                fraction_of_sand_in_soil = texture_data$fraction_of_sand_in_soil,
                fraction_of_silt_in_soil = texture_data$fraction_of_silt_in_soil,
                fraction_of_clay_in_soil = texture_data$fraction_of_clay_in_soil,
                soil_depth = DepthL.Data$soil_depth[1],
                mukey = unique(DepthL.Data$mukey),
                soil_organic_carbon_stock = DepthL.Data$soil_organic_carbon_stock[1],
                bulk_density = DepthL.Data$bulk_density[1]
              )
              return(result_df)
            } else {
              # For ensembles, we cannot generate variability without data
              PEcAn.logger::logger.warn(
                paste0("Mapunit ", unique(DepthL.Data$mukey), 
                       " at depth ", DepthL.Data$soil_depth[1], 
                       " has only one component. Cannot estimate texture variability. ",
                       "Consider using size=1 or a larger search radius.")
              )
              return(NULL)
            }
          } else {
            # Multiple components - fit Dirichlet
            dir.model <- sirt::dirichlet.mle(texture_data)
            alpha <- matrix(dir.model$alpha, nrow = size, ncol = length(dir.model$alpha), byrow = TRUE)
            simulated.soil <- sirt::dirichlet.simul(alpha)
          }
          
          # Component-weighted SOC
          weights <- DepthL.Data$comppct_r / sum(DepthL.Data$comppct_r)
          soc_values <- DepthL.Data$soil_organic_carbon_stock
          
          soc_mean <- stats::weighted.mean(soc_values, weights, na.rm = TRUE)
          soc_sd <- sqrt(stats::weighted.mean((soc_values - soc_mean)^2, weights, na.rm = TRUE))
          
          if (is.na(soc_sd) || soc_sd == 0) {
            simulated_soc <- rep(soc_mean, size)
          } else {
            shape <- (soc_mean^2) / (soc_sd^2)
            rate <- soc_mean / (soc_sd^2)
            simulated_soc <- stats::rgamma(size, shape = shape, rate = rate)
          }
          
          # bulk density gamma-sampling
          bd_values <- DepthL.Data$bulk_density
          bd_mean <- stats::weighted.mean(bd_values, weights, na.rm = TRUE)
          bd_sd <- sqrt(stats::weighted.mean((bd_values - bd_mean)^2, weights, na.rm = TRUE))
          
          if (is.na(bd_sd) || bd_sd == 0 || is.na(bd_mean)) {
            simulated_bd <- rep(bd_mean, size)
          } else {
            shape <- (bd_mean^2) / (bd_sd^2)
            rate <- bd_mean / (bd_sd^2)
            simulated_bd <- stats::rgamma(size, shape = shape, rate = rate)
          }
          
          # Handle case where Dirichlet fit succeeded (size > 1, multiple components)
          if (nrow(texture_data) > 1) {
            result_df <- data.frame(
              fraction_of_sand_in_soil = simulated.soil[,1],
              fraction_of_silt_in_soil = simulated.soil[,2],
              fraction_of_clay_in_soil = simulated.soil[,3],
              soil_depth = DepthL.Data$soil_depth[1],
              mukey = unique(DepthL.Data$mukey),
              soil_organic_carbon_stock = simulated_soc,
              bulk_density = simulated_bd
            )
          } else {
            # Single component, size=1 case already returned above
            result_df <- NULL
          }
          
          return(result_df)
        },
        error = function(e) {
          PEcAn.logger::logger.warn(conditionMessage(e))
          return(NULL)
        })
      })
    
    # Validate simulated.soil.props before filtering
    if (is.null(simulated.soil.props) || nrow(simulated.soil.props) == 0 || 
        !("mukey" %in% names(simulated.soil.props))) {
      PEcAn.logger::logger.warn(
        "No valid simulated soil properties generated. ",
        "This may occur when all soil types have only one component. ",
        "Returning only the unsampled best-estimate ensemble member."
      )
      # Skip ensemble generation, proceed directly to NetCDF output
    } else {
      # Calculate area weights
      mukey_area <- data.frame(
        mukey = names(mukey_counts),
        Area = as.numeric(mukey_counts) / sum(mukey_counts)
      ) %>%
        dplyr::filter(.data$mukey %in% unique(simulated.soil.props$mukey)) %>%
        dplyr::mutate(Area = .data$Area / sum(.data$Area, na.rm = TRUE))
      
      # Generate weighted profiles
      soil.profiles <- simulated.soil.props %>%
        split(.$mukey) %>%
        purrr::map(function(soiltype.sim){
          sizein <- mukey_area$Area[mukey_area$mukey == unique(soiltype.sim$mukey)] * size
          
          1:ceiling(sizein) %>%
            purrr::map(function(x){
              soiltype.sim %>%
                split(.$soil_depth) %>%
                purrr::map_dfr(~.x[x,])
            })
        }) %>%
        purrr::flatten()
      
      # Convert to ensemble arrays
      all.soil.ens <- soil.profiles %>%
        purrr::map(function(SEns){
          SEns <- SEns[, names(SEns) != "mukey"]
          names(SEns) %>%
            purrr::map(function(var){
              as.numeric(unlist(SEns[, var]))
            }) %>%
            stats::setNames(names(SEns))
        }) %>%
        c(all.soil.ens, .)
    }
    
  },
  error = function(e) {
    PEcAn.logger::logger.warn(conditionMessage(e))
  })
  
  # Generate NetCDF files
  out.ense <- (1:length(all.soil.ens)) %>%
    purrr::map(function(i) {
      tryCatch({
        prefix <- paste0("gSSURGO_soil_", i)
        new.file <- file.path(outdir, paste0(prefix, ".nc"))
        
        if ("bulk_density" %in% names(all.soil.ens[[i]])) {
          all.soil.ens[[i]]$soil_bulk_density <- PEcAn.utils::ud_convert(
            all.soil.ens[[i]]$bulk_density, "g cm-3", "kg m-3"
          )
          all.soil.ens[[i]]$bulk_density <- NULL
        }
        
        suppressWarnings({
          PEcAn.data.land::soil2netcdf(all.soil.ens[[i]], new.file)
        })
        new.file
      },
      error = function(e) {
        PEcAn.logger::logger.warn(conditionMessage(e))
        return(NULL)
      })
    }) %>%
    purrr::discard(is.null) %>%
    stats::setNames(rep("path", length(.)))
  
  return(out.ense)
}


#' Extract soil data from the gridpoint closest to a location
#'
#' @param in.file path to netcdf file containing soil data
#' @param outdir directory in which to write netcdf file of extracted data.
#'  Output filename will be the same as input filename.
#' @param lat,lon location in decimal degrees.
#'  Data will be extracted from the point in `in.file` that is nearest this
#'
#' @return path to netCDF file containing extracted data
#' @export
#'
#' @examples
#' \dontrun{
#' in.file <- "~/paleon/env_paleon/soil/paleon_soil.nc"
#' outdir  <- "~/paleon/envTest"
#' lat     <- 40
#' lon     <- -80
#' PEcAn.data.land::extract_soil_nc(in.file,outdir,lat,lon)
#' }
extract_soil_nc <- function(in.file,outdir,lat,lon){
  
  ## open soils
  nc <- ncdf4::nc_open(in.file)
  
  ## extract lat/lon
  dims <- names(nc$dim)
  lat.dim <- dims[grep("^lat",dims)]
  lon.dim <- dims[grep("^lon",dims)]
  soil.lat <- ncdf4::ncvar_get(nc, lat.dim)
  soil.lon <- ncdf4::ncvar_get(nc, lon.dim)
  
  ## check in range
  dlat <- abs(stats::median(diff(soil.lat)))
  dlon <- abs(stats::median(diff(soil.lon)))
  if(lat < (min(soil.lat)-dlat) | lat > (max(soil.lat)+dlat)){
    PEcAn.logger::logger.error("site lat out of bounds",lat,range(soil.lat))
  }
  if(lon < (min(soil.lon)-dlon) | lon > (max(soil.lon)+dlon)){
    PEcAn.logger::logger.error("site lon out of bounds",lon,range(soil.lon))
  }
  if(dims[1] == lat.dim){
    soil.row <- which.min(abs(lat-soil.lat))
    soil.col <- which.min(abs(lon-soil.lon))
  } else if(dims[1] == lon.dim){
    soil.col <- which.min(abs(lat-soil.lat))
    soil.row <- which.min(abs(lon-soil.lon))
  } else {
    PEcAn.logger::logger.error("could not determine lat/lon dimension order:: ",dims)
  }
  
  ## extract raw soil data
  soil.data <- list()
  soil.vars <- names(nc$var)
  for(i in seq_along(soil.vars)){
    if(length(dims) == 2){
      soil.data[[soil.vars[i]]] <- ncdf4::ncvar_get(nc,soil.vars[i])[soil.row,soil.col]
    } else {
      ## assuming there's a 3rd dim of soil depth profile
      soil.data[[soil.vars[i]]] <- ncdf4::ncvar_get(nc,soil.vars[i])[soil.row,soil.col,]
    }
  }
  ncdf4::nc_close(nc)
  
  ## PalEON / MSTMIP / UNASM hack
  # t_ variables are topsoil layer (0– 30 cm) and
  # s_ variables are subsoil layer (30–100 cm)
  depth <- ncdf4::ncdim_def(name = "depth", units = "meters", vals = c(0.3,1.0), create_dimvar = TRUE)  
  dvars <- soil.vars[grep("t_",soil.vars,fixed=TRUE)]
  for(i in seq_along(dvars)){
    svar <- sub("t_","s_",dvars[i])
    soil.data[[dvars[i]]] <- c(soil.data[[dvars[i]]],soil.data[[svar]]) ## combine different depths
    soil.data[[svar]] <- NULL  ## drop old variable
    names(soil.data)[which(names(soil.data) == dvars[i])] <- sub("t_","",dvars[i]) ## rename original
  }
  
  
  ## name/unit conversions 
  soil.data$sand   <- soil.data$sand/100
  soil.data$silt   <- soil.data$silt/100
  soil.data$clay   <- soil.data$clay/100
  soil.data$oc     <- soil.data$oc/100
  soil.data$gravel <- soil.data$gravel/100
  soil.data$ref_bulk <- PEcAn.utils::ud_convert(soil.data$ref_bulk,"g cm-3","kg m-3")
  names(soil.data)[which(names(soil.data) == "clay")] <- "fraction_of_clay_in_soil"
  names(soil.data)[which(names(soil.data) == "sand")] <- "fraction_of_sand_in_soil"
  names(soil.data)[which(names(soil.data) == "silt")] <- "fraction_of_silt_in_soil"
  names(soil.data)[which(names(soil.data) == "gravel")] <- "fraction_of_gravel_in_soil"
  names(soil.data)[which(names(soil.data) == "ref_bulk")] <- "soil_bulk_density"
  names(soil.data)[which(names(soil.data) == "ph")]   <- "soil_ph"
  names(soil.data)[which(names(soil.data) == "cec")]  <- "soil_cec" ## units = meq/100g
  names(soil.data)[which(names(soil.data) == "oc")]   <- "soilC"  ## this is currently the BETY name, would like to change and make units SI
  
  ## calc new filename
  prefix <- tools::file_path_sans_ext(basename(in.file))
  new.file <- file.path(outdir,paste0(prefix,".nc"))
  
  ## Calculate soil parameters and export to netcdf
  PEcAn.data.land::soil2netcdf(soil.data,new.file)
  
  return(new.file)
  
}


#' Get standard units for a soil variable
#'
#' Given SSURGO names for soil properties, looks up their standard units.
#' Note that names must match exactly.
#'
#' Supported variables are:
#'  * `soil_depth`
#'  * `soil_cec`
#'  * `fraction_of_clay_in_soil`
#'  * `fraction_of_sand_in_soil`
#'  * `fraction_of_silt_in_soil`
#'  * `fraction_of_gravel_in_soil`
#'  * `volume_fraction_of_water_in_soil_at_saturation`
#'  * `volume_fraction_of_water_in_soil_at_field_capacity`
#'  * `volume_fraction_of_condensed_water_in_dry_soil`
#'  * `volume_fraction_of_condensed_water_in_soil_at_wilting_point`
#'  * `soilC`
#'  * `soil_ph`
#'  * `soil_bulk_density`
#'  * `soil_type`
#'  * `soil_hydraulic_b`
#'  * `soil_water_potential_at_saturation`
#'  * `soil_hydraulic_conductivity_at_saturation`
#'  * `thcond0`
#'  * `thcond1`
#'  * `thcond2`
#'  * `thcond3`
#'  * `soil_thermal_conductivity`
#'  * `soil_thermal_conductivity_at_saturation`
#'  * `soil_thermal_capacity`
#'  * `soil_albedo`
#'  * `slpotwp`
#'  * `slpotcp`
#'  * `slcpd`
#'  * `slden`
#'  * `soil_organic_carbon_stock`
#'
#' @param varname character vector. See details
#'
#' @return character matrix with columns `var` and `unit`
#' @md
#' @export
#'
#' @examples
#' soil.units("soil_albedo")
soil.units <- function(varname = NA){
  variables <- as.data.frame(matrix(c("soil_depth","m",
                                      "soil_cec","meq/100g",
                                      "fraction_of_clay_in_soil","1",
                                      "fraction_of_sand_in_soil","1",
                                      "fraction_of_silt_in_soil","1",
                                      "fraction_of_gravel_in_soil","1",
                                      "volume_fraction_of_water_in_soil_at_saturation","m3 m-3",
                                      "volume_fraction_of_water_in_soil_at_field_capacity","m3 m-3",
                                      "volume_fraction_of_condensed_water_in_dry_soil","m3 m-3",
                                      "volume_fraction_of_condensed_water_in_soil_at_wilting_point","m3 m-3",
                                      "soilC","percent",
                                      "soil_ph","1",
                                      "soil_bulk_density","kg m-3",
                                      "soil_type","string",
                                      "soil_hydraulic_b","1",
                                      "soil_water_potential_at_saturation","m",
                                      "soil_hydraulic_conductivity_at_saturation","m s-1",
                                      "thcond0","W m-1 K-1",
                                      "thcond1","W m-1 K-1",
                                      "thcond2","1",
                                      "thcond3","1",
                                      "soil_thermal_conductivity","W m-1 K-1", 
                                      "soil_thermal_conductivity_at_saturation","W m-1 K-1", 
                                      "soil_thermal_capacity","J kg-1 K-1",
                                      "soil_albedo","1",
                                      "slpotwp","m",
                                      "slpotcp","m",
                                      "slcpd","J m-3 K-1",
                                      "slden","kg m-3",
                                      "soil_organic_carbon_stock","kg m-2"
    ),
  ncol=2,byrow = TRUE))
  colnames(variables) <- c('var','unit')
  
  unit = which(variables$var == varname)
  
  if(length(unit) == 0){
    if(is.na(varname)){
      return(variables)
    } else {
      return(NA)
    }
  }else{
    unit = as.character(variables$unit[unit])
    return(unit)
  }
  
}