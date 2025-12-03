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
extract_soil_gssurgo <- function(outdir, lat = NULL, lon = NULL, aoi = NULL, size = 1, radius = 500, depths = c(0, 0.15, 0.30, 0.60)){
  all.soil.ens <- list()
  
  # Validate inputs
  if (is.null(aoi) && (is.null(lat) || is.null(lon))) {
    PEcAn.logger::logger.severe("Must provide either 'aoi' OR both 'lat' and 'lon'")
  }
  
  # Create AOI from point + radius if needed
  if (is.null(aoi)) {
    aoi <- data.frame(lon = lon, lat = lat) %>%
      terra::vect(crs = "epsg:4326") %>%
      terra::buffer(width = radius)

  }

  # Validate depths parameter (must start with 0, like hist() breaks)
  if (depths[1] != 0) {
    PEcAn.logger::logger.severe(
      "First depth must be 0. Use depths = c(0, 0.15, 0.30, ...) like hist() breaks. ",
      "This creates n layers from n+1 breakpoints."
    )
  }

  PEcAn.logger::logger.info("Querying gSSURGO Web Coverage Service for map unit keys")
  mu_raster <- soilDB::mukey.wcs(aoi = aoi, db = 'gSSURGO', res = 30)
  
  mukey_values <- terra::values(mu_raster)
  mukey_values <- mukey_values[!is.na(mukey_values)]
  mukey_counts <- table(mukey_values)
  mukeys_all <- as.character(names(mukey_counts))
  
  if (length(mukeys_all) == 0) {
    PEcAn.logger::logger.severe("No mapunit keys were found for this site.")
  }
  
  # Fetch complete soil data
  sda_data <- tryCatch({
    soilDB::fetchSDA(
      WHERE = paste0("mukey IN (", paste(mukeys_all, collapse = ","), ")"),
      duplicates = TRUE,
      childs = TRUE,
      nullFragsAreZero = TRUE,
      rmHzErrors = TRUE
    )
  }, error = function(e) {
    PEcAn.logger::logger.error(paste("Failed to fetch SDA data:", e$message))
    return(NULL)
  })
  
  if (is.null(sda_data)) {
    PEcAn.logger::logger.error("Could not retrieve soil data from SDA")
    return(NULL)
  }
  
  hz_data <- aqp::horizons(sda_data)
  site_data <- aqp::site(sda_data)
  
  # Component-level aggregation by depth
  depths_cm <- depths * 100
  all_soil_data <- list()
  
  # loop through depth intervals (n+1 breaks --> n intervals, like hist())
  for (i in seq_len(length(depths_cm) - 1)) {
    top_depth <- depths_cm[i]
    bottom_depth <- depths_cm[i + 1]
    
    depth_hz <- hz_data %>%
      dplyr::filter(hzdept_r < bottom_depth & hzdepb_r > top_depth)
    
    if (nrow(depth_hz) == 0) next
    
    # Aggregate by COMPONENT (preserves within-mapunit variability)
    component_data <- depth_hz %>%
      dplyr::left_join(site_data[, c("cokey", "comppct_r", "mukey")], by = "cokey") %>%
      dplyr::mutate(
        hz_top_adj = pmax(hzdept_r, top_depth),
        hz_bot_adj = pmin(hzdepb_r, bottom_depth),
        hz_thickness = hz_bot_adj - hz_top_adj
      ) %>%
      dplyr::group_by(mukey, cokey, comppct_r) %>%
      dplyr::summarise(
        sandtotal_r = stats::weighted.mean(sandtotal_r, hz_thickness, na.rm = TRUE),
        silttotal_r = stats::weighted.mean(silttotal_r, hz_thickness, na.rm = TRUE),
        claytotal_r = stats::weighted.mean(claytotal_r, hz_thickness, na.rm = TRUE),
        om_r = stats::weighted.mean(om_r, hz_thickness, na.rm = TRUE),
        dbthirdbar_r = stats::weighted.mean(dbthirdbar_r, hz_thickness, na.rm = TRUE),
        fragvol_r = stats::weighted.mean(fragvol_r, hz_thickness, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        tex_sum    = sandtotal_r + silttotal_r + claytotal_r,
        sandtotal_r = sandtotal_r / tex_sum * 100,
        silttotal_r = silttotal_r / tex_sum * 100,
        claytotal_r = claytotal_r / tex_sum * 100
      ) %>%
      dplyr::select(-tex_sum) %>%
      dplyr::mutate(
        hzdept_r = top_depth,
        hzdepb_r = bottom_depth
      )
    
    all_soil_data[[i]] <- component_data
  }
  
  soilprop <- do.call(rbind, all_soil_data)
  
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
    fraction_of_sand_in_soil = soilprop.new$fraction_of_sand_in_soil,
    fraction_of_silt_in_soil = soilprop.new$fraction_of_silt_in_soil,
    fraction_of_clay_in_soil = soilprop.new$fraction_of_clay_in_soil,
    soil_depth = soilprop.new$soil_depth,
    soil_organic_carbon_stock = soilprop.new$soil_organic_carbon_stock
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
          texture_data <- DepthL.Data[,c("fraction_of_sand_in_soil",
                                         "fraction_of_silt_in_soil",
                                         "fraction_of_clay_in_soil")] %>%
            as.matrix()
          
          if(nrow(texture_data) == 0) return(NULL)
          
          # Fit Dirichlet on component textures
          dir.model <- sirt::dirichlet.mle(texture_data)
          alpha <- matrix(dir.model$alpha, nrow = size, ncol = length(dir.model$alpha), byrow = TRUE)
          simulated.soil <- sirt::dirichlet.simul(alpha)
          
          # Component-weighted SOC
          soc_values <- DepthL.Data$soil_organic_carbon_stock
          weights <- DepthL.Data$comppct_r / sum(DepthL.Data$comppct_r)
          
          soc_mean <- stats::weighted.mean(soc_values, weights)
          soc_sd <- sqrt(stats::weighted.mean((soc_values - soc_mean)^2, weights))
          
          if (is.na(soc_sd) || soc_sd == 0) {
            # No variability - use mean value (preserves data for single observations)
            simulated_soc <- rep(soc_mean, size)
          } else {
            # Has variability - sample from gamma distribution
            shape <- (soc_mean^2) / (soc_sd^2)
            rate <- soc_mean / (soc_sd^2)
            simulated_soc <- stats::rgamma(size, shape = shape, rate = rate)
          }
          
          result_df <- data.frame(
            fraction_of_sand_in_soil = simulated.soil[,1],
            fraction_of_silt_in_soil = simulated.soil[,2],
            fraction_of_clay_in_soil = simulated.soil[,3],
            soil_depth = DepthL.Data$soil_depth[1],
            mukey = unique(DepthL.Data$mukey),
            soil_organic_carbon_stock = simulated_soc
          )
          
          return(result_df)
        },
        error = function(e) {
          PEcAn.logger::logger.warn(conditionMessage(e))
          return(NULL)
        })
      })
    
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