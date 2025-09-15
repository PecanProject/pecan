#' Extract soil data from gssurgo
#' @details This function takes a single lat/lon point and creates a spatial grid 
#' around it for sampling soil variability. The grid_size parameter determines 
#' how many grid points (grid_size x grid_size) are created around the center point.
#'
#' @param outdir Output directory for writing down the netcdf file
#' @param lat Latitude of center point (single numeric value)
#' @param lon Longitude of center point (single numeric value) 
#' @param size Ensemble size
#' @param grid_size Size of the spatial sampling grid around the center point (default: 3)
#' @param grid_spacing Spacing between grid cells in meters (default: 100)
#' @param depths Standard set of soil depths in m to create the ensemble of soil profiles with.
#'
#' @return It returns the address for the generated soil netcdf file
#'
#' @section Current Limitations:
#' - MUKEY frequency weighting treats occurrence counts as proportional to area coverage
#' - This approximation may introduce geometric bias for irregular polygon data
#' - Buffer radius is set to grid_spacing/2 to reduce overlapping queries, but may still miss coverage
#' - True area-weighted aggregation using polygon geometries is planned (see issue #3609)
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
extract_soil_gssurgo <- function(outdir, lat, lon, size=1, grid_size=3, grid_spacing=100, depths=c(0.15,0.30,0.60)){
  # I keep all the ensembles here 
  all.soil.ens <-list()

  # Grid-based spatial sampling around the center point (via WFS queries)
  # This creates a grid_size x grid_size sampling grid centered on lat/lon
  proj_crs <- sf::st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  wgs84_crs <- sf::st_crs(4326)
  
  # Convert single center lat/lon to projected coordinates
  point_sf <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = wgs84_crs)
  point_proj <- sf::st_transform(point_sf, proj_crs)
  coords_proj <- sf::st_coordinates(point_proj)
  
  # Define grid extent 
  half_extent <- (grid_size - 1) / 2 * grid_spacing
  xmin <- coords_proj[1] - half_extent
  xmax <- coords_proj[1] + half_extent
  ymin <- coords_proj[2] - half_extent
  ymax <- coords_proj[2] + half_extent
  
  # Create raster template
  raster_template <- terra::rast(
    xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
    resolution = grid_spacing, crs = proj_crs$wkt
  )
  grid_coords <- terra::crds(raster_template)
  
  # Transform grid coordinates back to WGS84 for gSSURGO queries
  grid_sf <- sf::st_as_sf(data.frame(x = grid_coords[, 1], y = grid_coords[, 2]),
                          coords = c("x", "y"), crs = proj_crs)
  grid_wgs84 <- sf::st_transform(grid_sf, wgs84_crs)
  grid_coords_wgs84 <- sf::st_coordinates(grid_wgs84)
  
  # Query gSSURGO for each grid point to capture spatial variability
  buffer_radius <- grid_spacing / 2
  PEcAn.logger::logger.warn(
    "Buffer radius set to grid_spacing/2 to avoid overlap",
    "results may be biased due to lack of area weighting and incomplete spatial coverage."
  )
  mukeys_all <- c()
  for (i in seq_len(nrow(grid_coords_wgs84))) {
    # Extract coordinates for this grid point (not user input)
    this_lon <- grid_coords_wgs84[i, 1]
    this_lat <- grid_coords_wgs84[i, 2]
    
    # I ask the gSSURGO to find all the mukeys (loosely can be thought of soil type) within grid_spacing distance of each grid point location. 
    # Basically I think of this as me going around and taking soil samples at each grid point.
    #https://sdmdataaccess.nrcs.usda.gov/SpatialFilterHelp.htm
    mu.Path <- paste0(
      "https://sdmdataaccess.nrcs.usda.gov/Spatial/SDMWGS84Geographic.wfs?",
      "SERVICE=WFS",
      "&VERSION=1.1.0",
      "&REQUEST=GetFeature&TYPENAME=MapunitPoly",
      "&FILTER=",
        "<Filter>",
          "<DWithin>",
            "<PropertyName>Geometry</PropertyName>",
            "<gml:Point>",
              "<gml:coordinates>", this_lon, ",", this_lat, "</gml:coordinates>",
            "</gml:Point>",
            "<Distance%20units=%27m%27>", buffer_radius, "</Distance>",
          "</DWithin>",
        "</Filter>",
      "&OUTPUTFORMAT=XMLMukeyList"
    )
    
    # XML handling with temp file
    temp_file <- tempfile(fileext = ".xml")
    xmll <- curl::curl_download(
      mu.Path,
      destfile = temp_file,
      handle = curl::new_handle(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE)
    )
    
    # mukey extraction with error recovery
    mukey_str <- tryCatch({
      xml_doc <- XML::xmlParse(temp_file)
      mapunit_nodes <- XML::getNodeSet(xml_doc, "//MapUnitKeyList")
      
      if (length(mapunit_nodes) > 0) {
        mukey_data <- XML::xmlValue(mapunit_nodes[[1]])
        if (!is.null(mukey_data) && nchar(trimws(mukey_data)) > 0) {
          mukey_data
        } else {
          PEcAn.logger::logger.debug(paste("Empty MapUnitKeyList for coordinates", 
                                           this_lat, ",", this_lon))
          NULL
        }
      } else {
        PEcAn.logger::logger.debug(paste("No MapUnitKeyList found for coordinates", 
                                         this_lat, ",", this_lon, "skipping grid point"))
        NULL
      }
    }, error = function(e) {
      PEcAn.logger::logger.warn(paste("Failed to parse gSSURGO response for coordinates", 
                                      this_lat, ",", this_lon, ":", e$message))
      NULL
    })
    if (file.exists(temp_file)) unlink(temp_file)
    if (is.null(mukey_str)) next
    
    mukeys <- strsplit(mukey_str, ",")[[1]]
    if (length(mukeys) == 0) next
    
    mukeys_all <- c(mukeys_all, mukeys)
  }

  # mukey occurrences across all grid points
  mukey_counts <- table(mukeys_all)
  # Get unique mukeys from all grid points
  mukeys_all <- unique(mukeys_all)
  if (length(mukeys_all) == 0) {
    PEcAn.logger::logger.severe("No mapunit keys were found for this site.")
    return(NULL)
  }
  
  # calling the query function sending the mapunit keys
  soilprop <- gSSURGO.Query(
    mukeys_all,
    c("chorizon.sandtotal_r",
      "chorizon.silttotal_r",
      "chorizon.claytotal_r",
      "chorizon.hzdept_r",
      "chorizon.hzdepb_r",  
      "chorizon.om_r",      
      "chorizon.dbthirdbar_r",  # bulk density at 1/3 bar (field capacity);which is the standard field capacity bulk density measurement
      "chfrags.fragvol_r",
      "component.comppct_r")) 
  
  # Two-step aggregation:
  # (1) Sum fragments within horizons, (2) Component area-weighting by mapunit
  soilprop.weighted <- soilprop %>%
    dplyr::group_by(.data$cokey, .data$hzdept_r, .data$hzdepb_r) %>%
    # Each horizon may have multiple rows from different fragment size classes
    # Sum fragments across size classes and remove duplicate horizon data
    dplyr::mutate(fragvol_r = min(sum(.data$fragvol_r, na.rm = TRUE), 100)) %>%
    dplyr::distinct() %>% # Remove duplicate rows created by multiple fragment size classes
    dplyr::ungroup() %>%
    # Component area-weighted aggregation by mapunit and horizon depth
    dplyr::group_by(.data$mukey, .data$hzdept_r, .data$hzdepb_r) %>%
    dplyr::summarise(
      sandtotal_r = stats::weighted.mean(.data$sandtotal_r, .data$comppct_r, na.rm = TRUE),
      silttotal_r = stats::weighted.mean(.data$silttotal_r, .data$comppct_r, na.rm = TRUE),
      claytotal_r = stats::weighted.mean(.data$claytotal_r, .data$comppct_r, na.rm = TRUE),
      om_r = stats::weighted.mean(.data$om_r, .data$comppct_r, na.rm = TRUE),
      dbthirdbar_r = stats::weighted.mean(.data$dbthirdbar_r, .data$comppct_r, na.rm = TRUE),
      fragvol_r = stats::weighted.mean(.data$fragvol_r, .data$comppct_r, na.rm = TRUE),
      .groups = "drop"
    )
  
  soilprop.new <- soilprop.weighted %>%
    dplyr::arrange(.data$hzdept_r) %>%
    dplyr::select(
      fraction_of_sand_in_soil = "sandtotal_r", # %
      fraction_of_silt_in_soil = "silttotal_r", # %
      fraction_of_clay_in_soil = "claytotal_r", # %
      soil_depth = "hzdept_r", # cm
      soil_depth_bottom = "hzdepb_r", # cm
      organic_matter_pct = "om_r", # %
      bulk_density = "dbthirdbar_r", # g/cm3
      coarse_fragment_pct = "fragvol_r", # %
      mukey = "mukey") %>%
    dplyr::mutate(
      dplyr::across(c(dplyr::starts_with("fraction_of"), "coarse_fragment_pct"), 
                    ~ . / 100),
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
  
  #converting it to list
  soil.data.gssurgo <- list(
    fraction_of_sand_in_soil = soilprop.new$fraction_of_sand_in_soil,
    fraction_of_silt_in_soil = soilprop.new$fraction_of_silt_in_soil,
    fraction_of_clay_in_soil = soilprop.new$fraction_of_clay_in_soil,
    soil_depth = soilprop.new$soil_depth,
    soil_organic_carbon_stock = soilprop.new$soil_organic_carbon_stock
  )
  #This ensures that I have at least one soil ensemble in case the modeling part failed
  all.soil.ens <-c(all.soil.ens, list(soil.data.gssurgo))
  
  
  # What I do here is that I put soil data into depth classes and then model each class speparatly
  #- see if we need to generate soil ensemble and add that to the list of all
  tryCatch({
    # find the soil depth levels based on the depth argument 
    # if soil profile is deeper than what is specified in the argument then I go as deep as the soil profile.
    if (max(soilprop.new$soil_depth) > max(depths)) {
      depths <- sort(c(depths, max(soilprop.new$soil_depth)))
    }
    depth.levs<-findInterval(soilprop.new$soil_depth, depths)
    depth.levs[depth.levs==0] <-1
    depth.levs[depth.levs>length(depths)] <-length(depths)
    
    # Remove any NA depth levels
    valid_indices <- !is.na(depth.levs)
    if(sum(!valid_indices) > 0) {
      soilprop.new <- soilprop.new[valid_indices, ]
      depth.levs <- depth.levs[valid_indices]
    }
    
    soilprop.new.grouped<-soilprop.new %>% 
      dplyr::mutate(DepthL=depths[depth.levs])
    
    # let's fit dirichlet for each depth level separately
    simulated.soil.props<-soilprop.new.grouped %>%
      split(list(soilprop.new.grouped$DepthL, soilprop.new.grouped$mukey)) %>%
      purrr::map_df(function(DepthL.Data){
        tryCatch({
          # I model the soil properties for this depth
          dir.model <-DepthL.Data[,c(1:3)] %>%
            as.matrix() %>%
            sirt::dirichlet.mle(.)
          # Monte Carlo sampling based on my dirichlet model
          alpha <- dir.model$alpha
          alpha <- matrix(alpha, nrow= size, ncol=length(alpha), byrow=TRUE )
          simulated.soil <- sirt::dirichlet.simul(alpha)
          # Validate SOC data before processing
          if (any(is.na(DepthL.Data$soil_organic_carbon_stock))) {
            PEcAn.logger::logger.warn("Found NA values in soil_organic_carbon_stock data. Removing incomplete records.")
            DepthL.Data <- DepthL.Data[!is.na(DepthL.Data$soil_organic_carbon_stock), ]
          }
          if (nrow(DepthL.Data) == 0) {
            PEcAn.logger::logger.warn("No valid SOC data after removing NAs")
            return(NULL)
          }
          # Simulate SOC uncertainty using Gamma distribution
          soc_mean <- mean(DepthL.Data$soil_organic_carbon_stock, na.rm = TRUE)
          soc_sd <- stats::sd(DepthL.Data$soil_organic_carbon_stock, na.rm = TRUE)
          
          # Handle edge cases for SOC simulation
          if (nrow(DepthL.Data) == 1) {
            simulated_soc <- rep(NA_real_, size)
          } else if (is.na(soc_sd) || soc_sd == 0) {
            simulated_soc <- rep(NA_real_, size)
          } else {
            shape <- (soc_mean^2) / (soc_sd^2)
            rate <- soc_mean / (soc_sd^2)
            simulated_soc <- stats::rgamma(size, shape=shape, rate=rate)
          }
          
          simulated.soil<-simulated.soil %>%
            as.data.frame %>%
            dplyr::mutate(DepthL=rep(DepthL.Data$DepthL[1], size),
                   mukey=rep(DepthL.Data$mukey[1], size),
                   soil_organic_carbon_stock = simulated_soc) %>%
            `colnames<-`(c("fraction_of_sand_in_soil",
                           "fraction_of_silt_in_soil",
                           "fraction_of_clay_in_soil",
                           "soil_depth",
                           "mukey",
                           "soil_organic_carbon_stock"))
          simulated.soil
        },
        error = function(e) {
          PEcAn.logger::logger.warn(conditionMessage(e))
          return(NULL)
        })
      }) 
    
    # estimating the proportion of areas for those mukeys which are modeled

    # defining mukey_area
    mukey_area <- data.frame(
      mukey = names(mukey_counts),
      Area = as.numeric(mukey_counts) / sum(mukey_counts)
    ) %>%
      dplyr::filter(.data$mukey %in% unique(simulated.soil.props$mukey)) %>%
      dplyr::mutate(Area = .data$Area / sum(.data$Area, na.rm = TRUE))
    #--- Mixing the depths
    soil.profiles<-simulated.soil.props %>% 
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
    #- add them to the list of all the ensembles ready to be converted to .nc file
    all.soil.ens<-soil.profiles %>%
      purrr::map(function(SEns){
        SEns <- SEns[, names(SEns) != "mukey"]
        names(SEns) %>%
          purrr::map(function(var){
            as.numeric(unlist(SEns[, var]))
          }) %>%
          stats::setNames(names(SEns))
      }) %>%
      c(all.soil.ens,.)
    
  },
  error = function(e) {
    PEcAn.logger::logger.warn(conditionMessage(e))
  })
  
  
  #-- generating the .nc files for all the collected ensembles
  out.ense <- (1:length(all.soil.ens)) %>%
    purrr::map(function(i) {
      
      tryCatch({
        #browser()
        # calc new filename
        prefix <- paste0("gSSURGO_soil_", i)
        new.file <- file.path(outdir, paste0(prefix, ".nc"))
        #sending it to the func where some new params will be added and then it will be written down as nc file.
        suppressWarnings({
          PEcAn.data.land::soil2netcdf(all.soil.ens[[i]], new.file)
        })
        new.file
      },
      error = function(e) {
        PEcAn.logger::logger.warn(conditionMessage(e))
        return(NULL)
      })
    })
  # removing the nulls or the ones that throw exception in the above trycatch
  out.ense<- out.ense %>%
    purrr::discard(is.null)
  
  out.ense<-out.ense %>% 
    stats::setNames(rep("path", length(out.ense)))
  
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