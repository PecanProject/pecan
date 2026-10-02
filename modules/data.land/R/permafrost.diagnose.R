#' Build soil physical and permafrost site table
#'
#' Extracts model-site index, latitude, and longitude directly from PEcAn
#' settings, reads site-level soil physical properties from PEcAn soil NetCDF
#' files, extracts multi-year ESA CCI permafrost fraction, and generates the
#' site table required by the SIPNET SoilT workflow.
#'
#' Permafrost is defined as multi-year mean PFR greater than or equal to
#' `threshold_pct`. Missing PFR is treated as non-permafrost.
#'
#' @param settings PEcAn Settings or MultiSettings object. Site `index`,
#'   latitude, and longitude are read from `run$site$id`, `run$site$lat`,
#'   and `run$site$lon`.
#' @param point_depth_cm Requested soil depth in centimeters.
#' @param soil_root Root directory containing site-specific soil NetCDF files.
#' @param start_year First year used to calculate multi-year PFR.
#' @param end_year Last requested PFR year.
#' @param threshold_pct Mean PFR threshold used to classify permafrost.
#' @param cache_dir Directory used to cache ESA CCI PFR NetCDF files.
#'
#' @return A data.table with one row per SIPNET model site.
#'
#' @export
#' @author Yang Gu
build_sipnet_pfr_sites <- function(
    settings,
    point_depth_cm = 6,
    soil_root = paste0(
      "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/soil_nc/",
      "soil_texture_output/soil_texture_ensemble"
    ),
    start_year = 2012L,
    end_year = 2024L,
    threshold_pct = 10,
    cache_dir = "/projectnb/dietzelab/guYANG/NA_runs/ESA_CCI"
) {
  
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package `data.table` is required.", call. = FALSE)
  }
  
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    stop("Package `ncdf4` is required.", call. = FALSE)
  }
  
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package `terra` is required.", call. = FALSE)
  }
  
  # ==========================================================================
  # 1. Read site index / latitude / longitude from settings
  # ==========================================================================
  
  settings_list <- if (!is.null(settings$run) && !is.null(settings$run$site)) {
    list(settings)
  } else {
    as.list(settings)
  }
  
  site_rows <- lapply(settings_list, function(settings_i) {
    
    site <- settings_i$run$site
    
    if (is.null(site)) {
      return(NULL)
    }
    
    data.table::data.table(
      index = suppressWarnings(as.integer(site$id)),
      lat = suppressWarnings(as.numeric(site$lat)),
      lon = suppressWarnings(as.numeric(site$lon))
    )
  })
  
  sites <- data.table::rbindlist(
    site_rows,
    use.names = TRUE,
    fill = TRUE
  )
  
  sites <- unique(
    sites[
      !is.na(index) &
        is.finite(lat) &
        is.finite(lon)
    ],
    by = "index"
  )
  
  if (nrow(sites) == 0L) {
    stop(
      "No valid `run$site$id`, `lat`, and `lon` found in `settings`.",
      call. = FALSE
    )
  }
  
  if (anyDuplicated(sites$index) > 0L) {
    stop(
      "Duplicated site indices found in `settings`.",
      call. = FALSE
    )
  }
  
  # ==========================================================================
  # 2. Soil physical properties
  # ==========================================================================
  
  point_depth_cm <- suppressWarnings(
    as.numeric(point_depth_cm)[1L]
  )
  
  if (!is.finite(point_depth_cm) || point_depth_cm <= 0) {
    stop(
      "`point_depth_cm` must be positive.",
      call. = FALSE
    )
  }
  
  read_fraction <- function(nc, variable_name, selected_layer) {
    
    if (!variable_name %in% names(nc$var)) {
      return(NA_real_)
    }
    
    value <- as.numeric(
      ncdf4::ncvar_get(nc, variable_name)[selected_layer]
    )
    
    if (is.finite(value) && abs(value) <= 1.5) {
      value <- value * 100
    }
    
    value
  }
  
  read_bulk_density <- function(nc, selected_layer) {
    
    if (!"soil_bulk_density" %in% names(nc$var)) {
      return(NA_real_)
    }
    
    value <- as.numeric(
      ncdf4::ncvar_get(nc, "soil_bulk_density")[selected_layer]
    )
    
    if (!is.finite(value)) {
      return(NA_real_)
    }
    
    if (value < 10) {
      value <- value * 1000
    }
    
    value
  }
  
  read_one_soil_site <- function(site_index) {
    
    soil_file <- file.path(
      soil_root,
      as.character(site_index),
      sprintf("Soil_params_0-%d_1.nc", site_index)
    )
    
    if (!file.exists(soil_file)) {
      
      site_dir <- file.path(
        soil_root,
        as.character(site_index)
      )
      
      candidates <- if (dir.exists(site_dir)) {
        list.files(
          site_dir,
          pattern = sprintf("-%d_1\\.nc$", site_index),
          full.names = TRUE
        )
      } else {
        character()
      }
      
      if (length(candidates) == 0L) {
        return(
          data.table::data.table(
            index = site_index,
            soil_file = NA_character_,
            requested_depth_cm = point_depth_cm,
            selected_layer = NA_integer_,
            layer_bottom_depth_m = NA_real_,
            sand_pct = NA_real_,
            silt_pct = NA_real_,
            clay_pct = NA_real_,
            bulk_density_kg_m3 = NA_real_,
            soil_success = FALSE
          )
        )
      }
      
      soil_file <- candidates[1L]
    }
    
    nc <- ncdf4::nc_open(
      soil_file
    )
    
    on.exit(
      ncdf4::nc_close(nc),
      add = TRUE
    )
    
    depth_dim <- getElement(
      nc$dim,
      "depth"
    )
    
    if (is.null(depth_dim)) {
      stop(
        "No `depth` dimension in ",
        soil_file,
        ".",
        call. = FALSE
      )
    }
    
    depth_values <- as.numeric(
      depth_dim$vals
    )
    
    point_depth_m <- point_depth_cm / 100
    
    selected_layer <- which(
      depth_values >= point_depth_m
    )[1L]
    
    if (is.na(selected_layer)) {
      selected_layer <- which.min(
        abs(depth_values - point_depth_m)
      )
    }
    
    sand_pct <- read_fraction(
      nc,
      "fraction_of_sand_in_soil",
      selected_layer
    )
    
    data.table::data.table(
      index = site_index,
      soil_file = soil_file,
      requested_depth_cm = point_depth_cm,
      selected_layer = selected_layer,
      layer_bottom_depth_m = depth_values[selected_layer],
      sand_pct = sand_pct,
      silt_pct = read_fraction(
        nc,
        "fraction_of_silt_in_soil",
        selected_layer
      ),
      clay_pct = read_fraction(
        nc,
        "fraction_of_clay_in_soil",
        selected_layer
      ),
      bulk_density_kg_m3 = read_bulk_density(
        nc,
        selected_layer
      ),
      soil_success = is.finite(sand_pct)
    )
  }
  
  soil_physic <- data.table::rbindlist(
    lapply(
      sites$index,
      read_one_soil_site
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  soil_physic[
    ,
    texture_sum_pct :=
      sand_pct +
      silt_pct +
      clay_pct
  ]
  
  sites <- merge(
    sites,
    soil_physic,
    by = "index",
    all.x = TRUE,
    sort = FALSE
  )
  
  # ==========================================================================
  # 3. ESA CCI PFR years
  # ==========================================================================
  
  start_year <- as.integer(start_year)[1L]
  end_year <- as.integer(end_year)[1L]
  threshold_pct <- as.numeric(threshold_pct)[1L]
  
  if (
    !is.finite(start_year) ||
    !is.finite(end_year) ||
    end_year < start_year
  ) {
    stop(
      "Invalid `start_year` / `end_year`.",
      call. = FALSE
    )
  }
  
  if (
    !is.finite(threshold_pct) ||
    threshold_pct < 0 ||
    threshold_pct > 100
  ) {
    stop(
      "`threshold_pct` must be between 0 and 100.",
      call. = FALSE
    )
  }
  
  available_start_year <- 1997L
  available_end_year <- 2023L
  
  use_start_year <- max(
    start_year,
    available_start_year
  )
  
  use_end_year <- min(
    end_year,
    available_end_year
  )
  
  if (
    start_year < available_start_year ||
    end_year > available_end_year
  ) {
    warning(
      "Requested PFR years ",
      start_year,
      "-",
      end_year,
      "; ESA CCI PFR v5.0 covers ",
      available_start_year,
      "-",
      available_end_year,
      ". Using ",
      use_start_year,
      "-",
      use_end_year,
      ".",
      call. = FALSE
    )
  }
  
  if (use_end_year < use_start_year) {
    stop(
      "Requested period does not overlap ESA CCI PFR coverage.",
      call. = FALSE
    )
  }
  
  years <- seq.int(
    use_start_year,
    use_end_year
  )
  
  dir.create(
    cache_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  base_url <- paste0(
    "https://data.cci.ceda.ac.uk/thredds/fileServer/",
    "esacci/permafrost/data/permafrost_extent/",
    "L4/area4/pp/v05.0/northern_hemisphere"
  )
  
  # ==========================================================================
  # 4. Extract PFR for every site and year
  # ==========================================================================
  
  points_ll <- terra::vect(
    data.frame(
      lon = sites$lon,
      lat = sites$lat
    ),
    geom = c("lon", "lat"),
    crs = "EPSG:4326"
  )
  
  annual_rows <- vector(
    "list",
    length(years)
  )
  
  for (year_i in seq_along(years)) {
    
    year <- years[year_i]
    
    message(
      "PFR year ",
      year,
      " [",
      year_i,
      "/",
      length(years),
      "]"
    )
    
    if (year >= 2003L) {
      
      filename <- sprintf(
        paste0(
          "ESACCI-PERMAFROST-L4-PFR-",
          "MODISLST_CRYOGRID-AREA4_PP-%d-fv05.0.nc"
        ),
        year
      )
      
    } else {
      
      filename <- sprintf(
        paste0(
          "ESACCI-PERMAFROST-L4-PFR-",
          "ERA5_MODISLST_BIASCORRECTED-AREA4_PP-%d-fv05.0.nc"
        ),
        year
      )
    }
    
    nc_file <- file.path(
      cache_dir,
      filename
    )
    
    if (!file.exists(nc_file)) {
      
      download_url <- paste0(
        base_url,
        "/",
        year,
        "/",
        filename
      )
      
      message(
        "Downloading: ",
        filename
      )
      
      utils::download.file(
        url = download_url,
        destfile = nc_file,
        mode = "wb",
        quiet = TRUE
      )
    }
    
    if (!file.exists(nc_file)) {
      stop(
        "Failed to obtain PFR file for ",
        year,
        ".",
        call. = FALSE
      )
    }
    
    pfr <- terra::rast(
      nc_file
    )
    
    layer_names <- names(
      pfr
    )
    
    pfr_layer_id <- grep(
      "PFR|permafrost.*fraction",
      layer_names,
      ignore.case = TRUE
    )
    
    if (length(pfr_layer_id) >= 1L) {
      
      pfr <- pfr[[pfr_layer_id[1L]]]
      
    } else if (terra::nlyr(pfr) == 1L) {
      
      pfr <- pfr[[1L]]
      
    } else {
      
      stop(
        "Could not identify PFR layer for ",
        year,
        ". Available layers: ",
        paste(layer_names, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    
    points <- points_ll
    
    if (!terra::same.crs(points, pfr)) {
      points <- terra::project(
        points,
        terra::crs(pfr)
      )
    }
    
    extracted <- terra::extract(
      pfr,
      points,
      method = "simple"
    )
    
    value_columns <- setdiff(
      names(extracted),
      "ID"
    )
    
    if (length(value_columns) != 1L) {
      stop(
        "Unexpected PFR extraction result for ",
        year,
        ".",
        call. = FALSE
      )
    }
    
    annual_rows[[year_i]] <- data.table::data.table(
      index = sites$index,
      year = year,
      PFR_pct = as.numeric(
        extracted[[value_columns[1L]]]
      )
    )
  }
  
  # ==========================================================================
  # 5. Multi-year PFR summary
  # ==========================================================================
  
  annual <- data.table::rbindlist(
    annual_rows,
    use.names = TRUE,
    fill = TRUE
  )
  
  pfr_summary <- annual[
    ,
    {
      valid_pfr <- PFR_pct[
        is.finite(PFR_pct)
      ]
      
      if (length(valid_pfr) == 0L) {
        
        .(
          n_years = 0L,
          mean_PFR_pct = NA_real_,
          median_PFR_pct = NA_real_,
          min_PFR_pct = NA_real_,
          max_PFR_pct = NA_real_,
          sd_PFR_pct = NA_real_
        )
        
      } else {
        
        .(
          n_years = length(valid_pfr),
          mean_PFR_pct = mean(valid_pfr),
          median_PFR_pct = stats::median(valid_pfr),
          min_PFR_pct = min(valid_pfr),
          max_PFR_pct = max(valid_pfr),
          sd_PFR_pct = if (length(valid_pfr) > 1L) {
            stats::sd(valid_pfr)
          } else {
            NA_real_
          }
        )
      }
    },
    by = index
  ]
  
  # ==========================================================================
  # 6. Final SIPNET pfr_sites table
  # ==========================================================================
  
  pfr_sites <- merge(
    sites,
    pfr_summary,
    by = "index",
    all.x = TRUE,
    sort = FALSE
  )
  
  pfr_sites[
    ,
    mean_PFR_fraction :=
      mean_PFR_pct / 100
  ]
  
  pfr_sites[
    ,
    permafrost_zone := data.table::fcase(
      is.na(mean_PFR_pct), NA_character_,
      mean_PFR_pct < 10, "isolated",
      mean_PFR_pct < 50, "sporadic",
      mean_PFR_pct < 90, "discontinuous",
      default = "continuous"
    )
  ]
  
  # Missing PFR is treated as non-permafrost.
  pfr_sites[
    ,
    is_permafrost :=
      !is.na(mean_PFR_pct) &
      mean_PFR_pct >= threshold_pct
  ]
  
  pfr_sites[
    ,
    soilT_model := data.table::fifelse(
      is_permafrost,
      "permafrost",
      "non_permafrost"
    )
  ]
  
  pfr_sites[
    ,
    `:=`(
      PFR_start_year = use_start_year,
      PFR_end_year = use_end_year,
      threshold_pct = threshold_pct
    )
  ]
  
  data.table::setorder(
    pfr_sites,
    index
  )
  
  message(
    "========================================"
  )
  
  message(
    "SIPNET SOIL + PFR SITE TABLE"
  )
  
  message(
    "========================================"
  )
  
  message(
    "Sites = ",
    nrow(pfr_sites),
    " | soil available = ",
    sum(pfr_sites$soil_success, na.rm = TRUE),
    " | permafrost = ",
    sum(pfr_sites$is_permafrost),
    " | non-permafrost = ",
    sum(!pfr_sites$is_permafrost)
  )
  
  missing_sand <- pfr_sites[
    !is.finite(sand_pct),
    .(
      index,
      lat,
      lon,
      soil_file
    )
  ]
  
  if (nrow(missing_sand) > 0L) {
    warning(
      nrow(missing_sand),
      " sites are missing `sand_pct`; SoilT tau cannot be predicted for them.",
      call. = FALSE
    )
  }
  
  return(
    pfr_sites
  )
}