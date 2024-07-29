#' @title Aggregation Function
#' @name aggregate
#' @author Harunobu Ishii
#'
#' @param downscale_output  Raster file output from downscale_function.R. Read file in this way if stored locally: \code{downscale_output <- readRDS("xxx.rds")}
#' @param polygon_data A spatial polygon object (e.g., an `sf` object) that defines the spatial units for aggregation. 
#'                     This data should be in a coordinate reference system compatible with the raster data (e.g., "EPSG:4326").
#' @param func A character string specifying the aggregation function to use (e.g., 'mean', 'sum').
#' @details This function will aggregate previously downscaled carbon flux amount to a spatial unit of choice 
#'
#' @return It returns the `polygon_data` with added columns for mean and sum values of the aggregated raster data for each ensemble member.
#' @export
#' @examples
#'        \dontrun{
#'        # Download a shapefile of U.S. (polygon data)
#'        url <- "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_state_20m.zip"
#'        download.file(url, destfile = "polygon/us_states.zip")
#'        
#'        # Unzip the downloaded file and save locally
#'        unzip("polygon/us_states.zip", exdir = "polygon/us_states")
#'        us_states <- st_read("polygon/us_states/cb_2020_us_state_20m.shp")
#'        saveRDS(us_states, "polygon/us_states.rds")
#'        
#'        # Load the saved polygon data with Massachusetts as an example
#'        us_states <- readRDS("polygon/us_states.rds")
#'        state <- "MA"
#'        polygon_data <- st_transform(us_states[us_states$STUSPS == state, ], crs = "EPSG:4326")
#'      
#'        # Load the downscaled raster output
#'        downscale_output <- readRDS("path/to/downscale_output.rds")
#'        
#'        # Slot in as argument to the aggregate function
#'        result <- aggregate(downscale_output, polygon_data)
#'        print(result)
#'        }

aggregate <- function(downscale_output, polygon_data, func = 'mean'){
  # check availability of optional packages
  missing_pkgs <- c()
  if (!requireNamespace("sf", quietly = TRUE)) {
    missing_pkgs <- c(missing_pkgs, "sf")
  }
  if (!requireNamespace("exactextractr", quietly = TRUE)) {
    missing_pkgs <- c(missing_pkgs, "exactextractr")
  }
  if (!requireNamespace("raster", quietly = TRUE)) {
    missing_pkgs <- c(missing_pkgs, "raster")
  }
  if (length(missing_pkgs) > 0) {
    PEcAn.logger::logger.severe(
      "Package(s)", missing_pkgs,
      "needed by PEcAnAssimSequential::aggregate() but not installed")
  }

  grand_TTL <- 0
  if (sf::st_crs(downscale_output$maps$ensemble1) != sf::st_crs(polygon_data)) {
    stop("CRS of downscale_output and polygon_data must match.")
  }

  # Perform spatial operations on each raster
  for (name in names(downscale_output$maps)) {
    raster_data <- downscale_output$maps[[name]]
    agg_values <- exactextractr::exact_extract(raster_data, polygon_data, fun = func)
    
    polygon_data[[paste0(name, "_", func)]] <- agg_values
    grand_TTL = grand_TTL + agg_values
  }
  if(func == 'mean'){
    grand_TTL = grand_TTL/length(downscale_output$maps)
  }
  polygon_data[[paste0("TTL_", func)]] <- grand_TTL
  return (polygon_data)
}