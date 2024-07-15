#' @title Aggregation Function
#' @name aggregate
#' @author Harunobu Ishii
#'
#' @param downscale_output  Raster file output from downscale_function.R. Read file in this way if stored locally: \code{downscale_output <- readRDS("xxx.rds")}
#' @param polygon_data A spatial polygon object (e.g., an `sf` object) that defines the spatial units for aggregation. 
#'                     This data should be in a coordinate reference system compatible with the raster data (e.g., "EPSG:4326").
#'                     Example of state-level aggregation: 
#'                     \code{
#'                     us_states <- readRDS("polygon/us_states.rds")
#'                     state <- "MA"
#'                     polygon_data <- st_transform(us_states[us_states$STUSPS == state, ], crs = "EPSG:4326")
#'                     }
#' @param func A character string specifying the aggregation function to use (e.g., 'mean', 'sum').
#' @details This function will aggregate previously downscaled carbon flux amount to a spatial unit of choice 
#'
#' @return It returns the `polygon_data` with added columns for mean and sum values of the aggregated raster data for each ensemble member.
#' @import sf
#' @import exactextractr
#' @import raster
#' @export

aggregate <- function(downscale_output, polygon_data, func = 'mean'){
  grand_TTL <- 0

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