##' @title Aggregation Function
##' @name aggregate
##' @author Harunobu Ishii
##'
##' @param downscale_output  Raster file output from downscale_function.R. Read file in this way if stored locally: \code{downscale_output <- readRDS("xxx.rds")}
##' @param polygon_data A spatial polygon object (e.g., an `sf` object) that defines the spatial units for aggregation. 
##'                     This data should be in a coordinate reference system compatible with the raster data (e.g., "EPSG:4326").
##'                     Example of state-level aggregation: 
##'                     \code{
##'                     us_states <- readRDS("polygon/us_states.rds")
##'                     state <- "MA"
##'                     polygon_data <- st_transform(us_states[us_states$STUSPS == state, ], crs = "EPSG:4326")
##'                     }
##' @details This function will aggregate previously downscaled carbon flux amount to a spatial unit of choice 
##'
##' @return It returns the `polygon_data` with added columns for mean and sum values of the aggregated raster data for each ensemble member.


library(exactextractr)
library(terra)
library(sf)

aggregate <- function(downscale_output, polygon_data){

    # Perform spatial operations on each raster
    for (name in names(downscale_output$maps)) {
      raster_data <- downscale_output$maps[[name]]

      mean_values <- exact_extract(raster_data, polygon_data, fun = 'mean')
      sum_values <- exact_extract(raster_data, polygon_data, fun = 'sum')
      
      polygon_data[[paste0(name, "_mean")]] <- mean_values
      polygon_data[[paste0(name, "_sum")]] <- sum_values
    }
return (polygon_data)
}