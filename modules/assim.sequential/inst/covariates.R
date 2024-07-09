##' @title Covariate Data Prep
##' @author Joshua Ploshay
##'
##' @description This script lists the code needed to download, resample, and 
##' stack the covariate data used in the downscale_function.R sript.
##' 
##' @return A spatraster object consisting of a stack of maps with labeled layers

#### WorldClim ####
# WorldClim v2.1 (1970-2000) historical climate data
# Data can be found at https://www.worldclim.org/data/worldclim21.html
# Product is a zip file containing 12 .tif files, one for each month
# Use code below to average the 12 files to obtain one map
# 2023 REU project used 10 minute spatial resolution

## Solar Radiation (kJ m-2 day-1)
srad <- terra::rast(list.files(path = "/projectnb/dietzelab/jploshay/pecan_copy/jploshay/10m_srad",
                               pattern='.tif$',
                               all.files= T,
                               full.names= T))
srad <- terra::app(srad, mean)


## Vapor Pressure (kPa)
vapr <- terra::rast(list.files(path = "/projectnb/dietzelab/jploshay/pecan_copy/jploshay/10m_vapr",
                               pattern='.tif$',
                               all.files= T,
                               full.names= T)) 
vapr <- terra::app(vapr, mean)


## Average Temperature (*C)
tavg <- terra::rast(list.files(path = "/projectnb/dietzelab/jploshay/pecan_copy/jploshay/avg_temp_prep/WorldClim",
                               pattern='.tif$',
                               all.files= T,
                               full.names= T))
tavg <- terra::app(tavg, mean)


## Total Precipitation (mm)
prec <- terra::rast(list.files(path = "/projectnb/dietzelab/jploshay/pecan_copy/jploshay/total_prec",
                               pattern='.tif$',
                               all.files= T,
                               full.names= T))
prec <- terra::app(prec, mean)


#### SoilGrids ####
# geodata::soil_world() pulls data from the SoilGRIDS database
# More information on pulling different soil data can be found in the geodata 
# manual https://cran.r-project.org/web/packages/geodata/geodata.pdf

## Soil pH
phh2o <- geodata::soil_world(var = "phh2o", depth = 5, stat = "mean", path = tempdir()) 

## Soil Nitrogen (g kg-1)
nitrogen <- geodata::soil_world(var = "nitrogen", depth = 5, stat = "mean", path = tempdir())

## Soil Organic Carbon (g kg-1)
soc <- geodata::soil_world(var = "soc", depth = 5, stat = "mean", path = tempdir())

## Soil Sand (%)
sand <- geodata::soil_world(var = "sand", depth = 5, stat = "mean", path = tempdir())


#### Land Cover ####
GLanCE_extract <- function(pattern, path) {
  files <- list.files(path = "/projectnb/dietzelab/dietze/glance2012/e4ftl01.cr.usgs.gov/MEASURES/GLanCE30.001/2012.07.01", #make this path default
                      all.files = T,
                      full.names = T,
                      pattern)
  # empty .vrt file path
  vrtfile <- paste0(tempfile(), ".vrt") 
  # "connects" tiles together that returns SpatRaster
  GLanCE_product <- terra::vrt(files, vrtfile, overwrite = T) 
  return(GLanCE_product)
}

# Integer identifier for class in the current year
land_cover <- GLanCE_extract(pattern = "NA_LC.tif$")


#### Resample and Stack Maps ####
# Define the extent to crop the covariates to North America
NA_extent <- terra::ext(-178.19453125, -10, 7.22006835937502, 83.5996093750001)

# Stack WorldClim maps 
WorldClim <- c(tavg, srad, prec, vapr)

# Crop WolrdClim stack to North America using North America extent
NA_WorldClim <- terra::crop(WorldClim, NA_extent)
names(NA_WorldClim) <- c("tavg", "srad", "prec", "vapr")

# Stack SoilGrids maps 
SoilGrids <- c(phh2o, nitrogen, soc, sand)

# Crop SoilGrids stack to North America using North America extent
NA_SoilGrids <- terra::crop(SoilGrids, NA_extent)
names(NA_SoilGrids) <- c("phh2o", "nitrogen", "soc", "sand")

# Resample SoilGrids to match WorldClim maps
NA_SoilGrids <- terra::resample(NA_SoilGrids, NA_WorldClim, method = 'bilinear')

# Resample land cover to match WorldClim maps (~25 min)
land_cover <- terra::resample(land_cover, NA_WorldClim, method = 'near')
names(land_cover) <- "land_cover"

# Stack all maps 
covariates <- c(NA_WorldClim, NA_SoilGrids, land_cover) 