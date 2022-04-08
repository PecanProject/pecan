#' extract_NEON_veg
#' @title extract_NEON_veg
#' @name extract_NEON_veg
#' 
#' @param lon site longitude, passed from ic_process
#' @param lat site latitude, passed from ic_process
#' @param start_date "YYYY-MM-DD", used to download NEON datasets for desired time period
#' @param end_date "YYYY_MM_DD", used to download NEON datasets for desired time period
#' @param store_dir location where you want to store downloaded NEON files
#' @param ... Additional parameters
#' 
#'
#' @return veg_info object to be passed to extract_veg within ic_process
#' @author Alexis Helgeson and Michael Dietze
#' @export
#' 
#' @importFrom rlang .data
#' 
#' @examples start_date = as.Date("2020-01-01") 
#' end_date = as.Date("2021-09-01")

extract_NEON_veg <- function(lon, lat, start_date, end_date, store_dir, ...){

#Find sitename from lon and lat params using distance
neonsites <- neonstore::neon_sites(api = "https://data.neonscience.org/api/v0", .token = Sys.getenv("NEON_TOKEN"))
neonsites <- dplyr::select(neonsites, .data$siteCode, .data$siteLatitude, .data$siteLongitude) #select for relevant columns
betyneondist <- swfscMisc::distance(lat1 = lat, lon1 = lon, lat2 = neonsites$siteLatitude, lon2 = neonsites$siteLongitude)
mindist <- min(betyneondist)
distloc <- match(mindist, betyneondist)
lat <- neonsites$siteLatitude[distloc]
lon <- neonsites$siteLongitude[distloc]
site <- dplyr::filter(neonsites, .data$siteLatitude == lat & .data$siteLongitude == lon)
sitename = site$siteCode
#Load in NEON datasets
neonstore::neon_download("DP1.10098.001", dir = store_dir, table = NA, site = sitename, start_date = start_date, end_date = end_date, type = "basic",api = "https://data.neonscience.org/api/v0")
apparentindividual <- neonstore::neon_read(table = "apparentindividual", product = "DP1.10098.001", site = sitename, start_date = start_date, end_date = end_date, dir = store_dir)
mappingandtagging <- neonstore::neon_read(table = "mappingandtagging", product = "DP1.10098.001", site = sitename, start_date = start_date, end_date = end_date, dir = store_dir)
joined.veg <- dplyr::left_join(mappingandtagging, apparentindividual, by = "individualID")
#Filter joined.veg for required information: DBH, tree height, and species
filter.veg <- dplyr::select(joined.veg, .data$siteID.x, .data$plotID.x, .data$subplotID.x, .data$taxonID, .data$scientificName, .data$taxonRank, .data$date.y, .data$stemDiameter, .data$height)
#Filter for most recent record
filter.date <- dplyr::filter(filter.veg, .data$date.y >= start_date)
#Create year column
filter.date$year <- format(as.Date(filter.date$date.y, format="%d/%m/%Y"),"%Y")
#Rename NEON column names to match pecan functions
colnames(filter.date) <- c("site_name", "plot", "Subplot", "species_USDA_symbol", "species", "taxonRank", "date", "DBH", "height", "year")
#Create veg_info object as a list
veg_info <- list()
#Set filter.date as veg_info[[2]]
veg_info[[2]] <- filter.date
#Set plot size as veg_info[[1]]
veg_info[[1]] <- list(area = 400)

return(veg_info)
}
