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

extract_NEON_veg_withHerb <- function(lon, lat, start_date, end_date, store_dir, ...){

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
#tree ABG
neonstore::neon_download("DP1.10098.001", dir = store_dir, table = NA, site = sitename, start_date = start_date, end_date = end_date, type = "basic",api = "https://data.neonscience.org/api/v0")
apparentindividual <- neonstore::neon_read(table = "apparentindividual", product = "DP1.10098.001", site = sitename, start_date = start_date, end_date = end_date, dir = store_dir)
mappingandtagging <- neonstore::neon_read(table = "mappingandtagging", product = "DP1.10098.001", site = sitename, start_date = start_date, end_date = end_date, dir = store_dir)
joined.tree <- dplyr::left_join(mappingandtagging, apparentindividual, by = "individualID")

#herb AGB
neonstore::neon_download("DP1.10023.001", dir = store_dir, table = NA, site = sitename, start_date = start_date, end_date = end_date, type = "basic",api = "https://data.neonscience.org/api/v0")
massdata <- neonstore::neon_read(table = "massdata", product = "DP1.10023.001", site = sitename, start_date = start_date, end_date = end_date, dir = store_dir)
perbout <- neonstore::neon_read(table = "perbout", product = "DP1.10023.001", site = sitename, start_date = start_date, end_date = end_date, dir = store_dir)
joined.herb <- dplyr::left_join(massdata, perbout, by = "sampleID")

#species info
neonstore::neon_download("DP1.10058.001", dir = store_dir, table = NA, site = sitename, start_date = start_date, end_date = end_date, type = "basic",api = "https://data.neonscience.org/api/v0")
div_1m2 <- neonstore::neon_read(table = "div_1m2", product = "DP1.10058.001", site = sitename, start_date = start_date, end_date = end_date, dir = store_dir)

# #soil carbon
neonstore::neon_download("DP1.00096.001", dir = store_dir, table = NA, site = sitename, start_date = as.Date("2012-01-01"), end_date = as.Date("2014-12-31"), type = "basic",api = "https://data.neonscience.org/api/v0")
perbiogeosample <- neonstore::neon_read(table = "perbiogeosample", product = "DP1.00096.001", site = sitename, start_date = as.Date("2012-01-01"), end_date = as.Date("2014-12-31"), dir = "/projectnb/dietzelab/ahelgeso/test_download/")
perarchivesample <- neonstore::neon_read(table = "perarchivesample", product = "DP1.00096.001", site = sitename, start_date = as.Date("2012-01-01"), end_date = as.Date("2014-12-31"), dir = "/projectnb/dietzelab/ahelgeso/test_download/")
perbulksample <- neonstore::neon_read(table = "perbulksample", product = "DP1.00096.001", site = sitename, start_date = as.Date("2012-01-01"), end_date = as.Date("2014-12-31"), dir = "/projectnb/dietzelab/ahelgeso/test_download/")
joined.soil <- dplyr::left_join(perarchivesample, perbiogeosample, by = "horizonID")
joined.soil <- dplyr::left_join(joined.soil, perbulksample, by = "horizonID")
soilcarbon.per.m2 <- sum(joined.soil$bulkDensExclCoarseFrag * joined.soil$carbonTot * 0.001 *  (joined.soil$biogeoBottomDepth - joined.soil$biogeoTopDepth) * 10000)

#Filter joined.tree, joined.herb, and div_1m2 for required information: DBH, tree height, dryMass, taxonID (USDA code) and species info
filter.tree <- dplyr::select(joined.tree, siteID.y, plotID.x, subplotID, nestedSubplotID, taxonID, scientificName, taxonRank, date.y, stemDiameter, height)
filter.herb <- dplyr::select(joined.herb, siteID.y, plotID.x, subplotID, plotType.x, clipArea, dryMass, collectDate.y)
filter.species <- dplyr::select(div_1m2, plotID, subplotID, taxonID, scientificName, taxonRank)
#check if species info is available for herb plots
herb.plot <- unique(filter.herb$plotID.x)
check.species <- herb.plot %in% filter.species$plotID
colnames(filter.herb)[2] <- "plotID"
if (TRUE %in% check.species) {
  #add species info to filter.herb if it exists
  filter.herb <- dplyr::left_join(filter.herb, filter.species, by = "plotID")
}else{
  PEcAn.logger::logger.info(paste0("No herbacious species info available for ", sitename))
}

#remove NAs from species column only, next step species matching does not like NAs
filter.herb <- filter.herb[!is.na(filter.herb$scientificName),]
filter.tree <- filter.tree[!is.na(filter.tree$scientificName),]
#Create year column
filter.tree$year <- format(as.Date(filter.tree$date.y, format="%Y-%m-%d"),"%Y")
filter.herb$year <- format(as.Date(filter.herb$collectDate.y, format="%Y-%m-%d"),"%Y")
#Rename NEON column names to match pecan functions
colnames(filter.tree) <- c("site_name", "plot", "Subplot", "nestedSubplot","species_USDA_symbol", "species", "taxonRank", "date", "DBH", "height", "year")
colnames(filter.herb) <- c("site_name", "plot", "Subplot", "plotType", "clipArea", "dryMass", "date", "Subplot.species","species_USDA_symbol", "species", "taxonRank", "year")
#Create veg_info object as a list
veg_info <- list()
#Set plot size as veg_info[[1]]
veg_info[[1]] <- list(Plot = 1600, subPlot = 400, nestedSubplot = 100, herb_clipArea = filter.herb$clipArea[1])
#Set filter.tree as veg_info[[2]]
veg_info[[2]] <- filter.tree
#Set filter.herb as veg_info[[3]]
veg_info[[3]] <- filter.herb

# #set soilcarbon.per.m2 as veg_info[[4]]
veg_info[[4]] <- soilcarbon.per.m2

return(veg_info)
}
