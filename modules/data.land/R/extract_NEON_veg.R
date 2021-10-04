#' extract_NEON_veg
#' @title extract_NEON_veg
#' @name extract_NEON_veg
#' 
#' @param new_site object passed from get_veg_module that includes site lat, lon, id, and name
#' @param start_date "YYYY-MM-DD", used to download NEON datasets for desired time period
#' @param end_date "YYYY_MM_DD", used to download NEON datasets for desired time period
#' @param ... Additional parameters
#' 
#' @return veg_info object to be passed to extract_veg within ic_process
#' @author Alexis Helgeson and Michael Dietze
#' @export
#' 
#' @examples start_date = as.Date("2020-01-01") end_date = as.Date("2021-09-01")

extract_NEON_veg <- function(new_site, start_date, end_date, ...){

#turn new_site back into a df
new.site <- as.data.frame(new_site, stringsAsFactors = FALSE)
#extract lat and lon from new_site
lat <- new.site$lat
lon <- new.site$lon
sitename <- new.site$name
#Set store_dir to point to dietzelab/neon_store
store_dir = "/projectnb/dietzelab/neon_data"
#Load in NEON datasets
#tree ABG
neonstore::neon_download("DP1.10098.001", dir = store_dir, table = NA, site = sitename, start_date = as.Date("2019-01-01"), end_date = as.Date("2021-09-01"), type = "basic",api = "https://data.neonscience.org/api/v0")
apparentindividual <- neonstore::neon_read(table = "apparentindividual", product = "DP1.10098.001", site = sitename, start_date = as.Date("2019-01-01"), end_date = as.Date("2021-09-01"), dir = store_dir)
mappingandtagging <- neonstore::neon_read(table = "mappingandtagging", product = "DP1.10098.001", site = sitename, start_date = as.Date("2019-01-01"), end_date = as.Date("2021-09-01"), dir = store_dir)
joined.tree <- dplyr::left_join(mappingandtagging, apparentindividual, by = "individualID")
#herb AGB
neonstore::neon_download("DP1.10023.001", dir = store_dir, table = NA, site = sitename, start_date = as.Date("2019-01-01"), end_date = as.Date("2021-09-01"), type = "basic",api = "https://data.neonscience.org/api/v0")
massdata <- neonstore::neon_read(table = "massdata", product = "DP1.10023.001", site = sitename, start_date = as.Date("2019-01-01"), end_date = as.Date("2021-09-01"), dir = store_dir)
perbout <- neonstore::neon_read(table = "perbout", product = "DP1.10023.001", site = sitename, start_date = as.Date("2019-01-01"), end_date = as.Date("2021-09-01"), dir = store_dir)
joined.herb <- dplyr::left_join(massdata, perbout, by = "sampleID")
#species info
neonstore::neon_download("DP1.10058.001", dir = store_dir, table = NA, site = sitename, start_date = as.Date("2019-01-01"), end_date = as.Date("2021-09-01"), type = "basic",api = "https://data.neonscience.org/api/v0")
div_1m2 <- neonstore::neon_read(table = "div_1m2", product = "DP1.10058.001", site = sitename, start_date = as.Date("2019-01-01"), end_date = as.Date("2021-09-01"), dir = store_dir)
#Filter joined.tree, joined.herb, and div_1m2 for required information: DBH, tree height, dryMass, taxonID (USDA code) and species info
filter.tree <- dplyr::select(joined.tree, siteID.x, plotID.x, subplotID.x, taxonID, scientificName, taxonRank, date.y, stemDiameter, height)
filter.herb <- dplyr::select(joined.herb, siteID.y, plotID.x, subplotID, clipArea, dryMass, collectDate.y)
colnames(filter.herb)[2] <- "plotID"
filter.species <- dplyr::select(div_1m2, plotID, taxonID, scientificName, taxonRank)
#check if species info is available for herb plots
herb.plot <- unique(filter.herb$plotID)
check.species <- herb.plot %in% filter.species$plotID
if (TRUE %in% check.species) {
  #add species info to filter.herb if it exists
  filter.herb <- dplyr::left_join(filter.herb, filter.species, by = "plotID")
}else{
  PEcAn.logger::logger.info(paste0("No herbacious species info available for ", sitename))
}
#remove NAs
filter.herb <- na.omit(filter.herb)
filter.tree <- na.omit(filter.tree)
#Create year column
filter.tree$year <- format(as.Date(filter.tree$date.y, format="%Y-%m-%d"),"%Y")
filter.herb$year <- format(as.Date(filter.herb$collectDate.y, format="%Y-%m-%d"),"%Y")
#Rename NEON column names to match pecan functions
colnames(filter.tree) <- c("site_name", "plot", "Subplot", "species_USDA_symbol", "species", "taxonRank", "date", "DBH", "height", "year")
colnames(filter.herb) <- c("site_name", "plot", "Subplot", "plotSize", "clipArea", "dryMass", "date", "species_USDA_symbol", "species", "taxonRank", "year")
#Create veg_info object as a list
veg_info <- list()
#Set plot size as veg_info[[1]]
veg_info[[1]] <- list(tree_subplot = 400, herb_subplot = filter.herb$clipArea)
#Set filter.tree as veg_info[[2]]
veg_info[[2]] <- filter.tree
#Set filter.herb as veg_info[[3]]
veg_info[[3]] <- filter.herb


return(veg_info)
}
