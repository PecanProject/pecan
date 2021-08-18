#function inputs
start_date = as.Date("2020-01-01")
end_date = as.Date("2021-09-01")
outfolder = "/projectnb/dietzelab/ahelgeso/NEON_ic_data/"
sitename = "HARV"

extract_NEON_veg <- function(start_date, end_date, outfolder, sitename){
#Set store_dir to point to dietzelab/neon_store
store_dir = "/projectnb/dietzelab/neon_data"
#Load in NEON datasets
#Only need to run neon_download once
neonstore::neon_download("DP1.10098.001", dir = store_dir, table = NA, site = sitename, start_date = start_date, end_date = end_date, type = "basic",api = "https://data.neonscience.org/api/v0")
temp.veg <- neonUtilities::stackFromStore(filepaths=store_dir,dpID="DP1.10098.001",pubdate="2021-06-01",package="basic")
joined.veg <- dplyr::left_join(temp.veg$vst_mappingandtagging, temp.veg$vst_apparentindividual, by = "individualID")

#Filter joined.veg for required information: DBH, tree height, and species
filter.veg <- dplyr::select(joined.veg, siteID.x, plotID.x, subplotID.x, taxonID, scientificName, taxonRank, date.y, stemDiameter, height)
#Filter for most recent record
filter.date <- dplyr::filter(filter.veg, date.y >= start_date)
#Create year column
filter.date$year <- format(as.Date(filter.date$date.y, format="%d/%m/%Y"),"%Y")
#Rename NEON column names to match pecan functions
colnames(filter.date) <- c("site_name", "plot", "Subplot", "species_USDA_symbol", "species", "taxonRank", "date", "DBH", "height", "year")
#Set filter.date as veg_info[[2]]
veg_info[[2]] <- filter.date
#Set plot size as veg_info[[1]]
veg_info[[1]] <- list(area = 400)

return(veg_info)
}