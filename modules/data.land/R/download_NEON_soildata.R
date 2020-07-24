##' @name download_NEON_soildata
##' @description: 
##' Download NEON Soil Water Content and Soil Salinity data by date and site name
##' 
##' @param site four letter NEON site code name(s). If no site is specified, it will download all of them (chr) (e.g "BART" or multiple c("SRER", "KONA", "BART"))
##' @param avg averaging interval: either 30 minute OR 1 minute, default will return BOTH (chr) (e.g 1 or 30) 
##' @param var variable of interest, either "SWC" (soil water content) or "SIC" (soil ion content)
##' @param startdate start date as YYYY-mm. If left empty, all data will available will be downloaded (chr)
##' @param enddate start date as YYYY-mm. If left empty, all data will available will be downloaded (chr) 
##' @param outdir out directory to store .CSV files with individual site data
##' @return Data.frames for each site of interest loaded to the R Global Environment, AND saved as .csv files
##' 
##' @author Juliette Bateman
##' 
##' @example
##' \dontrun{
##' test <- download_NEON_soildata(
##'   site = c("SRER", "BART"),
##'   avg = 30,
##'   startdate = "2019-01",
##'   enddate = "2020-01",
##'   var = "SWC",
##'   outdir = getwd())}

## Install NEON libs
#devtools::install_github("NEONScience/NEON-geolocation/geoNEON")
#devtools::install_github("NEONScience/NEON-utilities/neonUtilities", force = TRUE)
#install.packages("BiocManager")
# BiocManager::install("rhdf5")


download_NEON_soildata <- function(site, avg = "all", var,
                                   startdate = NA, enddate = NA,
                                   outdir) {
  
  
  #################### Data Download from NEON #################### 
  soil.raw = neonUtilities::loadByProduct(dpID = "DP1.00094.001", site = "KONA", avg = avg, startdate = startdate, enddate = enddate, check.size = FALSE)
  
  # Only select data from list and remove flagged observations 
  if (avg == 30) {
    data.raw = soil.raw$SWS_30_minute
  } else if (avg == 1) {
    data.raw = soil.raw$SWS_1_minute
  } else {
    data.raw = list(soil.raw$SWS_1_minute, soil.raw$SWS_30_minute)
  }

  
  # Separate selected variable, omit NA values
  if (var == "SWC") {
    data.raw = (split(data.raw, data.raw$VSWCFinalQF))$'0'
    data.raw = data.raw[,-c(15:22)] %>% na.omit()
  } else if (var == "SIC") {
    data.raw = (split(data.raw, data.raw$VSICFinalQF))$'0'
    data.raw = data.raw[,-c(7:14)] %>% na.omit()
  }
  
  # Separate dataframe into list of each site of interest
  data.sites = data.raw %>% 
    split(data.raw$siteID) 
  
  # Separate each site by sensors (horizontal and vertical position)
  data.sitePositions = vector(mode = "list", length = length(data.sites))
  names(data.sitePositions) = names(data.sites)
  for (i in length(data.sites)) {
  data.sitePositions[[i]] = data.sites[[i]] %>% 
    split(list(data.sites[[i]]$horizontalPosition, data.sites[[i]]$verticalPosition))
  }
  
  # Load individual site list into the Global Environment
  data.sitePositions %>%
    list2env(envir = .GlobalEnv)
  
  # Save site lists as .rds files to outdir 
  for (i in names(data.sitePositions)){
    #rlist::list.save(data.sites[[i]], file = paste0(outdir, "/", i, ".json"), type = tools::file_ext(".json"))
    #write.csv(data.sites[[1]], paste0(outdir, "/", i,".csv"))
    saveRDS(data.sitePositions[[i]], file = paste0(outdir, "/", i, ".rds"))
  }
  
}

                     

