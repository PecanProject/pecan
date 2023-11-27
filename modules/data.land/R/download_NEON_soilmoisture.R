##' Download NEON Soil Water Content and Soil Salinity data by date and site name
##' 
##' @param site four letter NEON site code name(s). If no site is specified, it will download all of them (chr) (e.g "BART" or c("SRER", "KONA", "BART"))
##' @param avg averaging interval (minutes): 1, 30, or both ("all") . default returns both
##' @param var variable of interest: "SWC" (soil water content) or "SIC" (soil ion content) or both ("all") default returns both.
##'     Both variables will be saved in outdir automatically (chr)
##' @param startdate start date as YYYY-mm. If left empty, all data available will be downloaded (chr)
##' @param enddate start date as YYYY-mm. If left empty, all data available will be downloaded (chr) 
##' @param outdir out directory to store the following data:
##'     .rds list files of SWC and SIC data for each site and sensor position, 
##'     sensor positions .csv for each site, 
##'     variable description .csv file,
##'     readme .csv file
##' @return List of specified variable(s) AND prints the path to output folder
##' 
##' @author Juliette Bateman
##' 
##' @examples
##' \dontrun{
##' test <- download_NEON_soilmoisture(
##'   site = c("SRER", "BART", "KONA"),
##'   avg = 30,
##'   var = "SWC",
##'   startdate = "2019-01",
##'   enddate = "2020-01",
##'   outdir = getwd())}

## Install NEON libs
#devtools::install_github("NEONScience/NEON-geolocation/geoNEON")
#devtools::install_github("NEONScience/NEON-utilities/neonUtilities", force = TRUE)
#install.packages("BiocManager")
# BiocManager::install("rhdf5")


download_NEON_soilmoist <- function(site, avg = "all", var = "all",
                                    startdate = NA, enddate = NA,
                                    outdir) {
  
  
  #################### Data Download from NEON #################### 
  soil.raw = neonUtilities::loadByProduct(dpID = "DP1.00094.001", site = site, avg = avg, startdate = startdate, enddate = enddate, check.size = FALSE)
  
  # Export into new folder in outdir
  dir = paste0(outdir, "/NEONSoilMoist", "_", startdate, "-", enddate)
  dir.create(dir)
  
  #################### Clean-up Data Observations ####################
  # Only select data from list and remove flagged observations 
  if (avg == 30) {
    data.raw = soil.raw$SWS_30_minute %>% stats::na.omit()
  } else if (avg == 1) {
    data.raw = soil.raw$SWS_1_minute %>% stats::na.omit()
  } else {
    data.raw = list(soil.raw$SWS_1_minute, soil.raw$SWS_30_minute) %>% stats::na.omit()
  }
  
  # Separate variables, omit flagged data obs
  data.raw.SWC = (split(data.raw, data.raw$VSWCFinalQF))$'0' %>%
    dplyr::select(c("domainID", "siteID", "horizontalPosition", "verticalPosition", "startDateTime", "endDateTime", "VSWCMean", "VSWCMinimum", "VSWCMaximum", "VSWCVariance", "VSWCNumPts", "VSWCExpUncert", "VSWCStdErMean"))
  data.raw.SIC = (split(data.raw, data.raw$VSICFinalQF))$'0' %>%
    dplyr::select(c("domainID", "siteID", "horizontalPosition", "verticalPosition", "startDateTime", "endDateTime","VSICMean", "VSICMinimum", "VSICMaximum", "VSICVariance", "VSICNumPts", "VSICExpUncert", "VSICStdErMean"))
  
  data.raw.both = list(data.raw.SWC, data.raw.SIC)
  names(data.raw.both) <- c("SWC", "SIC")
  data.split.both = lapply(data.raw.both, function(x) split(x, x$siteID))
  
  # Separate dataframe into lists by site and sensor position
  data.SWC.sites = split(data.raw.SWC, data.raw.SWC$siteID)
  data.SIC.sites = split(data.raw.SIC, data.raw.SIC$siteID)
  for (i in 1:length(data.SWC.sites)){
    data.SWC.sites[i]=lapply(data.SWC.sites[i], function(x) split(x, list(x$horizontalPosition, x$verticalPosition)))
  }
  for (i in 1:length(data.SIC.sites)){
    data.SIC.sites[i]=lapply(data.SIC.sites[i], function(x) split(x, list(x$horizontalPosition, x$verticalPosition)))
  }
  
  #################### Save data into folders ####################
  
  # Saving metadata and site data lists as .rds files to outdir, organize into site specific folders 
  sensor.pos = split(soil.raw$sensor_positions_00094, soil.raw$sensor_positions_00094$siteID) 
  for (i in names(sensor.pos)){
    utils::write.csv(sensor.pos[[i]], file = paste0(dir, "/", i, "_sensor_positions.csv"))
  }
  for (i in names(data.SIC.sites)) {
    saveRDS(data.SIC.sites[[i]], file = paste0(dir, "/", i, "_SIC_data.rds"))
  }
  for (i in names(data.SWC.sites)) {
    saveRDS(data.SWC.sites[[i]], file = paste0(dir, "/", i, "_SWC_data.rds"))
  }
  for (i in 1:length(site)){
    folders = paste0(dir, "/", site[1:i])
    dir.create(folders[i])
    fs::file_move(paste0(dir, "/", site[i], "_sensor_positions.csv"), folders[i])
    fs::file_move(paste0(dir, "/", site[i], "_SIC_data.rds"), folders[i])
    fs::file_move(paste0(dir, "/", site[i], "_SWC_data.rds"), folders[i])
  }
  
  utils::write.csv(soil.raw$readme_00094, file = (paste0(dir,"/readme.csv")))
  utils::write.csv(soil.raw$variables_00094, file = paste0(dir, "/variable_description.csv"))
  
  # Return file path to data and print lists of 
  PEcAn.logger::logger.info("Done! NEON soil data has been downloaded and stored in ", paste0(dir), ".")
  if (var == "SWC") {
    data.SWC = data.SWC.sites
    return(data.SWC)
  } else if (var == "SIC") {
    data.SIC = data.SIC.sites
    return(data.SIC) 
  } else if (var == "all") {
    data.SWC <- data.SWC.sites
    data.SIC <- data.SIC.sites
    both.var = list(data.SWC, data.SIC)
    names(both.var) = c("SWC", "SIC")
    return(both.var)
  }
  
}
