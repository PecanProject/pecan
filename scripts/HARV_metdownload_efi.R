#load libraries
library(dplyr)
#load fcns
source("/projectnb/dietzelab/ahelgeso/Forecast_Scripts/download_noaa_gefs_efi.R")
source("/projectnb/dietzelab/ahelgeso/Forecast_Scripts/noaa_gefs_efi_helper.R")
#set fcn inputs
startdate = format(Sys.Date(), "%Y-%m-%d")
nc_dir = "/projectnb/dietzelab/ahelgeso/NOAA_met_data/"
site.lat = 42.5
site.lon = -72.15
sitename <- "HARV"
siteid <- 646
clim_dir <- "/projectnb/dietzelab/ahelgeso/NOAA_met_data_CH1/"

#download met using EFI fcns
download_NOAA_GEFS_EFI(sitename = sitename,
                       outfolder = nc_dir,
                       start_date = startdate,
                       site.lat = site.lat,
                       site.lon = site.lon)
#set up path for met2model
output_path <- file.path("/projectnb/dietzelab/ahelgeso/NOAA_met_data/noaa/NOAAGEFS_1hr", sitename, startdate, "00")
########## Met2Model For SIPNET ##############
outfolder = file.path(clim_dir, "noaa_clim", sitename, startdate)
if(!dir.exists(outfolder)){dir.create(outfolder, recursive = TRUE)}

in.path = output_path
in.prefix = list.files(output_path)

end_date = as.Date(startdate) + lubridate::days(35)

for(l in 1:length(in.prefix)){
  
  PEcAn.SIPNET::met2model.SIPNET(in.path = in.path, 
                                 in.prefix = in.prefix[l], 
                                 outfolder = outfolder, 
                                 start_date = startdate, 
                                 end_date = end_date,
                                 overwrite = FALSE,
                                 verbose = FALSE, 
                                 year.fragment = TRUE) 
  
} 

