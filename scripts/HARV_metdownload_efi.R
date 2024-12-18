#!/usr/bin/env Rscript
#load libraries
.libPaths(c("/projectnb/dietzelab/dietze/test-pecan/R/library",.libPaths()))
library(dplyr)
option_list = list(optparse::make_option("--start.date",
                                         default = Sys.Date(),
                                         type="character"),
                   optparse::make_option("--jumpback",
                                         default = 10,
                                         type="integer")
)
args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
if(is.null(args$jumpback)) args$jumpback = 10
jumpback = args$jumpback

#set fcn inputs
startdate = as.Date(args$start.date)
nc_dir = "/projectnb/dietzelab/ahelgeso/NOAA_met_data/"
site.lat = 42.5
site.lon = -72.15
sitename <- "HARV"
siteid <- 646
clim_dir <- "/projectnb/dietzelab/ahelgeso/NOAA_met_data_CH1/"
runDays <- format(seq(from=as.Date(startdate - lubridate::days(jumpback)),
               to=as.Date(as.character(startdate)),
               by="days"),"%Y-%m-%d")
print("args set")

for(t in seq_along(runDays)){
  print(runDays[t])
  
  #download met using EFI fcns
  tmp = PEcAn.data.atmosphere:::download.NOAA_GEFS_EFI(sitename = sitename,
                                                 outfolder = nc_dir,
                                                 start_date = runDays[t],
                                                 site.lat = site.lat,
                                                 site.lon = site.lon)
  #set up path for met2model
  output_path <- file.path("/projectnb/dietzelab/ahelgeso/NOAA_met_data/noaa/NOAAGEFS_1hr", sitename, runDays[t], "00")
  ########## Met2Model For SIPNET ##############
  outfolder = file.path(clim_dir, "noaa_clim", sitename, runDays[t])
  if(!dir.exists(outfolder)){dir.create(outfolder, recursive = TRUE)}
  
  in.path = output_path
  in.prefix = list.files(output_path)
  
  end_date = as.Date(runDays[t]) + lubridate::days(35)
  
  for(l in 1:length(in.prefix)){
    
    PEcAn.SIPNET::met2model.SIPNET(in.path = in.path, 
                                   in.prefix = in.prefix[l], 
                                   outfolder = outfolder, 
                                   start_date = runDays[t], 
                                   end_date = end_date,
                                   overwrite = FALSE,
                                   verbose = FALSE, 
                                   year.fragment = TRUE) 
    
  } 
  
}
print("done")

