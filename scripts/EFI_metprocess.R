##############################################
#
#       EFI Forecasting Challenge 
#
###############################################
source('/projectnb/dietzelab/ahelgeso/pecan/modules/data.atmosphere/R/met.process.R')
source('/projectnb/dietzelab/ahelgeso/pecan/modules/data.atmosphere/R/download.raw.met.module.R')
source('/projectnb/dietzelab/ahelgeso/pecan/modules/data.atmosphere/R/GEFS_helper_functions.R')
source('/projectnb/dietzelab/ahelgeso/pecan/modules/data.atmosphere/R/download.NOAA_GEFS.R')
library(PEcAn.all)
library(tidyverse)

#read in .csv with site info 
setwd("/projectnb/dietzelab/ahelgeso/EFI_Forecast_Scripts/CSV/")
data_prep <- read.csv("data_prep_metprocess.csv")
sitename <- data_prep$site_name
site_id <- data_prep$siteid_BETY3
base_dir <- data_prep$base_dir
model_name <- data_prep$model_name3
met_source <- data_prep$input_met_source3
met_output <- data_prep$input_met_output3

#run info
start_date = as.Date(format(Sys.Date(), "%Y-%m-%d"))
end_date = as.Date(format(Sys.Date()+1, "%Y-%m-%d"))
host = list()
  host$name = "localhost"
dbparms = list()
  dbparms$dbname = "bety"
  dbparms$host = "psql-pecan.bu.edu"
  dbparms$user = "bety"
  dbparms$password = "bety"

#met.process
  for (i in 1:length(sitename)) {
    outfolder = file.path(base_dir[i], "noaa_clim/", sitename[i], "/", start_date, "/")
    if(!dir.exists(outfolder)){dir.create(outfolder, recursive = TRUE)}
    
    input_met = list()
    input_met$source = met_source[i]
    input_met$output = met_output[i] 
    
    site = list()
      site$id = site_id[i]
      site$name = sitename[i]
    
    model = model_name[i]
      
    met.process(site = site, 
                input_met = input_met, 
                start_date = start_date, 
                end_date = end_date, 
                model = model,
                host = host, 
                dbparms = dbparms, 
                dir = outfolder, 
                browndog = NULL, 
                spin = NULL,
                overwrite = FALSE)
    
    
    
  }





