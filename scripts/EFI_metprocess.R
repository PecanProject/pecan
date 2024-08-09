##############################################
#
#       EFI Forecasting Challenge 
#
###############################################
#set home directory as object (remember to change to your own directory before running this script)
homedir <- "/projectnb/dietzelab/ahelgeso"

library(PEcAn.all)
library(tidyverse)
source('/projectnb/dietzelab/ahelgeso/pecan/modules/data.atmosphere/R/download.NOAA_GEFS.R')
source('/projectnb/dietzelab/ahelgeso/pecan/modules/data.atmosphere/R/download.raw.met.module.R')

#read in .csv with site info 
setwd(file.path(homedir, "pecan/scripts/")) #remember to change to where you keep your dataprep .csv file with the site info
data_prep <- read.csv("dataprep_10_sites.csv") #this .csv file contains the sitename, BETY site id, location to store met files, model name, met source (from .xml), and the met output (from .xml) for each site you want to download met data
data_prep <- filter(data_prep, met_download == "metprocess")
sitename <- data_prep$site_name
site_id <- data_prep$siteid_BETY4
base_dir <- data_prep$base_dir
model_name <- data_prep$model_name4
met_source <- data_prep$input_met_source4
met_output <- data_prep$input_met_output4

#run info
start_date = as.Date(format(Sys.Date()-1, "%Y-%m-%d"))
end_date = as.Date(format(Sys.Date(), "%Y-%m-%d"))
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
                spin = NULL,
                overwrite = FALSE)
    
    
    
  }





