##############################################
#
#       EFI Forecasting Challenge 
#
###############################################
#set home directory as object (remember to change to your own directory before running this script)
homedir <- "/projectnb/dietzelab/ahelgeso"

library(PEcAn.all)
library(tidyverse)
########## Site Info ###########
#read in .csv with site info
setwd(file.path(homedir, 'pecan/scripts/')) #remember to change to where you keep your dataprep .csv file with the site info
data_prep <- read.csv("dataprep_10_sites.csv") #this .csv file contains the NEON site id, BETY site id, and location where you want the met data saved. Remember to change to fit your sites and file path before running the script
data_prep <- filter(data_prep, met_download == "dataprep")
sitename <- data_prep$siteid_NEON4
siteid <- data_prep$siteid_BETY4
base_dir <- data_prep$base_dir4

#run info 
start_date = format(Sys.Date()-2, "%Y-%m-%d")

for(i in 1:length(sitename)){

###### Download Data ###########

download_noaa_files_s3 <- function(siteID, date, cycle, local_directory){
  
  Sys.setenv("AWS_DEFAULT_REGION" = "data",
             "AWS_S3_ENDPOINT" = "ecoforecast.org")
  
  object <- aws.s3::get_bucket("drivers", prefix=paste0("noaa/NOAAGEFS_1hr/",siteID,"/",date,"/",cycle))
  
  for(j in 1:length(object)){
    aws.s3::save_object(object[[j]], bucket = "drivers", file = file.path(local_directory, object[[j]]$Key))
  }
}


download_noaa_files_s3(siteID = sitename[i], date = as.Date(start_date), cycle = "00", local_directory <- base_dir[i] )# }


############ Downscale to 30 minutes ##############

input_path = file.path(base_dir[i], "noaa/NOAAGEFS_1hr/", sitename[i], "/", start_date, "/00/")
output_path = file.path(base_dir[i], "noaa/half_hour/", sitename[i], "/", start_date, "/")
files = list.files(input_path)

if(!dir.exists(output_path)){dir.create(output_path, recursive = TRUE)}

for(k in 1:length(files)){

input_file = paste0(input_path, files[k])
output_file = paste0(output_path, "Half_Hour_", files[k])
temporal_downscale_half_hour(input_file = input_file, output_file = output_file , overwrite = FALSE, hr = 0.5)

}



########## Met2Model For SIPNET ##############
outfolder = file.path(base_dir[i], "noaa_clim/", sitename[i], "/", start_date, "/")
if(!dir.exists(outfolder)){dir.create(outfolder, recursive = TRUE)}

in.path = dirname(output_path)
in.prefix = list.files(output_path)

end_date = as.Date(start_date) + lubridate::days(35)

for(l in 1:length(in.prefix)){
  
  PEcAn.SIPNET::met2model.SIPNET(in.path = in.path, 
                                 in.prefix = in.prefix[l], 
                                 outfolder = outfolder, 
                                 start_date = start_date, 
                                 end_date = end_date,
                                 overwrite = FALSE,
                                 verbose = FALSE, 
                                 year.fragment = TRUE) 
  
} 

##### register downloaded met to BETY ##############
files = list.files(outfolder)

### Get BETY information ###
bety <- dplyr::src_postgres(dbname   = 'bety', 
                            host     = 'psql-pecan.bu.edu', 
                            user     = 'bety', 
                            password = 'bety')
con <- bety$con



for(h in 1:length(files)){
  
  
  
  dbfile.input.insert(in.path = outfolder,
                      in.prefix = files[h],
                      startdate = start_date, 
                      enddate = end_date,
                      siteid = siteid[i],
                      mimetype = "text/csv",
                      formatname = "Sipnet.climna",
                      parentid=NA,
                      con = con, 
                      hostname=PEcAn.remote::fqdn(),
                      allow.conflicting.dates=TRUE,
                      ens=TRUE) 
  
  
  
  
} #closes files for loop

} #closes sitename for loop





