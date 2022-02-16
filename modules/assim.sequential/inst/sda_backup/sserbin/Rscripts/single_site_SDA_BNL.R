####################################################################################################
#
#     Single site LAI SDA
#
#
#    --- Last updated:  03.22.2019 By Shawn P. Serbin <sserbin@bnl.gov>
####################################################################################################


#---------------- Close all devices and delete all variables. -------------------------------------#
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
#--------------------------------------------------------------------------------------------------#


#---------------- Load required libraries ---------------------------------------------------------#
library(PEcAn.all)
library(PEcAn.SIPNET)
library(PEcAn.LINKAGES)
library(PEcAn.visualization)
library(PEcAn.assim.sequential)
library(nimble)
library(lubridate)
library(PEcAn.visualization)
#PEcAn.assim.sequential::
library(rgdal) # need to put in assim.sequential
library(ncdf4) # need to put in assim.sequential
library(purrr)
library(listviewer)
library(dplyr)

run_SDA <- TRUE #TRUE/FALSE
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## set run options, some of these should be tweaked or removed as requirements
work_dir <- "/data/sserbin/Modeling/sipnet/NASA_CMS_AGB_LAI"
setwd(work_dir)  # best not to require setting wd and instead just providing full paths in functions

# Deifine observation - use existing or generate new? 
# set to a specific file, use that.
observation <- c("1000000048")

# delete an old run
unlink(c('run','out','SDA'),recursive = T)

# grab multi-site XML file
settings <- read.settings("XMLs/pecan_US-CZ3_LAI_SDA.xml")


# what is this step for????  is this to get the site locations for the map??
if ("MultiSettings" %in% class(settings)) site.ids <- settings %>% 
  map(~.x[['run']] ) %>% map('site') %>% map('id') %>% unlist() %>% as.character()

# sample from parameters used for both sensitivity analysis and Ens
get.parameter.samples(settings, 
                      ens.sample.method = settings$ensemble$samplingspace$parameters$method)  
## Aside: if method were set to unscented, would take minimal changes to do UnKF
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## Prepare observational data - still very hacky here

# option 1: use existing observation file 
# if (observation!="new") {
#   load(observation)
#   site1 <- point_list
#   site1$median_AGB[[1]] %>%
#     filter(Site_ID!='772') -> site1$median_AGB[[1]]
#   site1$stdv_AGB[[1]] %>%
#     filter(Site_ID!='772') -> site1$stdv_AGB[[1]]
# }

# where to put MODIS LAI data?
data_dir <- "/data/sserbin/Modeling/sipnet/NASA_CMS_AGB_LAI/modis_lai_data"
parameters <- settings$run

# get MODIS data
#modis <- PEcAn.data.remote::call_MODIS(lat = as.numeric(parameters$site$lat), lon = as.numeric(parameters$site$lon), 
#                                       start_date = parameters$start.date, end_date = parameters$end.date, 
#                                       siteID = parameters$site$id, size = 0, product = "MOD15A2H", band = "Lai_500m", 
#                                       band_qc = "", band_sd = "LaiStdDev_500m", package_method = "MODISTools")
#modis <- PEcAn.data.remote::call_MODIS(lat = as.numeric(parameters$site$lat), lon = as.numeric(parameters$site$lon), 
#                                       start_date = "2001/01/01", end_date = "2002/01/01", 
#                                       size = 0, product = "MOD15A2H", band = "Lai_500m", 
#                                       band_qc = "", band_sd = "LaiStdDev_500m", package_method = "MODISTools")

if (!file.exists(file.path(data_dir,'modis_lai_output.RData'))) {
  modis <- call_MODIS(product = "MOD15A2H", band = "Lai_500m", start_date = "2001001", end_date = "2010365", 
                      lat = as.numeric(parameters$site$lat), lon = as.numeric(parameters$site$lon), size = 0, 
                      band_qc = "FparLai_QC", band_sd = "LaiStdDev_500m", 
                      package_method = "MODISTools")
  save(modis, file = file.path(data_dir,'modis_lai_output.RData'))
  
} else {
  load(file = file.path(data_dir,'modis_lai_output.RData'))
}

# 
bety <- list(user='bety', password='bety', host='localhost',
             dbname='bety', driver='PostgreSQL',write=TRUE)
con <- PEcAn.DB::db.open(bety)
bety$con <- con

suppressWarnings(Site_Info <- PEcAn.DB::query.site(observation, con))
Site_Info
Site_ID <- Site_Info$id
Site_Name <- Site_Info$sitename

#plot(lubridate::as_date(modis$calendar_date), modis$data, type="l")

peak_lai <- vector()
years <- unique(year(as.Date(modis$calendar_date, "%Y-%m-%d")))
for (i in seq_along(years)) {
  year <- years[i]
  g <- grep(modis$calendar_date, pattern = year)
  d <- modis[g,]
  max <- which(d$data == max(d$data, na.rm = T))
  peak <- d[max,][1,]
  peak$calendar_date = paste("Year", year, sep = "_")
  peak_lai <- rbind(peak_lai, peak)
}

# transpose the data
median_lai = as.data.frame(cbind(Site_ID, Site_Name, t(cbind(peak_lai$data))), stringsAsFactors = F)
colnames(median_lai) = c("Site_ID", "Site_Name", peak_lai$calendar_date)
median_lai[3:length(median_lai)] = as.numeric(median_lai[3:length(median_lai)])

stdv_lai = as.data.frame(cbind(Site_ID, Site_Name, t(cbind(peak_lai$sd))), stringsAsFactors = F)
colnames(stdv_lai) = c("Site_ID", "Site_Name", peak_lai$calendar_date)
stdv_lai[3:length(stdv_lai)] = as.numeric(stdv_lai[3:length(stdv_lai)])

point_list = list()
point_list$median_lai = median_lai
point_list$stdv_lai = stdv_lai

## needed for landtrendr for nested lists. Lai isn't as nested
#point_list$median_lai <- point_list$median_lai[[1]] %>% filter(Site_ID %in% site.ids)
#point_list$stdv_lai <- point_list$stdv_lai[[1]] %>% filter(Site_ID %in% site.ids)
site.order <- sapply(site.ids,function(x) which(point_list$median_lai$Site_ID %in% x)) %>%
  as.numeric() %>% na.omit()
point_list$median_lai <- point_list$median_lai[site.order,]
point_list$stdv_lai <- point_list$stdv_lai[site.order,]

# truning lists to dfs  for both mean and cov
date.obs <- strsplit(names(point_list$median_lai),"_")[3:length(point_list$median_lai)] %>% map_chr(~.x[2]) %>% paste0(.,"/07/15") 

obs.mean <- names(point_list$median_lai)[3:length(point_list$median_lai)] %>%
  map(function(namesl){
    ((point_list$median_lai)[[namesl]] %>% 
       map(~.x %>% as.data.frame %>% `colnames<-`(c('LAI'))) %>% 
       setNames(site.ids[1:length(.)])
    )
  }) %>% setNames(date.obs)

obs.cov <-names(point_list$stdv_lai)[3:length(point_list$median_lai)] %>%
  map(function(namesl) {
    ((point_list$stdv_lai)[[namesl]] %>%
       map( ~ (.x) ^ 2 %>% as.matrix()) %>%
       setNames(site.ids[1:length(.)]))
    
  }) %>% setNames(date.obs)

# check input data - after creating list of lists
PEcAn.assim.sequential::Construct.R(site.ids, "LAI", obs.mean[[1]], obs.cov[[1]])
PEcAn.assim.sequential::Construct.R(site.ids, "LAI", obs.mean[[10]], obs.cov[[10]])
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## generate new settings object
new.settings <- PEcAn.settings::prepare.settings(settings)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## Run SDA
if (run_SDA) {
  #sda.enkf.multisite(new.settings, obs.mean =obs.mean ,obs.cov = obs.cov, 
  
  sda.enkf.multisite(settings, obs.mean =obs.mean ,obs.cov = obs.cov,
                     control=list(trace=T,
                                  FF=F,
                                  interactivePlot=F,
                                  TimeseriesPlot=T,
                                  BiasPlot=F,
                                  plot.title="LAI SDA, 1 site",
                                  facet.plots=T,
                                  debug=T,
                                  pause=F))
  
  # sda.enkf(settings, obs.mean = obs.mean ,obs.cov = obs.cov,
  #                             control=list(trace=T,
  #                                          FF=F,
  #                                          interactivePlot=F,
  #                                          TimeseriesPlot=T,
  #                                          BiasPlot=F,
  #                                          plot.title="LAI SDA, 1 site",
  #                                          facet.plots=T,
  #                                          debug=T,
  #                                          pause=F))

} else {
  print("*** Not running SDA ***")
}

#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## Wrap up
# Send email if configured
#if (!is.null(settings$email) && !is.null(settings$email$to) && (settings$email$to != "")) {
#  sendmail(settings$email$from, settings$email$to,
#           paste0("SDA workflow has finished executing at ", base::date()))
#}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### EOF
