####################################################################################################
#
#
#
#
#    --- Last updated:  02.01.2019 By Shawn P. Serbin <sserbin@bnl.gov>
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
library(PEcAnAssimSequential)
library(nimble)
library(lubridate)
library(PEcAn.visualization)
#PEcAnAssimSequential::
library(rgdal) # need to put in assim.sequential
library(ncdf4) # need to put in assim.sequential
library(purrr)
library(listviewer)
library(dplyr)


# temporary step until we get this code integrated into pecan
# library(RCurl)
# script <- getURL("https://raw.githubusercontent.com/serbinsh/pecan/download_osu_agb/modules/data.remote/R/LandTrendr.AGB.R", 
#                  ssl.verifypeer = FALSE)
# eval(parse(text = script))
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## set run options, some of these should be tweaked or removed as requirements
work_dir <- "/data/bmorrison/sda/lai"
setwd(work_dir)  # best not to require setting wd and instead just providing full paths in functions

# Deifine observation - use existing or generate new? 
# set to a specific file, use that.
#observation <- ""
#observation = c("1000000048", "796")
#observation = c("1000000048", "796", "1100", "71", "954", "39")

# delete an old run
unlink(c('run','out','SDA'),recursive = T)

# grab multi-site XML file
settings <- read.settings("pecan_MultiSite_SDA.xml")

# doesn't work for one site
# observation <- c()
# for (i in seq_along(1:length(settings$run))) {
#   command <- paste0("settings$run$settings.",i,"$site$id")
#   obs <- eval(parse(text=command))
#   observation <- c(observation,obs)
# }

observation = "1000000048"

# what is this step for????  is this to get the site locations for the map??
if ("MultiSettings" %in% class(settings)) site.ids <- settings %>% 
  map(~.x[['run']] ) %>% map('site') %>% map('id') %>% unlist() %>% as.character()

# sample from parameters used for both sensitivity analysis and Ens
get.parameter.samples(settings, 
                      ens.sample.method = settings$ensemble$samplingspace$parameters$method)  
## Aside: if method were set to unscented, would take minimal changes to do UnKF
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#


#---------AGB----------#
PEcAn.logger::logger.info("**** Extracting LandTrendr AGB data for model sites ****")
bety <- list(user='bety', password='bety', host='localhost',
             dbname='bety', driver='PostgreSQL',write=TRUE)
con <- PEcAn.DB::db.open(bety)
bety$con <- con
site_ID <- observation
suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon, 
                                            ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})", 
                                            ids = site_ID, .con = con))
suppressWarnings(qry_results <- DBI::dbSendQuery(con,site_qry))
suppressWarnings(qry_results <- DBI::dbFetch(qry_results))
site_info <- list(site_id=qry_results$id, site_name=qry_results$sitename, lat=qry_results$lat, 
                  lon=qry_results$lon, time_zone=qry_results$time_zone)

data_dir <- "/data2/RS_GIS_Data/LandTrendr/LandTrendr_AGB_data"
med_agb_data <- extract.LandTrendr.AGB(site_info, "median", buffer = NULL, fun = "mean", 
                                       data_dir, product_dates=NULL, file.path(work_dir,"Obs"))
sdev_agb_data <- extract.LandTrendr.AGB(site_info, "stdv", buffer = NULL, fun = "mean", 
                                        data_dir, product_dates=NULL, file.path(work_dir,"Obs"))



med_agb_data_sda <- med_agb_data[[1]] %>% filter(Site_ID %in% site.ids)
sdev_agb_data_sda <- sdev_agb_data[[1]] %>% filter(Site_ID %in% site.ids)
site.order <- sapply(site.ids,function(x) which(med_agb_data_sda$Site_ID %in% x)) %>%
  as.numeric() %>% na.omit()
med_agb_data_sda <- med_agb_data_sda[site.order,]
sdev_agb_data_sda <- sdev_agb_data_sda[site.order,]


#----------LAI----------#
#set directory to output MODIS data too
data_dir <- "/data/bmorrison/sda/lai/modis_lai_data"

# get the site location information to grab the correct lat/lons for site + add info to the point_list
# ################ Not working on interactive job on MODEX
bety <- list(user='bety', password='bety', host='localhost',dbname='bety', driver='PostgreSQL',write=TRUE)
con <- PEcAn.DB::db.open(bety)
bety$con <- con

site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon,
                            ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})",
                            ids = observation, .con = con)
suppressWarnings(qry_results <- DBI::dbSendQuery(con,site_qry))
suppressWarnings(qry_results <- DBI::dbFetch(qry_results))
# site_info = data.frame()
# for (i in seq_along(1:length(settings$run))) {
#   id <- eval(parse(text = paste0("settings$run$settings.",i,"$site$id")))
#   name = eval(parse(text = paste0("settings$run$settings.", i, "$site$name")))
#   lat = eval(parse(text = paste0("settings$run$settings.", i, "$site$lat")))
#   lon = eval(parse(text = paste0("settings$run$settings.", i, "$site$lon")))
#   site_info = rbind(site_info,(cbind(id, name, lon, lat)), stringsAsFactors = F)
#   }
site_IDs <- qry_results$id
site_names <- qry_results$sitename
site_coords <- data.frame(cbind(qry_results$lon, qry_results$lat))
site_info = as.data.frame(cbind(site_IDs, site_names, site_coords))
names(site_info) = c("IDs", "Names", "Longitude", "Latitude")
site_info$Longitude = as.numeric(site_info$Longitude)
site_info$Latitude = as.numeric(site_info$Latitude)


library(doParallel)
cl <- parallel::makeCluster(5, outfile="")
doParallel::registerDoParallel(cl)

start = Sys.time()
data = foreach(i=1:nrow(site_info)) %dopar% PEcAn.data.remote::call_MODIS(start_date = "2000/01/01", end_date = "2010/12/31", band = "Lai_500m", product = "MOD15A2H", lat = site_info$Latitude[i], lon = site_info$Longitude[i], size = 0, band_qc = "FparLai_QC", band_sd = "LaiStdDev_500m", package_method = "MODISTools", QC_filter = T)
end = Sys.time()
difference = end-start
stopCluster(cl)

output = as.data.frame(data)

# LAI is every 7 days --> calculate the peak LAI for a year for each site
load('/data/bmorrison/sda/lai/modis_lai_data/modis_lai_output_5site.RData')

for (i in 1:nrow(site_info))
{
  name = as.character(site_info$Names[i], stringsAsFactor = F)
  g = which(round(output$lat, digits = 3) == round(site_info$Latitude[i], digits = 3))
  output$tile[g] = name
}



data = output
peak_lai = data.frame()
years = unique(year(as.Date(data$calendar_date, "%Y-%m-%d")))
for (i in 1:length(years))
{
  year = years[i]
  g = grep(data$calendar_date, pattern = year)
  d = data[g,]
  sites = unique(data$tile)
  for (j in 1:length(sites))
  {
    info = site_info[which(site_info$Names == sites[j]),]
    index = which(round(d$lat, digits = 3) == round(info$Latitude, digits = 3) & round(d$lon, digits = 3) == round(info$Longitude, digits = 3))
    
    if (length(index) > 0)
    {
      site = d[index,]
      site$band = info$ID
      max = which(site$data == max(site$data, na.rm = T))
      peak = site[max[1],]
      #peak$data = max
      #peak$sd = mean
      peak$calendar_date = paste("Year", year, sep = "_")
      peak$tile = sites[j]
      peak_lai = rbind(peak_lai, peak)
    }
    
  }
  
}


# sort the data by site so the correct values are placed into the resized data frames below.

peak_lai = peak_lai[order(peak_lai$tile), ]

# # separate data into hotdog style dataframes with row == site and columns = info/data for each site
med_lai_data = cbind(unique(peak_lai$band), unique(peak_lai$tile), as.data.frame(matrix(unlist(t(peak_lai$data)), byrow = T, length(unique(peak_lai$tile)), length(years))))
colnames(med_lai_data) = c("Site_ID", "Site_Name", unique(peak_lai$calendar_date))

sdev_lai_data = cbind(unique(peak_lai$band), unique(peak_lai$tile), as.data.frame(matrix(unlist(t(peak_lai$sd)), byrow = T, length(unique(peak_lai$tile)), length(years))))
colnames(sdev_lai_data) = c("Site_ID", "Site_Name", unique(peak_lai$calendar_date))

point_list$med_lai_data <- point_list$med_lai_data[[1]] %>% filter(Site_ID %in% site.ids)
point_list$stdv_lai <- point_list$stdv_lai[[1]] %>% filter(Site_ID %in% site.ids)
site.order <- sapply(site.ids,function(x) which(point_list$median_lai$Site_ID %in% x)) %>%
  as.numeric() %>% na.omit()
point_list$median_lai <- point_list$median_lai[site.order,]
point_list$stdv_lai <- point_list$stdv_lai[site.order,]

peak_lai_data_sda = point_list$median_lai
sdev_lai_data_sda = point_list$stdv_lai
# 
# 
# 
# # make sure agb and lai only use same dates (for now just to test sda, will fix later)
# date_agb = colnames(med_agb_data_sda)
# date_lai = colnames(peak_lai_data_sda)
# 
# if (length(date_agb) > length(date_lai))
# {
#     index = which(!(date_agb %in% date_lai))
#     med_agb_data_sda = med_agb_data_sda[,-index]
#     sdev_agb_data_sda = sdev_agb_data_sda[,-index]
# } 
# if (length(date_lai) > length(date_agb))
# {
#   index = which(!(date_lai %in% date_agb))
#   peak_lai_data_sda = peak_lai_data_sda[,-index]
#   sdev_lai_data_sda = sdev_lai_data_sda[,-index]
# }
# 
# # combine agb and lai datasets
# med_data_sda = list()
# med_data_da
# # point_list = list()
# # point_list$agb$median_agb = med_agb_data_sda
# # point_list$agb$stdv_agb = sdev_agb_data_sda
# # point_list$lai$peak_lai = peak_lai_data_sda
# # point_list$lai$stdv_lai = sdev_lai_data_sda
# 
# # 
# #point_list$agb$median_agb = as.character(point_list$agb$median_agb[[1]]) %>% filter(site_ID %in% site.ids)
# 

point_list = list()
point_list$median_lai = med_lai_data
point_list$sdev_lai = sdev_lai_data

point_list$median_lai <- point_list$median_lai[[1]] %>% filter(site_ID %in% site.ids)
point_list$stdv_lai <- point_list$stdv_lai[[1]] %>% filter(Site_ID %in% site.ids)
site.order <- sapply(site.ids,function(x) which(point_list$median_lai$Site_ID %in% x)) %>%
  as.numeric() %>% na.omit()
point_list$median_lai <- point_list$median_lai[site.order,]
point_list$stdv_lai <- point_list$stdv_lai[site.order,]

med_lai_data_sda = point_list$median_lai
sdev_lai_data_sda = point_list$sdev_lai

# truning lists to dfs  for both mean and cov
date.obs <- strsplit(names(med_lai_data_sda),"_")[3:length(med_lai_data_sda)] %>% map_chr(~.x[2]) %>% paste0(.,"/12/31")

obs.mean <- names(med_lai_data_sda)[3:length(med_lai_data_sda)] %>%
  map(function(namesl){
    ((med_lai_data_sda)[[namesl]] %>%
       map(~.x %>% as.data.frame %>% `colnames<-`(c('LAI'))) %>%
       setNames(site.ids[1:length(.)]))
  }) %>% setNames(date.obs)

obs.cov <-names(sdev_lai_data_sda)[3:length(sdev_lai_data_sda)] %>%
  map(function(namesl) {
    ((sdev_lai_data_sda)[[namesl]] %>%
       map( ~ (.x) ^ 2%>% as.matrix()) %>%
       setNames(site.ids[1:length(.)]))
  }) %>% setNames(date.obs)

#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## generate new settings object
new.settings <- PEcAn.settings::prepare.settings(settings)
#--------------------------------------------------------------------------------------------------#

#Construct.R(site.ids, "LAI", obs.mean[[1]], obs.cov[[1]])



#--------------------------------------------------------------------------------------------------#
## Run SDA


# sda.enkf(settings, obs.mean =obs.mean ,obs.cov = obs.cov,
#          control=list(trace=T,
#                       FF=F,
#                       interactivePlot=F,
#                       TimeseriesPlot=T,
#                       BiasPlot=F,
#                       plot.title="LAI SDA, 1 site",
#                       facet.plots=T,
#                       debug=F,
#                       pause=F))

#unlink(c('run','out','SDA'),recursive = T)

sda.enkf.multisite(new.settings, obs.mean =obs.mean ,obs.cov = obs.cov,
                   control=list(trace=T,
                                FF=F,
                                interactivePlot=F,
                                TimeseriesPlot=T,
                                BiasPlot=F,
                                plot.title="Sobol sampling - 5sites/15 Ensemble - LAI",
                                facet.plots=T,
                                debug=T,
                                pause=F))


#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## Wrap up
# Send email if configured
if (!is.null(settings$email) && !is.null(settings$email$to) && (settings$email$to != "")) {
  sendmail(settings$email$from, settings$email$to,
           paste0("SDA workflow has finished executing at ", base::date()))
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### EOF
