
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

#--------------------------------------------------------------------------------------------------#

# delete an old run
unlink(c('run','out','SDA'),recursive = T)

# grab multi-site XML file
settings <- read.settings("pecan_MultiSite_SDA_LAI_AGB_2_Sites.xml")

# doesn't work for one site
observation <- c()
for (i in seq_along(1:length(settings$run))) {
  command <- paste0("settings$run$settings.",i,"$site$id")
  obs <- eval(parse(text=command))
  observation <- c(observation,obs)
}

#observation = "1000000048"

# what is this step for????  is this to get the site locations for the map??
if ("MultiSettings" %in% class(settings)) site.ids <- settings %>% 
  map(~.x[['run']] ) %>% map('site') %>% map('id') %>% unlist() %>% as.character()

# sample from parameters used for both sensitivity analysis and Ens
get.parameter.samples(settings, 
                      ens.sample.method = settings$ensemble$samplingspace$parameters$method)  
## Aside: if method were set to unscented, would take minimal changes to do UnKF
#--------------------------------------------------------------------------------------------------#


# get the site location information to grab the correct lat/lons for site + add info to the point_list
# ################ Not working on interactive job on MODEX

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


load('/data/bmorrison/sda/lai/modis_lai_data/med_agb_data.Rdata')
load( '/data/bmorrison/sda/lai/modis_lai_data/sdev_agb_data.Rdata')
load('/data/bmorrison/sda/lai/modis_lai_data/modis_lai_output_2_site.Rdata')

#rename tiles by actual site name
for (i in 1:length(site_info$site_name))
{
  name = as.character(site_info$site_name[i], stringsAsFactor = F)
  g = which(round(output$lat, digits = 3) == round(site_info$lat[i], digits = 3))
  output$tile[g] = name
}

# compute peak lai per year
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
    #info = site_info[which(site_info$site_name == sites[j]),]
    index = which(round(d$lat, digits = 3) == round(site_info$lat, digits = 3) & round(d$lon, digits = 3) == round(site_info$lon, digits = 3))
    
    if (length(index) > 0)
    {
      site = d[index,]
      site$band = site_info$site_id
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
med_lai_data$Site_ID = as.character(med_lai_data$Site_ID)
med_lai_data = list(med_lai_data)

sdev_lai_data = cbind(unique(peak_lai$band), unique(peak_lai$tile), as.data.frame(matrix(unlist(t(peak_lai$sd)), byrow = T, length(unique(peak_lai$tile)), length(years))))
colnames(sdev_lai_data) = c("Site_ID", "Site_Name", unique(peak_lai$calendar_date))
sdev_lai_data$Site_ID = as.character(sdev_lai_data$Site_ID)
sdev_lai_data = list(sdev_lai_data)


#med_lai_data = list(med_lai_data)
med_lai_data_sda <- med_lai_data[[1]] %>% filter(Site_ID %in% site.ids)
sdev_lai_data_sda <- sdev_lai_data[[1]] %>% filter(Site_ID %in% site.ids)
site.order <- sapply(site.ids,function(x) which(med_lai_data_sda$Site_ID %in% x)) %>%
  as.numeric() %>% na.omit()
med_lai_data_sda <- med_lai_data_sda[site.order,]
sdev_lai_data_sda <- sdev_lai_data_sda[site.order,]


#make sure agb and lai only use same dates (for now just to test sda, will fix later)
date_agb = colnames(med_agb_data_sda)
date_lai = colnames(med_lai_data_sda)

if (length(date_agb) > length(date_lai))
{
  index = which(!(date_agb %in% date_lai))
  med_agb_data_sda = med_agb_data_sda[,-index]
  sdev_agb_data_sda = sdev_agb_data_sda[,-index]
}
if (length(date_lai) > length(date_agb))
{
  index = which(!(date_lai %in% date_agb))
  med_lai_data_sda = med_lai_data_sda[,-index]
  sdev_lai_data_sda = sdev_lai_data_sda[,-index]
} 


### REFORMAT ALL DATA BY YEAR INSTEAD OF SITE HOTDOG STYLE. COMBINE AGB + LAI INTO 1 MED + 1 SDEV LIST(S).
med_data = as.data.frame(cbind(colnames(med_lai_data_sda[,3:ncol(med_lai_data_sda)]), med_lai_data_sda$Site_ID,  unlist(med_agb_data_sda[,3:ncol(med_agb_data_sda)]), unlist(med_lai_data_sda[,3:ncol(med_lai_data_sda)])), row.names = F, stringsAsFactors = F)
names(med_data) = c("date", "site_id", "med_agb", "med_lai")
med_data = med_data[order(med_data$date),]
med_data$date = as.character(med_data$date)
med_data$site_id = as.character(med_data$site_id, stringsAsFactors = F)
med_data$med_lai = as.numeric(med_data$med_lai, stringsAsFactors = F)
med_data$med_agb = as.numeric(med_data$med_agb, stringsAsFactors = F)
med_data = med_data %>% 
  split(.$date)

date.obs <- strsplit(names(med_data), "_") %>%
  map_chr(~.x[2]) %>% paste0(.,"/07/15")


med_data = names(med_data) %>%
  map(function(namesl){
    med_data[[namesl]] %>%
      split(.$site_id) %>%
      map(~.x[3:4] %>% setNames(c("AbvGrndWood", "LAI"))) %>%
      setNames(site.ids)
  }) %>% setNames(date.obs)

names = names(med_data)
for (i in 1:length(names))
{
  for (j in 1:length(names(med_data[[names[1]]])))
  {
    rownames(med_data[[i]][[j]]) = NULL
  }
}


sdev_data = as.data.frame(cbind(colnames(sdev_lai_data_sda[,3:ncol(sdev_lai_data_sda)]), sdev_lai_data_sda$Site_ID,  unlist(sdev_agb_data_sda[,3:ncol(sdev_agb_data_sda)]),  rep(0, nrow(sdev_lai_data_sda)), rep(0, nrow(sdev_lai_data_sda)),unlist(sdev_lai_data_sda[,3:ncol(sdev_lai_data_sda)])), row.names = F, stringsAsFactors =F)
names(sdev_data) = c("date", "site_id", "sdev_agb", "h1", "h2", "sdev_lai")
sdev_data = sdev_data[order(sdev_data$date),]
sdev_data$date = as.character(sdev_data$date, stringsAsFactors = F)
sdev_data$site_id = as.character(sdev_data$site_id, stringsAsFactors = F)
sdev_data$sdev_lai = as.numeric(sdev_data$sdev_lai, stringsAsFactors = F)
sdev_data$sdev_agb = as.numeric(sdev_data$sdev_agb, stringsAsFactors = F)
sdev_data$h1 = as.numeric(sdev_data$h1)
sdev_data$h2 = as.numeric(sdev_data$h2)
sdev_data = sdev_data %>%
  split(.$date) 

sdev_data = names(sdev_data) %>%
  map(function(namesl){
    sdev_data[[namesl]] %>%
      split(.$site_id) %>%
      map(~matrix(data = .x[3:6]^2, nrow = 2, ncol = 2)) %>%
      setNames(site.ids)
  }) %>% setNames(date.obs)


obs.mean = med_data

obs.cov = sdev_data

new.settings <- PEcAn.settings::prepare.settings(settings)

#unlink(c('run','out','SDA'),recursive = T)

sda.enkf.multisite(new.settings, obs.mean =obs.mean ,obs.cov = obs.cov,
                   control=list(trace=TRUE, 
                                FF=FALSE,
                                interactivePlot=FALSE, 
                                TimeseriesPlot=FALSE,
                                BiasPlot=FALSE,
                                plot.title=NULL,
                                facet.plots=4,
                                debug=F,
                                pause=FALSE))


