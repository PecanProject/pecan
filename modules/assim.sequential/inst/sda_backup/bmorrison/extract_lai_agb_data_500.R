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
library(furrr)
library(tictoc)

work_dir <- "/data/bmorrison/sda/500_site_run"

# delete an old run
#unlink(c('run','out','SDA'),recursive = T)

# grab multi-site XML file
settings <- read.settings("pecan_MultiSite_SDA_LAI_AGB_sitegroup_500.xml")

if ("sitegroup" %in% names(settings)){
  if (is.null(settings$sitegroup$nSite)){
    settings <- PEcAn.settings::createSitegroupMultiSettings(settings,
                                                             sitegroupId = settings$sitegroup$id)
  } else {
    settings <- PEcAn.settings::createSitegroupMultiSettings(settings,
                                                             sitegroupId = settings$sitegroup$id,
                                                             nSite = settings$sitegroup$nSite)
  }
  settings$sitegroup <- NULL ## zero out so don't expand a second time if re-reading
}


# doesn't work for one site
observation <- c()
for (i in seq_along(1:length(settings$run))) {
  command <- paste0("settings$run$settings.",i,"$site$id")
  obs <- eval(parse(text=command))
  observation <- c(observation,obs)
}

# what is this step for????  is this to get the site locations for the map??
if ("MultiSettings" %in% class(settings)) site.ids <- settings %>% 
  map(~.x[['run']] ) %>% map('site') %>% map('id') %>% unlist() %>% as.character()


sites_500 = observation
load('/data/bmorrison/sda/500_site_run/all_lai_data.Rdata')
sites_200 = sort(unique(output$site_id))

# list for 500 sites
PEcAn.logger::logger.info("**** Extracting LandTrendr AGB data for model sites ****")
bety <- list(user='bety', password='bety', host='localhost',
             dbname='bety', driver='PostgreSQL',write=TRUE)
con <- PEcAn.DB::db.open(bety)
bety$con <- con
site_ID <- sites_500
suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon,
                                            ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})",
                                            ids = site_ID, .con = con))

suppressWarnings(qry_results <- DBI::dbSendQuery(con,site_qry))
suppressWarnings(qry_results <- DBI::dbFetch(qry_results))
sites_500 <- list(site_id=qry_results$id, site_name=qry_results$sitename, lat=qry_results$lat,
                  lon=qry_results$lon, time_zone=qry_results$time_zone)

# list for previous 200 sites
PEcAn.logger::logger.info("**** Extracting LandTrendr AGB data for model sites ****")
bety <- list(user='bety', password='bety', host='localhost',
             dbname='bety', driver='PostgreSQL',write=TRUE)
con <- PEcAn.DB::db.open(bety)
bety$con <- con
site_ID <- sites_200
suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon,
                                            ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})",
                                            ids = site_ID, .con = con))

suppressWarnings(qry_results <- DBI::dbSendQuery(con,site_qry))
suppressWarnings(qry_results <- DBI::dbFetch(qry_results))
sites_200 <- list(site_id=qry_results$id, site_name=qry_results$sitename, lat=qry_results$lat,
                  lon=qry_results$lon, time_zone=qry_results$time_zone)

# remove sites that were done from the 200 site run
sites_500_xy = as.data.frame(cbind(sites_500$lon, sites_500$lat))
sites_200_xy = as.data.frame(cbind(sites_200$lon, sites_200$lat))

remove = vector()
for (i in 1:nrow(sites_200_xy))
{
  index = which(sites_500_xy$V1 == sites_200_xy$V1[i] & sites_500_xy$V2 == sites_200_xy$V2[i])
  remove = c(remove, index)
}

observation = sort(c(sites_500$site_id[-remove]))



############################ EXTRACT SITE INFORMATION FROM XML TO DOWNLOAD DATA + RUN SDA ###########################
################ Not working on interactive job on MODEX
observations = observation
lai_data = data.frame()
for (i in :37)
{
  start = (1+((i-1)*10))
  end = start+9
  obs = observations[start:end]

  working = print(paste("working on: ", i))
  sites = print(obs)
  PEcAn.logger::logger.info("**** Extracting LandTrendr AGB data for model sites ****")
  bety <- list(user='bety', password='bety', host='localhost',
               dbname='bety', driver='PostgreSQL',write=TRUE)
  con <- PEcAn.DB::db.open(bety)
  bety$con <- con
  site_ID <- obs
  suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon,
                                              ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})",
                                              ids = site_ID, .con = con))

  suppressWarnings(qry_results <- DBI::dbSendQuery(con,site_qry))
  suppressWarnings(qry_results <- DBI::dbFetch(qry_results))
  site_info <- list(site_id=qry_results$id, site_name=qry_results$sitename, lat=qry_results$lat,
                    lon=qry_results$lon, time_zone=qry_results$time_zone)


  lai = call_MODIS(outdir = NULL, var = "LAI", site_info = site_info, product_dates = c("1980001", "2018365"),
                   run_parallel = TRUE, ncores = 10, product = "MOD15A2H", band = "LaiStdDev_500m",
                   package_method = "MODISTools", QC_filter = TRUE, progress = FALSE)

  #lai_data = rbind(lai_data, lai)
  sd = lai
  save(sd, file = paste('/data/bmorrison/sda/500_site_run/lai_sd_sites_', i+16, '.Rdata', sep = ""))
  
}

observation = observations
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
# # output folder for the data
# data_dir <- "/data2/RS_GIS_Data/LandTrendr/LandTrendr_AGB_data"
# 
# # # extract the data
# med_agb_data <- extract.LandTrendr.AGB(site_info, "median", buffer = NULL, fun = "mean",
#                                        data_dir, product_dates=NULL, file.path(work_dir,"Obs"))[[1]]
# 
# sdev_agb_data <- extract.LandTrendr.AGB(site_info, "stdv", buffer = NULL, fun = "mean",
#                                         data_dir, product_dates=NULL, file.path(work_dir,"Obs"))[[1]]
# 
# # 
# # ndates = colnames(med_agb_data)[-c(1:2)]
# # 
# med_agb_data$Site_Name = as.character(med_agb_data$Site_Name, stringsAsFactors = FALSE)
# med_agb_data = reshape2::melt(med_agb_data, id.vars = "Site_ID", measure.vars = colnames(med_agb_data)[-c(1:2)])
# 
# sdev_agb_data$Site_Name = as.character(sdev_agb_data$Site_Name, stringsAsFactors = FALSE)
# sdev_agb_data = reshape2::melt(sdev_agb_data, id.vars = "Site_ID", measure.vars = colnames(sdev_agb_data)[-c(1:2)])
# 
# agb_data = as.data.frame(cbind(med_agb_data, sdev_agb_data$value))
# names(agb_data) = c("Site_ID", "Date", "Median", "SD")
# agb_data$Date = as.character(agb_data$Date, stringsAsFactors = FALSE)
# 
# #save AGB data into long style
# save(agb_data, file = '/data/bmorrison/sda/500_site_run/agb_data_sites_500.Rdata')

######### calculate peak_lai
# already in long format style for dataframe
names(lai_sd) = c("modis_date", "calendar_date", "band", "tile", "site_id", "lat", "lon", "pixels", "sd", "qc")
output = cbind(lai_data, lai_sd$sd)
names(output) = c(names(lai_data), "sd")
#output = as.data.frame(data)
save(output, file = '/data/bmorrison/sda/lai/50_site_run/all_lai_data.Rdata')

# remove extra data
output = output[,c(5, 2, 9, 11)]
colnames(output) = names(agb_data) # should be Site_ID, Date, Median, SD

data = output
data = data[-which(data$SD > 6),]
peak_lai = data.frame()


### Mikes way to do peak LAI for the summer months (weighted precision)
years = unique(year(as.Date(data$Date, "%Y-%m-%d")))
for (i in 1:length(years))
{
  d = data[grep(data$Date, pattern = years[i]),]
  sites = unique(d$Site_ID)
  for (j in 1:length(sites))
  {
    index = which(d$Site_ID == site_info$site_id[j]) #which(round(d$lat, digits = 3) == round(site_info$lat[j], digits = 3) & round(d$lon, digits = 3) == round(site_info$lon[j], digits = 3))
    site = d[index,]
    if (length(index) > 0)
    {
      dates = as.Date(site$Date, format = "%Y-%m-%d")
      index = which(dates >= as.Date(paste(years[i], "06-15", sep = "-")) & dates <= as.Date(paste(years[i], "08-15", sep = "-")))
      info = site[index,]
      
      weights = info$SD/sum(info$SD)
      mean = sum(info$Median*weights)
      sd = sum(info$SD*weights)
      
      output = as.data.frame(cbind(sites[j], paste(years[i], "07-15", sep = "-"), mean, sd), stringsAsFactors = F)
      names(output) = c("Site_ID", "Date", "Median", "SD")
      peak_lai = rbind(peak_lai, output)
    }
  }
}

peak_lai$Site_ID = as.numeric(peak_lai$Site_ID)
peak_lai$Date = as.Date(peak_lai$Date)
peak_lai$Median = as.numeric(peak_lai$Median)
peak_lai$SD = as.numeric(peak_lai$SD)
peak_lai$Date = paste("Year", year(peak_lai$Date), sep = "_")




# compute peak lai per year
# data = output
# peak_lai = data.frame()
# years = unique(year(as.Date(data$Date, "%Y-%m-%d")))
# for (i in seq_along(years))
# {
#   d = data[grep(data$Date, pattern = years[i]),]
#   sites = unique(d$Site_ID)
#   for (j in seq_along(sites))
#   {
#     index = which(d$Site_ID == site_info$site_id[j]) #which(round(d$lat, digits = 3) == round(site_info$lat[j], digits = 3) & round(d$lon, digits = 3) == round(site_info$lon[j], digits = 3))
#     site = d[index,]
#     #count = print(nrow(site))
#     if (length(index) > 0)
#     {
#       # peak lai is the max value that is the value <95th quantile to remove potential outlier values
#       max = site[which(site$Median == max(site$Median[which(site$Median <= quantile(site$Median, probs = 0.95))], na.rm = T))[1],] #which(d$Median == max(d$Median[index], na.rm = T))[1]
#       peak = data.frame(max$Site_ID, Date = paste("Year", years[i], sep = "_"), Median = max$Median, SD = max$SD)
#       peak_lai = rbind(peak_lai, peak)
# 
#     }
#   }
# }
# 
# # a fix for low SD values because of an issue with MODIS LAI error calculations. Reference: VISKARI et al 2014.
# peak_lai$SD[peak_lai$SD < 0.66] = 0.66
# 
# #output data
# names(peak_lai) = c("Site_ID", "Date", "Median", "SD")
# save(peak_lai, file = '/data/bmorrison/sda/500_site_run/peak_lai_data_500.Rdata')


# ######################### TIME TO FIX UP THE OBSERVED DATASETS INTO A FORMAT THAT WORKS TO MAKE OBS.MEAN and OBS.COV FOR SDA ########################
peak_lai$Site_ID = as.numeric(as.character(peak_lai$Site_ID, stringsAsFactors = F))
peak_lai$Date = as.character(peak_lai$Date, stringsAsFactors = F)

observed_vars = c("AbvGrndWood", "LAI")


# merge agb and lai dataframes and places NA values where data is missing between the 2 datasets
observed_data = merge(agb_data, peak_lai, by = c("Site_ID", "Date"), all = T)
names(observed_data) = c("Site_ID", "Date", "med_agb", "sdev_agb", "med_lai", "sdev_lai")

# order by year
observed_data = observed_data[order(observed_data$Date),]

#sort by date
dates = sort(unique(observed_data$Date))

# create the obs.mean list --> this needs to be adjusted to work with load.data in the future (via hackathon)
obs.mean = data.frame(date = observed_data$Date, site_id = observed_data$Site_ID, med_agb = observed_data$med_agb, med_lai = observed_data$med_lai)
obs.mean$date = as.character(obs.mean$date, stringsAsFactors = FALSE)

obs.mean = obs.mean %>%
  split(.$date)

# change the dates to be middle of the year
date.obs <- strsplit(names(obs.mean), "_") %>%
  map_chr(~.x[2]) %>% paste0(.,"/07/15")

obs.mean = names(obs.mean) %>%
  map(function(namesl){
    obs.mean[[namesl]] %>%
      split(.$site_id) %>%
      map(~.x[3:4] %>% setNames(c("AbvGrndWood", "LAI")) %>% `row.names<-`(NULL))
    #setNames(site.ids)
  }) %>% setNames(date.obs)

#remove NA data as this will crash the SDA. Removes rown numbers (may not be nessesary)
names = date.obs
for (name in names)
{
  for (site in names(obs.mean[[name]]))
  {
    na_index = which(!(is.na(obs.mean[[ name]][[site]])))
    colnames = names(obs.mean[[name]][[site]])
    if (length(na_index) > 0)
    {
      obs.mean[[name]][[site]] = obs.mean[[name]][[site]][na_index]
    }
  }
}

# fillers are 0's for the covariance matrix. This will need to change for differing size matrixes when more variables are added in.
# filler_0 = as.data.frame(matrix(0, ncol = length(observed_vars), nrow = nrow(observed_data)))
# names(filler_0) = paste0("h", seq_len(length(observed_vars)))

# create obs.cov dataframe -->list by date
obs.cov = data.frame(date = observed_data$Date, site_id = observed_data$Site_ID, sdev_agb = observed_data$sdev_agb, sdev_lai = observed_data$sdev_lai)#, filler_0)
obs.cov$date = as.character(obs.cov$date, stringsAsFactors = F)

obs.cov = obs.cov %>%
  split(.$date)

obs.cov = names(obs.cov) %>%
  map(function(namesl){
    obs.cov[[namesl]] %>%
      split(.$site_id) %>%
      map(~.x[3:4]^2 %>% unlist %>% diag(nrow = 2, ncol = 2) ) 
  }) %>% setNames(date.obs)


names = date.obs
for (name in names)
{
  for (site in names(obs.cov[[name]]))
  {
    bad = which(apply(obs.cov[[name]][[site]], 2, function(x) any(is.na(x))) == TRUE)
    if (length(bad) > 0)
    {
      obs.cov[[name]][[site]] = obs.cov[[name]][[site]][,-bad]
      if (is.null(dim(obs.cov[[name]][[site]])))
      {
        obs.cov[[name]][[site]] = obs.cov[[name]][[site]][-bad]
      } else {
        obs.cov[[name]][[site]] = obs.cov[[name]][[site]][-bad,]
      }
    }
  }
}


save(obs.mean, file = '/data/bmorrison/sda/500_site_run/obs_mean_500_ave.Rdata')
save(obs.cov, file = '/data/bmorrison/sda/500_site_run/obs_cov_500_ave.Rdata')



