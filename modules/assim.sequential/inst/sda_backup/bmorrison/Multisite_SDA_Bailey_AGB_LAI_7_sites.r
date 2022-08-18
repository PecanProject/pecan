
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
library(furrr)
library(tictoc)

#--------------------------------------------------------------------------------------------------#
######################################## INTIAL SET UP STUFF #######################################
work_dir <- "/data/bmorrison/sda/lai"
setwd(work_dir)
# delete an old run
#unlink(c('run','out','SDA'),recursive = T)

# grab multi-site XML file
settings <- read.settings("/data/bmorrison/sda/lai/pecan_MultiSite_SDA_LAI_AGB_8_Sites_2009.xml")


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


############################ EXTRACT SITE INFORMATION FROM XML TO DOWNLOAD DATA + RUN SDA ###########################
################ Not working on interactive job on MODEX

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


###################### EXTRACT AGB DATA + REFORMAT LONG VS. WIDE STYLE #####################################
### this is for LandTrendr data ###

# output folder for the data
# data_dir <- "/data2/RS_GIS_Data/LandTrendr/LandTrendr_AGB_data"
# 
# # extract the data
# med_agb_data <- extract.LandTrendr.AGB(site_info, "median", buffer = NULL, fun = "mean",
#                                        data_dir, product_dates=NULL, file.path(work_dir,"Obs"))[[1]]
# 
# sdev_agb_data <- extract.LandTrendr.AGB(site_info, "stdv", buffer = NULL, fun = "mean",
#                                         data_dir, product_dates=NULL, file.path(work_dir,"Obs"))[[1]]
# 
# 
# ###  temporary fix to make agb data long vs. wide format to match modis data. ###
ndates = colnames(med_agb_data)[-c(1:2)]

med_agb_data$Site_Name = as.character(med_agb_data$Site_Name, stringsAsFactors = FALSE)
med_agb_data = reshape2::melt(med_agb_data, id.vars = "Site_ID", measure.vars = colnames(med_agb_data)[-c(1:2)])

sdev_agb_data$Site_Name = as.character(sdev_agb_data$Site_Name, stringsAsFactors = FALSE)
sdev_agb_data = reshape2::melt(sdev_agb_data, id.vars = "Site_ID", measure.vars = colnames(sdev_agb_data)[-c(1:2)])

agb_data = as.data.frame(cbind(med_agb_data, sdev_agb_data$value))
names(agb_data) = c("Site_ID", "Date", "Median", "SD")
agb_data$Date = as.character(agb_data$Date, stringsAsFactors = FALSE)

# save AGB data into long style
save(agb_data, file = '/data/bmorrison/sda/lai/modis_lai_data/agb_data_update_sites.Rdata')
# 
# 
# # ####################### Extract MODISTools LAI data ##############################
# 
# library(doParallel)
# cl <- parallel::makeCluster(10, outfile="")
# doParallel::registerDoParallel(cl)
# 
# start = Sys.time()
# # keep QC_filter on for this because bad LAI values crash the SDA. Progress can be turned off if it annoys you. 
# data = foreach(i=1:length(site_info$site_id), .combine = rbind) %dopar% PEcAn.data.remote::call_MODIS(start_date = "2000/01/01", end_date = "2017/12/31", band = "Lai_500m", product = "MOD15A2H", lat = site_info$lat[i], lon = site_info$lon[i], size = 0, band_qc = "FparLai_QC", band_sd = "LaiStdDev_500m", package_method = "MODISTools", QC_filter = T, progress = T)
# end = Sys.time()
# difference = end-start
# stopCluster(cl)
# 
# # already in long format style for dataframe
# output = as.data.frame(data)
# save(output, file = '/data/bmorrison/sda/lai/modis_lai_data/modis_lai_output_update_sites.Rdata')
# 
# # change tile names to the site name
# for (i in 1:length(site_info$site_name))
# {
#   name = as.character(site_info$site_id[i], stringsAsFactor = F)
#   g = which(round(output$lat, digits = 3) == round(site_info$lat[i], digits = 3))
#   output$tile[g] = name
# }
# # remove extra data
# output = output[,c(4,2,8,10)]
# colnames(output) = names(agb_data)
# 
# # compute peak lai per year
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
# save(peak_lai, file = '/data/bmorrison/sda/lai/modis_lai_data/peak_lai_output_update_sites.Rdata')
# 
# 
# ######################### TIME TO FIX UP THE OBSERVED DATASETS INTO A FORMAT THAT WORKS TO MAKE OBS.MEAN and OBS.COV FOR SDA ########################
# #################
load('/data/bmorrison/sda/lai/modis_lai_data/agb_data_update_sites.Rdata')
load( '/data/bmorrison/sda/lai/modis_lai_data/peak_lai_output_update_sites.Rdata')
# output likes to make factors ..... :/... so this unfactors them
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
      map(~.x[3:4] %>% setNames(c("AbvGrndWood", "LAI")) %>% `row.names<-`(NULL)) %>%
      setNames(site.ids)
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
      row.names(obs.mean[[name]][[site]]) = NULL
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
      map(~.x[3:4]^2 %>% unlist %>% diag(nrow = 2, ncol = 2) ) %>%
      setNames(site.ids)
  }) %>% setNames(date.obs)


names = date.obs
for (name in names)
{
  for (site in names(obs.cov[[name]]))
  {
    na_index = which(is.na(obs.cov[[ name]][[site]]))
    #colnames = names(obs.cov[[name]][[site]])
    if (length(na_index) > 0)
    {
      obs.cov[[name]][[site]] = obs.cov[[name]][[site]][1]
      # row.names(obs.cov[[name]][[site]]) = NULL
      # colnames(obs.cov[[name]][[site]]) = NULL
    }
  }
}
# #sublist by date --> site
# obs.cov = names(obs.cov) %>%
#   map(function(namesl){
#     obs.cov[[namesl]] %>%
#       split(.$site_id) %>%
#       map(~diag(.x[3:4]^2, nrow = 2, ncol = 2)) %>%
#       setNames(site.ids)}) %>%
#   setNames(date.obs)

# remove NA/missing observations from covariance matrix and removes NA values to restructure size of covar matrix
# names = names(obs.cov)
# for (name in names)
# {
#   for (site in names(obs.cov[[name]]))
#   {
#     na_index = which(is.na(obs.cov[[ name]][[site]]))
#     if (length(na_index) > 0)
#     {
#       n_good_vars = length(observed_vars)-length(na_index)
#       obs.cov[[name]][[site]] = matrix(obs.cov[[name]][[site]][-na_index], nrow = n_good_vars, ncol = n_good_vars)
#     }
#   }
# }

# save these lists for future use.
save(obs.mean, file = '/data/bmorrison/sda/lai/obs_mean_8_sites_dif_dates.Rdata')
save(obs.cov, file = '/data/bmorrison/sda/lai/obs_cov_8_sites_dif_dates.Rdata')
save(date.obs, file = '/data/bmorrison/sda/lai/date_obs_8_sites_dif_dates.Rdata')



################################ START THE SDA ########################################
load('/data/bmorrison/sda/lai/obs_mean_8_sites_dif_dates.Rdata')
load('/data/bmorrison/sda/lai/obs_cov_8_sites_dif_dates.Rdata')
date.obs = names(obs.mean)

outfolder = "/data/bmorrison/sda/lai/easy_run_8_sites"
unlink(c('run','out', outfolder),recursive = T)

new.settings <- PEcAn.settings::prepare.settings(settings)

settings = new.settings
Q = NULL
restart = F
keepNC = T
forceRun = T
daily = F
#unlink(c('run','out','SDA'),recursive = T)

sda.enkf.multisite(outfolder = outfolder,
                   settings = new.settings, 
                   obs.mean = obs.mean,
                   obs.cov = obs.cov,
                   keepNC = TRUE, 
                   forceRun = TRUE,
                   daily = F,
                   control=list(trace=TRUE, 
                                FF=FALSE,
                                interactivePlot=FALSE, 
                                TimeseriesPlot=FALSE,
                                BiasPlot=FALSE,
                                plot.title=NULL,
                                facet.plots=4,
                                debug=FALSE,
                                pause=FALSE,
                                Profiling = FALSE,
                                OutlierDetection=FALSE))




### FOR PLOTTING after analysis if TimeseriesPlot == FALSE)
load('/data/bmorrison/sda/lai/8_sites_different_date/sda.output.Rdata')
facetg=4
readsFF=NULL
plot.title=NULL

obs.mean = Viz.output[[2]]
obs.cov = Viz.output[[3]]
obs.times = names(obs.mean)
PEcAn.assim.sequential::post.analysis.multisite.ggplot(settings = new.settings, t, obs.times, obs.mean, obs.cov, FORECAST, ANALYSIS, plot.title=NULL, facetg=4, readsFF=NULL)

