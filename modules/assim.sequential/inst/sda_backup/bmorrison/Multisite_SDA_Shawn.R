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


# temporary step until we get this code integrated into pecan
library(RCurl)
script <- getURL("https://raw.githubusercontent.com/serbinsh/pecan/download_osu_agb/modules/data.remote/R/LandTrendr.AGB.R", 
                 ssl.verifypeer = FALSE)
eval(parse(text = script))
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## set run options, some of these should be tweaked or removed as requirements
work_dir <- "/data/bmorrison/sda/lai"
setwd(work_dir)  # best not to require setting wd and instead just providing full paths in functions

# Deifine observation - use existing or generate new? 
# set to a specific file, use that.
#observation <- ""
#observation <- c("1000025731","1000000048","796", "772", "763", "1000000146")
#observation <- c("1000025731","1000000048","763","796","772","764","765","1000000024","678","1000000146")

# delete an old run
unlink(c('run','out','SDA'),recursive = T)

# grab multi-site XML file
settings <- read.settings("pecan_MultiSite_SDA_LAI_AGB.xml")

observation <- c()
for (i in seq_along(1:length(settings$run))) {
  command <- paste0("settings$run$settings.",i,"$site$id")
  obs <- eval(parse(text=command))
  observation <- c(observation,obs)
}

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

PEcAn.logger::logger.info("**** Preparing data for SDA ****")
#for multi site both mean and cov needs to be a list like this 
# +date
#   +siteid
#     c(state variables)/matrix(cov state variables)
# 
#reorder sites in obs
med_agb_data_sda <- med_agb_data[[1]] %>% filter(Site_ID %in% site.ids)
sdev_agb_data_sda <- sdev_agb_data[[1]] %>% filter(Site_ID %in% site.ids)
site.order <- sapply(site.ids,function(x) which(med_agb_data_sda$Site_ID %in% x)) %>%
  as.numeric() %>% na.omit()
med_agb_data_sda <- med_agb_data_sda[site.order,]
sdev_agb_data_sda <- sdev_agb_data_sda[site.order,]

# truning lists to dfs  for both mean and cov
date.obs <- strsplit(names(med_agb_data_sda),"_")[3:length(med_agb_data_sda)] %>%
  map_chr(~.x[2]) %>% paste0(.,"/12/31") 

obs.mean <- names(med_agb_data_sda)[3:length(med_agb_data_sda)] %>%
  map(function(namesl){
    ((med_agb_data_sda)[[namesl]] %>% 
       map(~.x %>% as.data.frame %>% `colnames<-`(c('AbvGrndWood'))) %>% 
       setNames(site.ids[1:length(.)]))
  }) %>% setNames(date.obs)

obs.cov <-names(sdev_agb_data_sda)[3:length(sdev_agb_data_sda)] %>%
  map(function(namesl) {
    ((sdev_agb_data_sda)[[namesl]] %>%
       map( ~ (.x) ^ 2%>% as.matrix()) %>%
       setNames(site.ids[1:length(.)]))
  }) %>% setNames(date.obs)

#--------------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------------#
## generate new settings object
new.settings <- PEcAn.settings::prepare.settings(settings)
#new.settings = settings
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## Run SDA
sda.enkf.multisite(new.settings, obs.mean =obs.mean ,obs.cov = obs.cov, 
                   control=list(trace=T, 
                                FF=F,
                                plot = T,
                                interactivePlot=F, 
                                TimeseriesPlot=T,
                                BiasPlot=F,
                                plot.title="Sobol sampling - 2 sites - AGB",
                                facet.plots=T,
                                debug=F,
                                pause=F)
)
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
