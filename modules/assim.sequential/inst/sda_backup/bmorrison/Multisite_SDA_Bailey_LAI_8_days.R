
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
library(furrr)
library(tictoc)

#--------------------------------------------------------------------------------------------------#
######################################## INTIAL SET UP STUFF #######################################
work_dir <- "/data/bmorrison/sda/lai"

# delete an old run
#unlink(c('run','out','SDA'),recursive = T)

# grab multi-site XML file
settings <- read.settings("pecan_MultiSite_SDA_LAI_4_sites_8_days.xml")


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



################################ START THE SDA ########################################
load('/data/bmorrison/sda/lai/obs_mean_4_sites_8_days.Rdata')
load('/data/bmorrison/sda/lai/obs_cov_4_sites_8_days.Rdata')
date.obs = names(obs.mean)


outfolder = "/data/bmorrison/sda/lai/4_sites_8_days"
unlink(c('run','out', outfolder),recursive = T)

new.settings <- PEcAn.settings::prepare.settings(settings)

settings = new.settings
Q = NULL
restart = F
keepNC = T
forceRun = T
daily = TRUE
#unlink(c('run','out','SDA'),recursive = T)

sda.enkf.multisite(outfolder = outfolder,
                   settings = new.settings, 
                   obs.mean = obs.mean,
                   obs.cov = obs.cov,
                   keepNC = TRUE, 
                   forceRun = TRUE,
                   daily = TRUE,
                   control=list(trace=TRUE, 
                                FF=FALSE,
                                interactivePlot=FALSE, 
                                TimeseriesPlot=FALSE,
                                BiasPlot=FALSE,
                                plot.title=NULL,
                                facet.plots=2,
                                debug=FALSE,
                                pause=FALSE,
                                Profiling = FALSE,
                                OutlierDetection=FALSE))




### FOR PLOTTING after analysis if TimeseriesPlot == FALSE)
load('/data/bmorrison/sda/lai/4_sites_8_days/sda.output.Rdata')
facetg=2
readsFF=NULL
settings= new.settings
settings$outfolder = outfolder
obs.mean = Viz.output[[2]]
obs.cov = Viz.output[[3]]
obs.times = names(obs.mean)
PEcAnAssimSequential::post.analysis.multisite.ggplot(settings = settings, t, obs.times, obs.mean, obs.cov, FORECAST, ANALYSIS, plot.title=NULL, facetg=2, readsFF=NULL)

