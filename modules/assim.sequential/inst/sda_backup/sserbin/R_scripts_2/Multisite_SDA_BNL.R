####################################################################################################
#
#
#
#
#    --- Last updated:  03.26.2019 By Shawn P. Serbin <sserbin@bnl.gov>
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
work_dir <- "/data/sserbin/Modeling/sipnet/NASA_CMS"
setwd(work_dir)  # best not to require setting wd and instead just providing full paths in functions

# Deifine observation - use existing or generate new? 
# set to a specific file, use that.
#observation <- ""
observation <- c("1000025731","1000000048","763","796","772","764","765","1000000024","678",
                 "1000000146")

# delete an old run
unlink(c('run','out','SDA'),recursive = T)

# grab multi-site XML file
settings <- read.settings("XMLs/pecan_MultiSite_SDA.xml")


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

# option 2: run extraction code to generate observation files
PEcAn.logger::logger.info("**** Extracting LandTrendr AGB data for model sites ****")
bety <- list(user='bety', password='bety', host='localhost',
             dbname='bety', driver='PostgreSQL',write=TRUE)
con <- PEcAn.DB::db.open(bety)
bety$con <- con
site_ID <- observation # BETYdb site IDs
data_dir <- "/data2/RS_GIS_Data/LandTrendr/LandTrendr_AGB_data"
#results <- PEcAn.data.remote::extract.LandTrendr.AGB(coords=site_ID, 
results <- extract.LandTrendr.AGB(coords=site_ID, 
                                  data_dir = data_dir, con = con,
                                  output_file = file.path(work_dir,"Obs"),
                                  plot_results = FALSE)
load("Obs/LandTrendr_AGB_output.RData")


#for multi site both mean and cov needs to be a list like this 
# +date
#   +siteid
#     c(state variables)/matrix(cov state variables)
# 
#reorder sites in obs
point_list$median_AGB <- point_list$median_AGB[[1]] %>% filter(Site_ID %in% site.ids)
point_list$stdv_AGB <- point_list$stdv_AGB[[1]] %>% filter(Site_ID %in% site.ids)
site.order <- sapply(site.ids,function(x) which(point_list$median_AGB$Site_ID %in% x)) %>%
  as.numeric() %>% na.omit()
point_list$median_AGB <- point_list$median_AGB[site.order,]
point_list$stdv_AGB <- point_list$stdv_AGB[site.order,]

# truning lists to dfs  for both mean and cov
date.obs <- strsplit(names(point_list$median_AGB),"_")[3:length(point_list$median_AGB)] %>%
  map_chr(~.x[2]) %>% paste0(.,"/12/31") 

obs.mean <- names(point_list$median_AGB)[3:length(point_list$median_AGB)] %>%
  map(function(namesl){
    ((point_list$median_AGB)[[namesl]] %>% 
       map(~.x %>% as.data.frame %>% `colnames<-`(c('AbvGrndWood'))) %>% 
       setNames(site.ids[1:length(.)])
    )
  }) %>% setNames(date.obs)

obs.cov <-names(point_list$stdv_AGB)[3:length(point_list$median_AGB)] %>%
  map(function(namesl) {
    ((point_list$stdv_AGB)[[namesl]] %>%
       map( ~ (.x) ^ 2%>% as.matrix()) %>%
       setNames(site.ids[1:length(.)]))
    
  }) %>% setNames(date.obs)

#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## generate new settings object
new.settings <- PEcAn.settings::prepare.settings(settings, force = FALSE)
# Write pecan.CHECKED.xml
PEcAn.settings::write.settings(new.settings, outputfile = "pecan.CHECKED.xml")
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## Run SDA
sda.enkf.multisite(new.settings, obs.mean =obs.mean ,obs.cov = obs.cov, 
                   control=list(trace=T, 
                                FF=F,
                                interactivePlot=F, 
                                TimeseriesPlot=T,
                                BiasPlot=F,
                                plot.title="Uniform sampling - 10 sites",
                                facet.plots=T,
                                debug=F,
                                pause=F)
)
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
