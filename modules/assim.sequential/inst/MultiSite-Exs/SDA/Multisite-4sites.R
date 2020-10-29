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
#------------------------------------------ Setup -------------------------------------
setwd("/fs/data3/hamzed/MultiSite_Project/SDA")
unlink(c('run','out','SDA'),recursive = T)
rm(list=ls())
settings <- read.settings("pecan.SDA.4sites.xml")
if (inherits(settings, "MultiSettings")) site.ids <- settings %>% map(~.x[['run']] ) %>% map('site') %>% map('id') %>% unlist() %>% as.character()
#sample from parameters used for both sensitivity analysis and Ens
get.parameter.samples(settings, ens.sample.method = settings$ensemble$samplingspace$parameters$method)  ## Aside: if method were set to unscented, would take minimal changes to do UnKF
#----------------------------------------------------------------
# OBS data preparation
#---------------------------------------------------------------
load("../Obs/LandTrendr_AGB_output-4sites.RData")
site1<-point_list
load("../Obs/LandTrendr_AGB_output_796-769.RData")
site2<-point_list
site2$median_AGB[[1]] %>% 
  filter(Site_ID!='1000000074') ->site2$median_AGB[[1]]


site2$stdv_AGB[[1]] %>% 
  filter(Site_ID!='1000000074') ->site2$stdv_AGB[[1]]
#listviewer::jsonedit(point_list)
#--------------------------------------------------------------------------------
#for multi site both mean and cov needs to be a list like this 
# +date
#   +siteid
#     c(state variables)/matrix(cov state variables)
# 
#reorder sites in obs
point_list$median_AGB <-rbind(site1$median_AGB[[1]],
                              site2$median_AGB[[1]]) %>% filter(Site_ID %in% site.ids)
point_list$stdv_AGB <-rbind(site1$stdv_AGB[[1]],
                            site2$stdv_AGB[[1]])%>% filter(Site_ID %in% site.ids)

site.order <- sapply(site.ids,function(x) which(point_list$median_AGB$Site_ID %in% x)) %>%
  as.numeric() %>% na.omit()

point_list$median_AGB <- point_list$median_AGB[site.order,]
point_list$stdv_AGB <- point_list$stdv_AGB[site.order,]

# truning lists to dfs  for both mean and cov
date.obs <- strsplit(names(site1$median_AGB[[1]]),"_")[3:length(site1$median_AGB[[1]])] %>%
  map_chr(~.x[2]) %>% paste0(.,"/12/31") 

obs.mean <-names(point_list$median_AGB)[3:length(point_list$median_AGB)] %>%
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

#----------------------------------------------------------------
# end OBS data preparation
#---------------------------------------------------------------
new.settings <- PEcAn.settings::prepare.settings(settings)
jsonedit(new.settings)
#------------------------------------------ SDA -------------------------------------
sda.enkf.multisite(new.settings, obs.mean =obs.mean ,obs.cov = obs.cov, 
                   control=list(trace=T, 
                                FF=F,
                                interactivePlot=F, 
                                TimeseriesPlot=T,
                                BiasPlot=F,
                                plot.title="lhc sampling - 4sites - SF50 - ALL PFTs - small sample size",
                                facet.plots=T,
                                debug=T,
                                pause=F)
        )


