#----------------------------------------------------------------
# Loading required Libraries
#---------------------------------------------------------------
library(PEcAn.assim.sequential)
library(PEcAn.settings)
library(PEcAn.uncertainty)
library(lubridate)
library(purrr)
library(dplyr)
library(reshape2)
library(furrr)
library(stringr)
#----------------------------------------------------------------
# Reading settings and paths
#---------------------------------------------------------------
setwd("/projectnb/dietzelab/dongchen/Multi-site/")
source('Helpers.R')
args <- commandArgs(trailingOnly = TRUE)
#Settings
if (is.na(args[1])){
  settings <- read.settings("4site.test.xml")
} else {
  settings.file = args[1]
  settings <- PEcAn.settings::read.settings(settings.file)
}
#change path
# if(!file.exists(paste(getwd(),"/met",sep = ""))){
#   dir.create("met")
# } else {
#   unlink(paste(getwd(),"/met",sep = ""))
# }
settings <- MultiSettings(settings%>%
  `class<-`(c("list"))%>% #until here, it separates all the settings for all sites that listed in the xml file
  future_map(function(list){
    list$run$inputs$met$path <- paste("/fs/data1/pecan.data/dbfiles/",str_split(list$run$inputs$met$path,pattern = "/")[[1]][6],"/",#change the met data path
                                      str_split(list$run$inputs$met$path,pattern = "/")[[1]][7],sep="")
    list$outdir <- "/projectnb/dietzelab/dongchen/Multi-site/SDA"
    list$rundir <- "/projectnb/dietzelab/dongchen/Multi-site/SDA/run"
    list$modeloutdir <- "/projectnb/dietzelab/dongchen/Multi-site/SDA/out"
    list$host$rundir <- "/projectnb/dietzelab/dongchen/Multi-site/SDA/run"
    list$host$outdir <- "/projectnb/dietzelab/dongchen/Multi-site/SDA/out"
    
    #Copy the met files into my own directory
    #file.copy(list$run$inputs$met$path, paste(getwd(),"/met",sep = ""))
    
    list
  }))

#Copy the met files into my own directory


#Obs Path
if (is.na(args[2])){
  obs.path <- "/projectnb/dietzelab/hamzed/MultiSite-Exs/Obs/LandTrendr_AGB_output50s.RData"
} else {
  obs.path = args[2]
}

#pft path
if (is.na(args[3])){
  pft.path <- "/projectnb/dietzelab/hamzed/MultiSite-Exs/SDA/site_pft.csv"
} else {
  pft.path = args[3]
}
#----------------------------------------------------------------
# Setup
#---------------------------------------------------------------
if(file.exists(settings$outdir)){
  unlink(c('run','out','SDA'),recursive = T)
  dir.create("SDA")
  setwd(settings$outdir)
  dir.create("run")
  dir.create("out")
} else {
  dir.create("SDA")
  setwd(settings$outdir)
  dir.create("run")
  dir.create("out")
}

#----------------------------------------------------------------
# Find what sites we are running for
#---------------------------------------------------------------
if ("MultiSettings" %in% class(settings)) site.ids <- settings %>% map(~.x[['run']] ) %>% map('site') %>% map('id') %>% unlist() %>% as.character()
#----------------------------------------------------------------
# samples should be ready if not lets make it
#---------------------------------------------------------------
if (!("samples.Rdata" %in% list.files())) get.parameter.samples(settings,
                                                                ens.sample.method = settings$ensemble$samplingspace$parameters$method)  ## Aside: if method were set to unscented, would take minimal changes to do UnKF
#make pft.csv file
if (!("site_pft.csv" %in% list.files())) file.copy(pft.path,getwd())
#----------------------------------------------------------------
# OBS data preparation
#---------------------------------------------------------------
obss <- Obs.data.prepare.MultiSite(obs.path, site.ids)
#----------------------------------------------------------------
# Preparing settings
#---------------------------------------------------------------
new.settings <- PEcAn.settings::prepare.settings(settings)
#----------------------------------------------------------------
# SDA
#---------------------------------------------------------------
settings <- new.settings
obs.mean <- obss$obs.mean
obs.cov <- obss$obs.cov
Q = NULL 
restart = FALSE 
forceRun = TRUE
keepNC = TRUE
control=list(trace = TRUE,
             FF = FALSE,
             interactivePlot = FALSE,
             TimeseriesPlot = FALSE,
             BiasPlot = FALSE,
             plot.title = NULL,
             facet.plots = FALSE,
             debug = FALSE,
             pause = FALSE,
             Profiling = FALSE,
             OutlierDetection=FALSE)
sda.enkf.multisite(new.settings,
                   obs.mean =obss$obs.mean ,
                   obs.cov = obss$obs.cov)


