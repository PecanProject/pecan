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
#----------------------------------------------------------------
# Reading settings and paths
#---------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
#Settings
if (is.na(args[1])){
  settings <- read.settings("pecan.SDA.4sites.xml")
} else {
  settings.file = args[1]
  settings <- PEcAn.settings::read.settings(settings.file)
}

#Obs Path
if (is.na(args[2])){
  obs.path <- "/projectnb/dietzelab/hamzed/MultiSite-Exs/Obs/LandTrendr_AGB_output50s.RData"
} else {
  obs.path = args[2]
}
#----------------------------------------------------------------
# Setup
#---------------------------------------------------------------
setwd(settings$outdir)
unlink(c('run','out','SDA'),recursive = T)
#----------------------------------------------------------------
# Find what sites we are running for
#---------------------------------------------------------------
if ("MultiSettings" %in% class(settings)) site.ids <- settings %>% map(~.x[['run']] ) %>% map('site') %>% map('id') %>% unlist() %>% as.character()
#----------------------------------------------------------------
# samples should be ready if not lets make it
#---------------------------------------------------------------
if (!("samples.Rdata" %in% list.files())) get.parameter.samples(settings,
                                                                ens.sample.method = settings$ensemble$samplingspace$parameters$method)  ## Aside: if method were set to unscented, would take minimal changes to do UnKF
#----------------------------------------------------------------
# OBS data preparation
#---------------------------------------------------------------
obss <- PEcAn.assim.sequential:::Obs.data.prepare.MultiSite(obs.path, site.ids)
#----------------------------------------------------------------
# Preparing settings
#---------------------------------------------------------------
new.settings <- PEcAn.settings::prepare.settings(settings)
#----------------------------------------------------------------
# SDA
#---------------------------------------------------------------
sda.enkf.multisite(new.settings,
                   obs.mean =obss$obs.mean ,
                   obs.cov = obss$obs.cov)


