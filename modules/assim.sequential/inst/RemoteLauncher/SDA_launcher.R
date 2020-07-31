#----------------------------------------------------------------
# Loading required Libraries
#---------------------------------------------------------------
library(PEcAn.assim.sequential)
library(PEcAn.settings)
library(PEcAn.uncertainty)
library(lubridate)
library(purrr)
library(dplyr)
library(furrr)
library(nimble)
library(reshape2)
library(tictoc)

plan(multiprocess)
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
  PEcAn.logger::logger.severe("This file needs to be called from terminal and needs to recived to argument with it. First, path to the setting xml file and second is the path to the obs data. Seems like the second argument is missing.")
} else {
  obs.path = args[2]
}
#----------------------------------------------------------------
# Setup
#---------------------------------------------------------------
setwd(settings$outdir)
# This is how I delete large folders
# In case there is an SDA run already performed in this dir and you're planning to use the same dir for some reason
# These next lines could be uncommented to delete the necessary dirs.
# c('run', 'out', 'SDA') %>%
#   map(function(dir.delete) {
#     if (dir.exists(file.path(settings$outdir, dir.delete))) {
#       setwd(settings$outdir)
#       list.dirs(dir.delete, full.names = T) %>%
#         furrr::future_map(function(del.dir) {
#           setwd(file.path(settings$outdir, del.dir))
#           system(paste0("perl -e 'for(<*>){((stat)[9]<(unlink))}'"))
#         })
#       PEcAn.logger::logger.info(paste0("I just deleted ", dir.delete, " folder !"))
#     }
#   })
# 
# unlink(c('run', 'out', 'SDA'), recursive = TRUE)
#----------------------------------------------------------------
# Find what sites we are running for
#---------------------------------------------------------------
if (inherits(settings, "MultiSettings")) site.ids <- settings %>% map(~.x[['run']] ) %>% map('site') %>% map('id') %>% unlist() %>% as.character()
#----------------------------------------------------------------
# samples should be ready if not lets make it
#---------------------------------------------------------------
if (!("samples.Rdata" %in% list.files())) {
  #check to see if there are posterior.files tags under pft
  
  posterior.files.vec<-settings$pfts %>%
    purrr::map(purrr::possibly('posterior.files', NA_character_)) %>%
    purrr::modify_depth(1, function(x) {
      ifelse(is.null(x), NA_character_, x)
    }) %>%
    unlist()
  
  get.parameter.samples(settings,
                        ens.sample.method = settings$ensemble$samplingspace$parameters$method,
                        posterior.files=posterior.files.vec)  ## Aside: if method were set to unscented, would take minimal changes to do UnKF
}
#----------------------------------------------------------------
# OBS data preparation
#---------------------------------------------------------------
tryCatch(
  {
    if (is.MultiSettings(settings)){
      obss <- PEcAn.assim.sequential:::Obs.data.prepare.MultiSite(obs.path, site.ids)
    } else {
      obss <- load(obs.path)
    }
  },
  error = function(e) {
    PEcAn.logger::logger.warn("Something happend during the proccsing of obs data. This could be either due to structure of your data or lack of access to the file.")
    PEcAn.logger::logger.severe(conditionMessage(e))
  }
)


#----------------------------------------------------------------
# SDA
#---------------------------------------------------------------
if (is.MultiSettings(settings)){
  #----------------------------------------------------------------
  # Preparing settings
  #---------------------------------------------------------------
  new.settings <- PEcAn.settings::prepare.settings(settings)
  #MultiSite SDA function
  sda.enkf.multisite(new.settings,
                     obs.mean =obss$obs.mean ,
                     obs.cov = obss$obs.cov)
} else {
  #Refactored SDA function
  sda.enkf(settings,
           obs.mean =obss$obs.mean ,
           obs.cov = obss$obs.cov
           )
}



