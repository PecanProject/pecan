# loading libraries.
library(dplyr)
library(xts)
library(PEcAn.all)
library(purrr)
library(furrr)
library(lubridate)
library(nimble)
library(ncdf4)
library(PEcAnAssimSequential)
library(dplyr)
library(sp)
library(raster)
library(zoo)
library(ggplot2)
library(mnormt)
library(sjmisc)
library(stringr)
library(doParallel)
library(doSNOW)
library(Kendall)
library(lgarch)
library(parallel)
library(foreach)
library(terra)
setwd("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/")

# read settings xml file.
settings_dir <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/pecan.xml"
settings <- PEcAn.settings::read.settings(settings_dir)

# setup the batch job settings.
general.job <- list(cores = 28, folder.num = 80)
batch.settings = structure(list(
  general.job = general.job,
  qsub.cmd = "qsub -l h_rt=24:00:00 -l mem_per_core=4G -l buyin -pe omp @CORES@ -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
))
settings$state.data.assimilation$batch.settings <- batch.settings

# alter the ensemble size.
settings$ensemble$size <- 10

# update settings with the actual PFTs.
settings <- PEcAn.settings::prepare.settings(settings)

# load observations.
load("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/observation/Rdata/obs_mean.Rdata")
load("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/observation/Rdata/obs_cov.Rdata")

# replace zero observations and variances with small numbers.
for (i in 1:length(obs.mean)) {
  if(is.null(obs.mean[[i]][[1]])){
    next
  }
  for (j in 1:length(obs.mean[[i]])) {
    if (length(obs.mean[[i]][[j]])==0) {
      next
    }
    obs.mean[[i]][[j]][which(obs.mean[[i]][[j]]==0)] <- 0.01
    if(length(obs.cov[[i]][[j]]) > 1){
      diag(obs.cov[[i]][[j]])[which(diag(obs.cov[[i]][[j]]<=0.1))] <- 0.1
    }else{
      if(obs.cov[[i]][[j]] <= 0.1){
        obs.cov[[i]][[j]] <- 0.1
      }
    }
  }
}

# load PFT parameter file.
load(file.path(settings$outdir, "samples.Rdata"))

# execute the SDA.
PEcAnAssimSequential::qsub_sda(settings = settings, 
                               obs.mean = obs.mean, 
                               obs.cov = obs.cov, 
                               Q = NULL, 
                               pre_enkf_params = NULL, 
                               ensemble.samples = ensemble.samples, 
                               outdir = NULL, 
                               control = list(TimeseriesPlot = FALSE,
                                              OutlierDetection=FALSE,
                                              send_email = NULL,
                                              keepNC = FALSE,
                                              forceRun = TRUE,
                                              MCMC.args = NULL))