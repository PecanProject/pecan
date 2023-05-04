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

setwd("/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/SDA")
settings_dir <- "/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/IC/pecan.xml"
settings <- PEcAn.settings::read.settings(settings_dir)
#prepare samples
# PEcAn.uncertainty::get.parameter.samples(settings, ens.sample.method = settings$ensemble$samplingspace$parameters$method)
#prepare settings
settings <- PEcAn.settings::prepare.settings(settings)
All_obs_prep <- settings$state.data.assimilation$Obs_Prep
folder_prefix <- c("AGB", "LAI", "SMP", "SoilC")
comb <- combn(4, 3)
for (i in 1:4) {
  settings$state.data.assimilation$Obs_Prep <- All_obs_prep[c(comb[,i], 5:8)]
  folder_name <- paste0(folder_prefix[comb[,i]], collapse = '_')
  #prep obs
  obs <- PEcAnAssimSequential::SDA_OBS_Assembler(settings = settings)
  #prepare obs
  load("/projectnb/dietzelab/dongchen/All_NEON_SDA/test_OBS/Rdata/obs.mean.Rdata")
  load("/projectnb/dietzelab/dongchen/All_NEON_SDA/test_OBS/Rdata/obs.cov.Rdata")
  
  for (i in 1:length(obs.mean)) {
    if(is.null(obs.mean[[i]][[1]])){
      next
    }
    for (j in 1:length(obs.mean[[i]])) {
      obs.mean[[i]][[j]][which(obs.mean[[i]][[j]]==0)] <- 0.01
      if(length(obs.cov[[i]][[j]]) > 1){
        diag(obs.cov[[i]][[j]])[which(diag(obs.cov[[i]][[j]]==0))] <- 1
      }else{
        if(obs.cov[[i]][[j]] == 0){
          obs.cov[[i]][[j]] <- 1
        }
      }
    }
  }
  load("/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/SDA_ALL/enkf.Rdata")
  PEcAn.utils::sendmail("zhangdc@bu.edu", "zhangdc@bu.edu", "SDA progress report", paste("Started working on", folder_name))
  if(is.character(try(PEcAnAssimSequential::sda.enkf.multisite(settings = settings,
                                                               obs.mean = obs.mean,
                                                               obs.cov = obs.cov,
                                                               Q = NULL,
                                                               restart = FALSE,
                                                               forceRun = TRUE,
                                                               keepNC = TRUE,
                                                               pre_enkf_params = enkf.params,
                                                               control=list(trace = TRUE,
                                                                            FF = FALSE,
                                                                            interactivePlot = FALSE,
                                                                            TimeseriesPlot = F,
                                                                            BiasPlot = FALSE,
                                                                            plot.title = NULL,
                                                                            facet.plots = FALSE,
                                                                            debug = FALSE,
                                                                            pause = FALSE,
                                                                            Profiling = FALSE,
                                                                            OutlierDetection=FALSE)), silent = T))){
    PEcAn.utils::sendmail("zhangdc@bu.edu", "zhangdc@bu.edu", "SDA running error", "Error")
    break
  }else{
    PEcAn.utils::sendmail("zhangdc@bu.edu", "zhangdc@bu.edu", "SDA progress report", paste0(folder_name, " has been completed!"))
  }
  file.rename('/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/SDA', 
              file.path('/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42', 
                        paste0("SDA_", folder_name)))
  dir.create('/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/SDA')
  dir.create('/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/SDA/out')
  dir.create('/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/SDA/run')
  file.copy(file.path('/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42', 
                      paste0("SDA_", folder_name),
                      "samples.Rdata"),
            file.path('/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/SDA',
                      "samples.Rdata"))
}