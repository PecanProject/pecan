
#####
##### Workflow code by Ann Raiho (ann.raiho@gmail.com)
##### This is the workflow template for doing a variance partitioning run
##### It probably will take you two days to rerun. The longest runs are the full SDA and process variance runs.
##### Basically I'm altering the pecan.SDA.xml to run the runs with data constrained initial conditions
##### For the spin up runs I'm altering the pecan.CONFIGS.xml to just use start_model_runs()
#####


library(PEcAn.all)
library(PEcAn.SIPNET)
library(PEcAn.LINKAGES)
library(PEcAn.visualization)
library(PEcAn.ED2)
library(PEcAn.assim.sequential)
library(nimble)
library(lubridate)
library(PEcAn.visualization)
#PEcAn.assim.sequential::
library(rgdal) # need to put in assim.sequential
library(ncdf4) # need to put in assim.sequential


#####
##### SDA FULL RUN
#####

settings <- read.settings("pecan.SDA.xml")

load('sda.obs.Rdata')

obs.mean <- obs.list$obs.mean
obs.cov <- obs.list$obs.cov

sda.enkf(settings, obs.mean, obs.cov, Q = NULL, restart=F, 
         control=list(trace=T,
                      interactivePlot=T,
                      TimeseriesPlot=T,
                      BiasPlot=T,
                      plot.title=NULL,
                      debug=F,
                      pause = F))

####
#### DEFAULT
####

nens <- settings$ensemble

#changed input to be only one met ensemble member
#basically the same as pecan.CONFIGS.xml
settings <- read.settings('pecan.DEFAULT.xml')
settings <- PEcAn.workflow::runModule.run.write.configs(settings)

# Taking average of samples to have fixed params across nens
load('samples.Rdata')
ensemble.samples.means <- ensemble.samples
for(i in 1:length(ensemble.samples.means)) ensemble.samples.means[[i]] <- matrix(colMeans(ensemble.samples[[i]]),nens,ncol(ensemble.samples[[i]]),byrow = T)
ensemble.samples <- ensemble.samples.means
save(ensemble.samples,file='average_samples.Rdata')
save(ensemble.samples,file='samples.Rdata')

outconfig <- write.ensemble.configs(defaults = settings$pfts, 
                                    ensemble.samples = ensemble.samples, 
                                    settings = settings,
                                    model = settings$model$type, 
                                    write.to.db = settings$database$bety$write,
                                    restart = NULL)
PEcAn.workflow::runModule_start_model_runs(settings, stop.on.error = FALSE)

file.rename('out','out_default')
file.rename('run','run_default')

####
#### DEFAULT -- DATA IC ####
####

#similar to pecan.SDA.xml but with not sampling params or met or doing process. Using SDA to constrain time step 1. 
settings <- read.settings('pecan.DEFAULT.DATAIC.xml')
load('sda.obs.Rdata')

#Becasue we only want to inform the initial conditions for this model experiment we only use the first data point. 
#The last data point is included so that the model runs until this point.
obs.cov <- obs.mean <- list()
for(i in c(1,length(obs.list$obs.mean))){
  obs.mean[[i]] <- obs.list$obs.mean[[i]]
  obs.cov[[i]] <- obs.list$obs.cov[[i]]
}

#write dates as names for data objects
names(obs.cov) <- names(obs.mean) <- names(obs.list$obs.cov)

obs.mean[2:(length(obs.list$obs.mean)-1)] <- NULL
obs.cov[2:(length(obs.list$obs.mean)-1)] <- NULL

obs.mean[[length(obs.mean)]] <- rep(NA,length(ensemble.samples.means))

sda.enkf(settings, obs.mean, obs.cov, Q = NULL, restart=F, 
         control=list(trace=T,
                      interactivePlot=T,
                      TimeseriesPlot=T,
                      BiasPlot=T,
                      plot.title=NULL,
                      debug=F,
                      pause = F))

file.rename('out','out_default_ic')
file.rename('run','run_default_ic')
file.rename('SDA','SDA_default_ic')

####
#### PARAM ####
####

#running with sampled params
settings <- read.settings('pecan.DEFAULT.xml')
settings <- PEcAn.workflow::runModule.run.write.configs(settings)
PEcAn.workflow::runModule_start_model_runs(settings, stop.on.error = FALSE)

file.rename('out','out_param')
file.rename('run','run_param')

####
#### PARAM DATA IC ####
####

settings <- read.settings('pecan.DEFAULT.DATAIC.xml')
load('sda.obs.Rdata')#load('sda.data_AGB.Rdata')

#Becasue we only want to inform the initial conditions for this model experiment we only use the first data point. 
#The last data point is included so that the model runs until this point.
obs.cov <- obs.mean <- list()
for(i in c(1,length(obs.list$obs.mean))){
  obs.mean[[i]] <- obs.list$obs.mean[[i]]
  obs.cov[[i]] <- obs.list$obs.cov[[i]]
}

#write dates as names for data objects
names(obs.cov) <- names(obs.mean) <- names(obs.list$obs.cov)

obs.mean[2:(length(obs.list$obs.mean)-1)] <- NULL
obs.cov[2:(length(obs.list$obs.mean)-1)] <- NULL

obs.mean[[length(obs.mean)]] <- rep(NA,length(ensemble.samples.means))

sda.enkf(settings, obs.mean, obs.cov, Q = NULL, restart=F, 
         control=list(trace=T,
                      interactivePlot=T,
                      TimeseriesPlot=T,
                      BiasPlot=T,
                      plot.title=NULL,
                      debug=F,
                      pause = F))

file.rename('out','out_param_ic')
file.rename('run','run_param_ic')

####
#### MET ####
####

#running with sampled params
settings <- read.settings('pecan.SAMP.MET.xml')
settings <- PEcAn.workflow::runModule.run.write.configs(settings)
PEcAn.workflow::runModule_start_model_runs(settings, stop.on.error = FALSE)

file.rename('out','out_met')
file.rename('run','run_met')

####
#### MET DATA IC ####
####

file.rename('ensemble_weights_SDA.Rdata','ensemble_weights.Rdata')

settings <- read.settings('pecan.SAMP.MET.DATA.IC.xml')
load('sda.obs.Rdata')#load('sda.data_AGB.Rdata')

#Becasue we only want to inform the initial conditions for this model experiment we only use the first data point. 
#The last data point is included so that the model runs until this point.
obs.cov <- obs.mean <- list()
for(i in c(1,length(obs.list$obs.mean))){
  obs.mean[[i]] <- obs.list$obs.mean[[i]]
  obs.cov[[i]] <- obs.list$obs.cov[[i]]
}

#write dates as names for data objects
names(obs.cov) <- names(obs.mean) <- names(obs.list$obs.cov)

obs.mean[2:(length(obs.list$obs.mean)-1)] <- NULL
obs.cov[2:(length(obs.list$obs.mean)-1)] <- NULL

obs.mean[[length(obs.mean)]] <- rep(NA,length(ensemble.samples.means))

sda.enkf(settings, obs.mean, obs.cov, Q = NULL, restart=F, 
         control=list(trace=T,
                      interactivePlot=T,
                      TimeseriesPlot=T,
                      BiasPlot=T,
                      plot.title=NULL,
                      debug=F,
                      pause = F))

file.rename('out','out_met_ic')
file.rename('run','run_met_ic')

####
#### PROCESS ####
####

settings <- read.settings('pecan.PROCESS.xml')

#running with sampled params
load('sda.obs.Rdata')#load('sda.data_AGB.Rdata')

obs.mean <- obs.list$obs.mean
obs.cov <- obs.list$obs.cov

#write dates as names for data objects
names(obs.cov) <- names(obs.mean) <- names(obs.list$obs.cov)

for(i in 1:length(obs.list$obs.mean)) obs.mean[[i]] <- rep(NA,length(ensemble.samples.means))

load('SDA_SDA/sda.output.Rdata')

Q <- solve(enkf.params[[t-1]]$q.bar)

rm(new.state)

sda.enkf(settings, obs.mean, obs.cov, Q = Q, restart=F, 
         control=list(trace=T,
                      interactivePlot=T,
                      TimeseriesPlot=T,
                      BiasPlot=T,
                      plot.title=NULL,
                      debug=F,
                      pause = F))

file.rename('out','out_process')
file.rename('run','run_process')
file.rename('SDA','SDA_process')

####
#### PROCESS DATA IC ####
####

settings <- read.settings('pecan.PROCESS.xml')

#running with sampled params
load('sda.obs.Rdata')

obs.mean <- obs.list$obs.mean
obs.cov <- obs.list$obs.cov

#write dates as names for data objects
names(obs.cov) <- names(obs.mean) <- names(obs.list$obs.cov)

for(i in 2:length(obs.list$obs.mean)) obs.mean[[i]] <- rep(NA,length(ensemble.samples.means))

load('SDA_SDA/sda.output.Rdata')

Q <- solve(enkf.params[[t-1]]$q.bar)

rm(new.state)

sda.enkf(settings, obs.mean, obs.cov, Q = Q, restart=T, 
         control=list(trace=T,
                      interactivePlot=T,
                      TimeseriesPlot=T,
                      BiasPlot=T,
                      plot.title=NULL,
                      debug=F,
                      pause = F))

file.rename('out','out_process_ic')
file.rename('run','run_process_ic')
file.rename('SDA','SDA_process_ic')
