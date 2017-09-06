
library(PEcAn.all)
library(PEcAn.SIPNET)
library(PEcAn.LINKAGES)
library(PEcAn.assim.sequential)
library(nimble)
library(lubridate)

#LINKAGES #AGB.pft #Harvard Forest
#setwd('/fs/data2/output//PEcAn_1000003314/')
#setwd('/fs/data2/output//PEcAn_1000007999/') #full run 50 nens
setwd('/fs/data2/output//PEcAn_1000008008/')
file.copy('/fs/data2/output//PEcAn_1000007999/sda.obs.Rdata',getwd())
#TO DO: Having problem with running proc.var == TRUE because nimble isn't keeping the toggle sampler in the function environment.


#SIPNET
#setwd('/fs/data2/output//PEcAn_1000003356')
#setwd('/fs/data2/output//PEcAn_1000007732')
#TO DO: Skip ensemble members that fail or are missing in read.restart
#See talk with with Mike on 6/21/17
#covariance for NPP is really weird #need to revisit

#---------------- Load PEcAn settings file. --------------------------------#
# Open and read in settings file for PEcAn run.
settings <- read.settings("pecan.SDA.xml")

obs.list <- PEcAn.assim.sequential::load_data_paleon_sda(settings = settings)

IC <- NULL

status.start("IC")
ne <- as.numeric(settings$state.data.assimilation$n.ensemble)
state <- as.data.frame(rmvnorm(ne,as.numeric(obs.list$obs.mean[[1]]),(obs.list$obs.cov[[1]]), method = "svd"))
colnames(state)<-c('AGB','NPP')
IC <- sample.IC.SIPNET(ne, state = state)
status.end()

PEcAn.assim.sequential::sda.enkf(settings, obs.mean = obs.list$obs.mean, obs.cov = obs.list$obs.cov, IC = IC)

for(i in 2:length(obs.mean)){
  obs.mean[[i]]<-NA
  obs.cov[[i]]<-NA
}

