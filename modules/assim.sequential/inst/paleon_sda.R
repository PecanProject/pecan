
library(PEcAn.all)
library(PEcAn.SIPNET)
library(PEcAn.LINKAGES)
library(PEcAn.assim.sequential)
library(nimble)
library(lubridate)

ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

#LINKAGES #AGB.pft #Harvard Forest
#setwd('/fs/data2/output//PEcAn_1000003314/')
#TO DO: Having problem with running proc.var == TRUE because nimble isn't keeping the toggle sampler in the function environment.
#TO DO: Intial conditions for linkages are messed up. Need to calibrate.


#SIPNET
setwd('/fs/data2/output//PEcAn_1000003356')
#TO DO: Skip ensemble members that fail or are missing in read.restart
#See talk with with Mike on 6/21/17

#---------------- Load PEcAn settings file. --------------------------------#
# Open and read in settings file for PEcAn run.
settings <- read.settings("pecan.SDA.xml")

obs.list <- PEcAn.assim.sequential::load_data_paleon_sda(settings = settings)

IC <- NULL

status.start("IC")
ne <- as.numeric(settings$state.data.assimilation$n.ensemble)
IC <- sample.IC.SIPNET(ne, state = c('AGB','NPP'))
status.end()


PEcAn.assim.sequential::sda.enkf(settings, obs.mean = obs.list$obs.mean, obs.cov = obs.list$obs.cov, IC = IC)



