
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
setwd('/fs/data2/output//PEcAn_1000003314/')

#SIPNET
#setwd('/fs/data2/output//PEcAn_1000003356')
#TO DO: Normalize state vector because NPP is too small.
#See talk with with Mike on 6/21/17

#---------------- Load PEcAn settings file. --------------------------------#
# Open and read in settings file for PEcAn run.
settings <- read.settings("pecan.SDA.xml")

obs.list <- PEcAn.assim.sequential::load_data_paleon_sda(settings = settings)

IC <- NULL

# status.start("IC")
# ne <- as.numeric(settings$state.data.assimilation$n.ensemble)
# IC <- sample.IC.SIPNET(ne, state = c('AGB','NPP'))
# status.end()

sda.enkf(settings, obs.mean = obs.list$obs.mean, obs.cov = obs.list$obs.cov, IC = IC)



