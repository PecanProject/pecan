#--------------------------------------------------------------------------------#
# functions used to write STATUS used by history
#--------------------------------------------------------------------------------#
options(warn = 1, keep.source = TRUE, error = quote({
  status.end("ERROR")
}))

status.start <- function(name) {
  cat(paste(name, format(Sys.time(), "%F %T"), sep="\t"), file=file.path(settings$outdir, "STATUS"), append=TRUE)      
}

status.end <- function(status="DONE") {
  cat(paste("", format(Sys.time(), "%F %T"), status, "\n", sep="\t"), file=file.path(settings$outdir, "STATUS"), append=TRUE)      
}

#---------------- Load libraries. -----------------------------------------------------------------#
library(PEcAn.all)
library(PEcAn.assim.sequential)
library(PEcAn.visualization)
library(PEcAn.allometry)
library(mvtnorm)
library(rjags)
library(reshape2)
#--------------------------------------------------------------------------------------------------#
#
# 
# <state.data.assimilation>
#   <n.ensemble>35</n.ensemble>
#   <process.variance>FALSE</process.variance>
#   <sample.parameters>TRUE</sample.parameters>
#   <state.variables>
#   <variable>
#   <variable.name>NPP</variable.name>
#   <unit>MgC/ha/yr</unit>
#   </variable>
#   <variable>
#   <variable.name>AbvGrndWood</variable.name>
#   <unit>KgC/m^2</unit>
#   </variable>
#   <variable>
#   <variable.name>TotSoilCarb</variable.name>
#   <unit>KgC/m^2</unit>
#   </variable>
#   <variable>
#   <variable.name>LeafC</variable.name>
#   <unit>m^2/m^2</unit>
#   </variable>
#   <variable>
#   <variable.name>SoilMoistFrac</variable.name>
#   <unit></unit>
#   </variable>
#   <variable>
#   <variable.name>SWE</variable.name>
#   <unit>cm</unit>
#   </variable>
#   <variable>
#   <variable.name>Litter</variable.name>
#   <unit>gC/m^2</unit>
#   </variable>
#   </state.variables>
#   <forecast.time.step>1</forecast.time.step>
#   <start.date>1961/01/01</start.date>
#   <end.date>2010/12/31</end.date>
#   </state.data.assimilation>

#---------------- Load PEcAn settings file. -------------------------------------------------------#
# Open and read in settings file for PEcAn run.
settings <- read.settings("pecan.SDA.xml") 
#--------------------------------------------------------------------------------------------------#

#---------------- Load data. -------------------------------------------------------#
load('~/sipnet_lyford_summary.Rdata')
years<-1962:2015
names(obs.mean) <- paste0(years,'/12/31')

#---------------- Build Initial Conditions ----------------------------------------------------------------------#
status.start("IC")
ne = as.numeric(settings$state.data.assimilation$n.ensemble)
IC = sample.IC.SIPNET(ne,state,year=1)
status.end()

#--------------- Assimilation -------------------------------------------------------#
status.start("EnKF")
sda.enkf(settings=settings, obs.mean = obs.mean,
         obs.cov = obs.cov, IC = IC, Q = NULL)
status.end()