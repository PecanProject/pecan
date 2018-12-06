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
library(PEcAn.LINKAGES)
#--------------------------------------------------------------------------------------------------#
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
load('~/linkages_lyford_summary_v6.Rdata')
row.keep <- list()
spp.params.default <- read.csv(system.file("spp_matrix.csv", package = "linkages")) #default spp.params #this doesn't work unless linkages is in my home directory

for(i in 1:15){
  row.keep[[i]]<-grep(rownames(ab_mat)[i],spp.params.default[,2])[1]
}

dist.mat<-spp.params.default[unlist(row.keep),]
dist.mat<-dist.mat[-9,]
rownames(dist.mat)<-dist.mat[,1]
dist(dist.mat,method="canberra")

new.names <- spp.params.default[unlist(row.keep),1]
new.names[2] <- spp.params.default[18,1]
rm.spp <- which(is.na(new.names))
new.names<-new.names[-rm.spp]

ab_mat<-ab_mat[-rm.spp,]

rownames(ab_mat)<- paste0("AGB.pft.",new.names)
obs.mean <- list()
for(i in 1:ncol(ab_mat)){
  obs.mean[[i]] <- ab_mat[,i]
}

cov_array<-cov_array[-rm.spp,-rm.spp,]
colnames(cov_array)<-new.names
rownames(cov_array)<-new.names
obs.cov <- list()
for(i in 1:dim(cov_array)[3]){
  obs.cov[[i]] <- cov_array[,,i]
}
years<-1960:2014
names(obs.mean) <- paste0(years,'/12/31')

for(i in 1:14){
  plot(ab_mat[i,],typ='l',main=new.names[i])
  lines(ab_mat[i,]*cov_array[i,i,])
  lines(ab_mat[i,]*cov_array[i,i,])
  lines(ab_mat[i,]*(cov_array[i,i,]+1))
}


#---------------- Build Initial Conditions ----------------------------------------------------------------------#
status.start("IC")
#ne = as.numeric(settings$state.data.assimilation$n.ensemble)
IC = matrix(NA,as.numeric(settings$state.data.assimilation$n.ensemble),length(settings$pfts))
#sample.IC.SIPNET(ne,state,year=1)
status.end()

#--------------- Assimilation -------------------------------------------------------#
status.start("EnKF")
sda.enkf(settings=settings, obs.mean = obs.mean,
         obs.cov = obs.cov, IC = IC, Q = NULL)
status.end()