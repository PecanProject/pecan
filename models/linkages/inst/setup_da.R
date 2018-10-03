#plots a confidence interval around an x-y plot (e.g. a timeseries)
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

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
#library(PEcAn.visualization)
library(mvtnorm)
library(rjags)
library(reshape2)
library(PEcAn.LINKAGES)
#--------------------------------------------------------------------------------------------------#
#
source('~/pecan/modules/assim.sequential/R/sda.enkf.R')

######### sipnet
settings <- read.settings("/fs/data2/output//PEcAn_1000002613/pecan.SDA.xml")
settings$ensemble$size <- 50
settings$state.data.assimilation$n.ensemble<- 100
load(file.path(settings$outdir, "samples.Rdata"))
pick.trait.params <- c(names(ensemble.samples[[1]]),names(ensemble.samples[[2]]))

obs.mean <- list()
for(i in 1:10) {
  obs.mean[[i]]<-c(100+i, 5+i)
  names(obs.mean[[i]])<-c("NPP",'plantWood')
  }

obs.cov <- list()
for(i in 1:10) obs.cov[[i]]<- diag(c(.1,.08))

sda.enkf(settings=settings, obs.mean = obs.mean,
         obs.cov = obs.cov, IC = IC, Q = NULL)


######### linkages
settings <- read.settings("/fs/data2/output//PEcAn_1000002229/pecan.xml")
settings$ensemble$size <- 30
IC = matrix(NA,as.numeric(settings$ensemble$size),length(settings$pfts))
settings$run$start.date <-"1960/01/01"
settings$run$end.date <-"1960/12/31"
settings$ensemble$start.date <-"1960/01/01"
settings$ensemble$end.date <-"1960/12/31"
variables <- c("AGB.pft","TotSoilCarb")
processvar <- TRUE
pick.trait.params <- c("G")
spp.params.default <- read.csv(system.file("spp_matrix.csv", package = "linkages")) #default spp.params #this doesn't work unless linkages is in my home directory
sample_parameters=TRUE


##################################################
load("/home/araiho/linkages_lyford_summary.Rdata")
row.keep <- list()
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


sda.enkf(settings=settings,obs.mean = obs.mean,
         obs.cov = obs.cov, pick.trait.params = c("G"),
         given.process.variance = NULL)

##################################################