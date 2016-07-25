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
require(PEcAn.all)
library(PEcAn.assim.sequential)
library(PEcAn.visualization)
library(mvtnorm)
library(rjags)
library(reshape2)
#--------------------------------------------------------------------------------------------------#
#

settings <- read.settings("/fs/data2/output//PEcAn_1000002229/pecan.xml")
settings$ensemble$size <- 20
IC = matrix(NA,as.numeric(settings$ensemble$size),length(settings$pft))
settings$run$start.date <-"1960/01/01"
settings$run$end.date <-"1960/12/31"
settings$ensemble$start.date <-"1960/01/01"
settings$ensemble$end.date <-"1960/12/31"
variables <- c("AGB.pft","TotSoilCarb")
processvar <- TRUE
pick.trait.params <- c("G")
spp.params.default <- read.csv(system.file("spp_matrix.csv", package = "linkages")) #default spp.params

##################################################
load("/home/araiho/lyford_summary.Rdata")
row.keep <- list()
for(i in 1:15){
  row.keep[[i]]<-grep(rownames(ab_mat)[i],spp.params.default[,2])[1]
}
new.names <- spp.params.default[unlist(row.keep),1]
rm.spp <- which(is.na(new.names))
new.names<-new.names[-rm.spp]

ab_mat<-ab_mat[-rm.spp,]

rownames(ab_mat)<- paste0("AGB.pft.",new.names)
obs.mean <- list()
for(i in 1:ncol(ab_mat)){
  obs.mean[[i]] <- list(ab_mat[,i])
}

cov_array<-cov_array[-rm.spp,-rm.spp,]
colnames(cov_array)<-new.names
rownames(cov_array)<-new.names
obs.cov <- list()
for(i in 1:dim(cov_array)[3]){
  obs.cov[[i]] <- cov_array[,,i]
}


##################################################



lyford.dat <- readRDS("~/lyford_ab_group_v1.rds")
lyford.dat <- lyford.dat[lyford.dat$name!='Havi',]
old.names = c("Betula","Pinus","Fraxinus","Acer","Tsuga","Castanea","Quercus","Prunus",
              "Fagus")
new.names = paste0("AGB.pft.",c("Yellow Birch(Betula Alleghaniensis)","White Pine(Pinus Strobus)",
                                "White Ash(Fraxinus Americana)","Maple(Rubrum)",
                                "Hemlock(Tsuga Canadensis)","Chestnut(Dentana)",
                                "Champion Oak(Quercus Rubra)","Black Cherry(Prunus Serotina)",
                                "Beech(Grandifolia)"))
for(i in 1:length(old.names)){
  lyford.dat$name <- sub(old.names[i],new.names[i],lyford.dat$name)
}

lyford.mean.melt <- melt(lyford.dat[lyford.dat$quant=="mean",],id=c("year","name","group","type","quant","site_id"))
lyford.mean.cast <- acast(lyford.mean.melt,year ~ name, mean)
lyford.mean.cast[is.na(lyford.mean.cast)]<-0
obs.mean <- list()
for(i in 1:nrow(lyford.mean.cast)){
  obs.mean[[i]] <- list(lyford.mean.cast[i,])
}

lyford.sd.melt <- melt(lyford.dat[lyford.dat$quant=="sd",],id=c("year","name","group","type","quant","site_id"))
lyford.sd.cast <- acast(lyford.sd.melt,year ~ name, mean)
lyford.sd.cast[is.na(lyford.sd.cast)]<-0
obs.sd <- list()
for(i in 1:nrow(lyford.sd.cast)){
  obs.sd[[i]] <- list(lyford.sd.cast[i,])
}
