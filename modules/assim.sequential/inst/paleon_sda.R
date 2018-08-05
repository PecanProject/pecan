
library(PEcAn.all)
library(PEcAn.SIPNET)
library(PEcAn.LINKAGES)
library(PEcAn.visualization)
library(PEcAn.ED2)
library(PEcAn.assim.sequential)
library(nimble)
library(lubridate)
library(PEcAn.visualization)

#LINKAGES #AGB.pft #Harvard Forest
#setwd('/fs/data2/output//PEcAn_1000003314/')
#setwd('/fs/data2/output//PEcAn_1000007999/') #full run 50 nens
setwd('/fs/data2/output//PEcAn_1000008008/')
file.copy('/fs/data2/output//PEcAn_1000007999/sda.obs.Rdata',getwd())
#TO DO: Having problem with running proc.var == TRUE because nimble isn't keeping the toggle sampler in the function environment.

## linkages fcomp
setwd('/fs/data2/output//PEcAn_1000008588/') #with variance inflation
adjustment=TRUE

setwd('/fs/data2/output//PEcAn_1000008683/') #without variance inflation
adjustment=TRUE

load("out/sda.initial.runs.Rdata")
#run inter part
load("sda.output.Rdata")
nt <- length(obs.list$obs.mean)
aqq <- array(NA,dim=c(nt,10,10))
t <- length(FORECAST)+1
aqq[t,,]<- solve(enkf.params[[t-1]]$q.bar)*enkf.params[[t-1]]$n
bqq<-numeric(nt)
bqq[t]<-enkf.params[[t-1]]$n


#SIPNET
#setwd('/fs/data2/output//PEcAn_1000003356')
#setwd('/fs/data2/output//PEcAn_1000007732')
#TO DO: Skip ensemble members that fail or are missing in read.restart
#See talk with with Mike on 6/21/17
#covariance for NPP is really weird #need to revisit

#---------------- Load PEcAn settings file. --------------------------------#
# Open and read in settings file for PEcAn run.
settings <- read.settings("pecan.SDA.xml")

#PEcAn.assim.sequential::
library(rgdal) # need to put in assim.sequential
library(ncdf4) # need to put in assim.sequential

obs.list <- load_data_paleon_sda(settings = settings) #add a way to get the correct time step in this function?


IC <- NULL

status.start("IC")
ne <- as.numeric(settings$state.data.assimilation$n.ensemble)
state <- as.data.frame(rmvnorm(ne,as.numeric(obs.list$obs.mean[[1]]),(obs.list$obs.cov[[1]]), method = "svd"))
colnames(state)<-c('AbvGrndWood','GWBI')
IC <- sample.IC.SIPNET(ne, state = state)
status.end()


#develop/debug
if(FALSE){
  obs.mean = obs.list$obs.mean
  obs.cov = obs.list$obs.cov
  Q = NULL
  adjustment = TRUE
  restart=NULL
}


PEcAn.assim.sequential::sda.enkf(settings, obs.mean = obs.list$obs.mean, obs.cov = obs.list$obs.cov, IC = IC)

obmn <- obvn <- list()
times.keep <- seq(1,1100,100)

for(i in 1:length(times.keep)){
  obmn[[i]] <- obs.mean[[times.keep[i]]]
  obvn[[i]] <- obs.cov[[times.keep[i]]]
}

names(obmn) <- names(obvn) <- names(obs.mean)[times.keep]

obs.mean <- obmn
obs.cov <- obvn

##### 
##### Plot Data #####
##### 

t1 <- 1
t <- length(obs.list$obs.mean)
names.y <- unique(unlist(lapply(obs.list$obs.mean[t1:t], function(x) { names(x) })))
Ybar <- t(sapply(obs.list$obs.mean[t1:t], function(x) {
  tmp <- rep(NA, length(names.y))
  names(tmp) <- names.y
  mch <- match(names(x), names.y)
  tmp[mch] <- x[mch]
  tmp
}))

YCI <- t(as.matrix(sapply(obs.list$obs.cov[t1:t], function(x) {
  if (length(x)<2) {
    rep(NA, length(names.y))
  }
  sqrt(diag(x))
})))

obs.dates <- rownames(Ybar)

green      <- col2rgb("green")
alphagreen <- rgb(green[1], green[2], green[3], 75, max = 255)


for(i in 1:length(obs.list$obs.mean[[1]])){
  plot(as.Date(obs.dates), 
       as.numeric(Ybar[, i]), 
       type = "l", 
       col = "darkgreen", 
       lwd = 2,ylim=c(0,1),main=colnames(Ybar)[i])
  ciEnvelope(as.Date(obs.dates),
             as.numeric(Ybar[, i]) - as.numeric(YCI[, i]) * 1.96, 
             as.numeric(Ybar[, i]) + as.numeric(YCI[, i]) * 1.96, 
             col = alphagreen)
  lines(as.Date(obs.dates), 
        as.numeric(Ybar[, i]), 
        type = "l", 
        col = "darkgreen", 
        lwd = 2)
}

