## example script for processing tree core data
## and fitting a state space model
setwd("~/Dropbox/Ecological Forecasting/Labs/Lab 8 - Tree Rings/")
library(rjags)
inc.unit.conv = 0.1

ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

parse.MatrixNames <- function(w,pre="x",numeric=FALSE){ 
  w = sub(pre,"",w)
  w = sub("[","",w,fixed=TRUE)
  w = sub("]","",w,fixed=TRUE)
  w = matrix(unlist(strsplit(w,",")),nrow=length(w),byrow=TRUE)
  if(numeric){
    class(w) <- "numeric"
  }
  colnames(w)<-c("row","col")
  return(as.data.frame(w))
}

## Read tree data
trees <- read.csv("H 2012 Adult Field Data.csv")
names(trees) = toupper(names(trees))
tree.ID = to.TreeCode(trees$SITE,trees$PLOT,trees$SUB,trees$TAG)

## Read tree ring data
rings <- Read_Tuscon("Revised 2/")
ring.file <- rep(names(rings),times=sapply(rings,ncol))
rings <- combine.rwl(rings)
ring.ID <- names(rings)
ring.info <- extract.stringCode(ring.ID)

## matching up data sets by tree
mch = match(tree.ID,ring.ID)
cored = apply(!is.na(trees[,grep("DATE_CORE_COLLECT",names(trees))]),1,any)
unmatched = which(cored & is.na(mch))
write.table(tree.ID[unmatched],file="unmatched.txt")
mch[duplicated(mch)] <- NA  ## if there's multiple stems, match the first

## combine data into one table
combined = cbind(trees,t(as.matrix(rings))[mch,-30:0 + nrow(rings)])
combined = combined[!is.na(combined$"2000"),]

## pull out growth to a matrix
y = as.matrix(combined[,!is.na(as.numeric(colnames(combined)))])*inc.unit.conv*2
time = as.numeric(colnames(y))

## pull out diameter to a matrix
DBH = as.matrix(combined[,grep("DBH",colnames(combined))])
class(DBH) <- "numeric"
z = matrix(NA,nrow(y),ncol(y))
DBH.years = as.numeric(sub("DBH","",colnames(DBH)))
DBH.years = ifelse(DBH.years < 20,DBH.years+2000,DBH.years+1900)
z[,which(time %in% DBH.years)] = DBH

##############   CODE TO FIT AN INDIVIDUAL RECORD ################

TreeDataFusion = "
model{
  
#### Data Model: DBH
for(i in 1:n){
z[i] ~ dnorm(x[i],tau_dbh)
}

#### Data Model: growth
for(i in 2:n){
inc[i] <- x[i]-x[i-1]
y[i] ~ dnorm(inc[i],tau_inc)
}

#### Process Model
for(i in 2:n){
Dnew[i] <- x[i-1] + mu
x[i]~dnorm(Dnew[i],tau_add)
}

#### Priors
x[1] ~ dnorm(x_ic,tau_ic)
tau_dbh ~ dgamma(a_dbh,r_dbh)
tau_inc ~ dgamma(a_inc,r_inc)
tau_add ~ dgamma(a_add,r_add)
mu ~ dnorm(0.5,0.5)
}"


## build data object for JAGS
data <- list(y=y[49,],z = z[49,],n=length(y[49,]),x_ic=1,tau_ic=0.00001,a_dbh=4,r_dbh=4,a_inc=1,r_inc=0.01,a_add=1,r_add=1)

## state variable initial condition
z0 = -rev(cumsum(rev(data$y)))+data$z[length(data$z)] 

## JAGS initial conditions
nchain = 3
init <- list()
for(i in 1:nchain){
  y.samp = sample(y,length(y),replace=TRUE)
  init[[i]] <- list(x = z0,tau_add=runif(1,1,5)/var(diff(y.samp),na.rm=TRUE),tau_dbh=1,tau_inc=100)
}

## compile JAGS model
j.model   <- jags.model (file = textConnection(TreeDataFusion),
                         data = data,
                         inits = init,
                         n.chains = 3)
## burn-in
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("tau_add","tau_dbh","tau_inc","mu"),
                            n.iter = 2000)
plot(jags.out)

## run MCMC
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("x","tau_add","tau_dbh","tau_inc","mu"),
                            n.iter = 10000)

### DBH
par(mfrow=c(2,1))
out <- as.matrix(jags.out)
ci <- apply(out[,substr(colnames(out),1,1)=="x"],2,quantile,c(0.025,0.5,0.975))

plot(time,ci[2,],type='n',ylim=range(ci,na.rm=TRUE),ylab="DBH (cm)")
ciEnvelope(time,ci[1,],ci[3,],col="lightBlue")
points(time,data$z,pch="+",cex=1.5)
lines(time,z0,lty=2)

## growth
inc.mcmc = apply(out[,substr(colnames(out),1,1)=="x"],1,diff)
inc.ci = apply(inc.mcmc,1,quantile,c(0.025,0.5,0.975))*5

plot(time[-1],inc.ci[2,],type='n',ylim=range(inc.ci,na.rm=TRUE),ylab="Ring Increment (mm)")
ciEnvelope(time[-1],inc.ci[1,],inc.ci[3,],col="lightBlue")
points(time,data$y*5,pch="+",cex=1.5,type='b',lty=2)

## process model
vars = (1:ncol(out))[-c(which(substr(colnames(out),1,1)=="x"),grep("tau",colnames(out)))]
for(i in vars){
  hist(out[,i],main=colnames(out)[i])
}
if(length(vars)>1) pairs(out[,vars])

## Standard Deviations
#layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))
par(mfrow=c(2,3))
prec = out[,grep("tau",colnames(out))]
hist(1/sqrt(prec[,1]),main=colnames(prec)[1])
hist(1/sqrt(prec[,2]),main=colnames(prec)[2])
hist(1/sqrt(prec[,3]),main=colnames(prec)[3])
plot(prec[,1],prec[,2],pch=".",xlab=colnames(prec)[1],ylab=colnames(prec)[2])
plot(prec[,1],prec[,2],pch=".",xlab=colnames(prec)[1],ylab=colnames(prec)[3])
plot(prec[,1],prec[,2],pch=".",xlab=colnames(prec)[2],ylab=colnames(prec)[3])
cor(prec[,1:3])

##############   CODE TO FIT MULTIPLE RECORDS ################

TreeDataFusionMV = "
model{

### Loop over all individuals
for(i in 1:ni){

  #### Data Model: DBH
  for(t in 1:nt){
    z[i,t] ~ dnorm(x[i,t],tau_dbh)
  }

  #### Data Model: growth
  for(t in 2:nt){
    inc[i,t] <- x[i,t]-x[i,t-1]
    y[i,t] ~ dnorm(inc[i,t],tau_inc)
  }

  #### Process Model
  for(t in 2:nt){
    Dnew[i,t] <- x[i,t-1] + mu
    x[i,t]~dnorm(Dnew[i,t],tau_add)
  }

  x[i,1] ~ dnorm(x_ic,tau_ic)
}  ## end loop over individuals

#### Priors
tau_dbh ~ dgamma(a_dbh,r_dbh)
tau_inc ~ dgamma(a_inc,r_inc)
tau_add ~ dgamma(a_add,r_add)
mu ~ dnorm(0.5,0.5)
}"

## version with tree and year random effects
TreeDataFusionMV = "
model{

### Loop over all individuals
for(i in 1:ni){

#### Data Model: DBH
for(t in 1:nt){
z[i,t] ~ dnorm(x[i,t],tau_dbh)
}

#### Data Model: growth
for(t in 2:nt){
inc[i,t] <- x[i,t]-x[i,t-1]
y[i,t] ~ dnorm(inc[i,t],tau_inc)
}

#### Process Model
for(t in 2:nt){
Dnew[i,t] <- x[i,t-1] + mu + ind[i] + year[t]
x[i,t]~dnorm(Dnew[i,t],tau_add)
}

## individual effects
ind[i] ~ dnorm(0,tau_ind)

## initial condition
x[i,1] ~ dnorm(x_ic,tau_ic)
}  ## end loop over individuals

## year effects
for(t in 1:nt){
  year[t] ~ dnorm(0,tau_yr)
}


#### Priors
tau_dbh ~ dgamma(a_dbh,r_dbh)
tau_inc ~ dgamma(a_inc,r_inc)
tau_add ~ dgamma(a_add,r_add)
tau_ind ~ dgamma(1,0.1)
tau_yr  ~ dgamma(1,0.1)
mu ~ dnorm(0.5,0.5)

}"


## build data object for JAGS
n = 10#nrow(y)
data <- list(y=y[1:n,],z = z[1:n,],ni=n,nt=ncol(y),x_ic=1,tau_ic=0.000001,
             a_dbh=8,r_dbh=4,a_inc=1,r_inc=0.01,a_add=1,r_add=1)

## state variable initial condition
z0 = t(apply(data$y,1,function(y){-rev(cumsum(rev(y)))})) + data$z[,ncol(z)] 

## JAGS initial conditions
nchain = 3
init <- list()
for(i in 1:nchain){
  y.samp = sample(data$y,length(data$y),replace=TRUE)
  init[[i]] <- list(x = z0,tau_add=runif(1,1,5)/var(diff(y.samp),na.rm=TRUE),
                    tau_dbh=1,tau_inc=500,tau_ind=50,tau_yr=100,ind=rep(0,data$ni),year=rep(0,data$nt))
}

## compile JAGS model
j.model   <- jags.model (file = textConnection(TreeDataFusionMV),
                         data = data,
                         inits = init,
                         n.chains = 3)
## burn-in
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("tau_add","tau_dbh","tau_inc","mu","tau_ind","tau_yr"),
                            n.iter = 2000)
plot(jags.out)

## run MCMC
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("x","tau_add","tau_dbh","tau_inc","mu","tau_ind","tau_yr"),
                            n.iter = 20000)


### DBH
#par(mfrow=c(3,2))
layout(matrix(c(1,2,3,4,5,6),3,2))
out <- as.matrix(jags.out)
x.cols = which(substr(colnames(out),1,1)=="x")
ci <- apply(out[,x.cols],2,quantile,c(0.025,0.5,0.975))
ci.names = parse.MatrixNames(colnames(ci),numeric=TRUE)

smp = sample.int(data$ni,3)
for(i in smp){
  sel = which(ci.names$row == i)
  plot(time,ci[2,sel],type='n',ylim=range(ci[,sel],na.rm=TRUE),ylab="DBH (cm)",main=tree.ID[i])
  ciEnvelope(time,ci[1,sel],ci[3,sel],col="lightBlue")
  points(time,data$z[i,],pch="+",cex=1.5)
  lines(time,z0[i,],lty=2)
}

## growth
for(i in smp){
  sel = which(ci.names$row == i)
  inc.mcmc = apply(out[,x.cols[sel]],1,diff)
  inc.ci = apply(inc.mcmc,1,quantile,c(0.025,0.5,0.975))*5
  #inc.names = parse.MatrixNames(colnames(ci),numeric=TRUE)
  
  plot(time[-1],inc.ci[2,],type='n',ylim=range(inc.ci,na.rm=TRUE),ylab="Ring Increment (mm)")
  ciEnvelope(time[-1],inc.ci[1,],inc.ci[3,],col="lightBlue")
  points(time,data$y[i,]*5,pch="+",cex=1.5,type='b',lty=2)
}

##check a DBH
plot(out[,which(colnames(out)=="x[3,31]")])
abline(h=z[3,31],col=2,lwd=2)
hist(out[,which(colnames(out)=="x[3,31]")])
abline(v=z[3,31],col=2,lwd=2)

## process model
vars = (1:ncol(out))[-c(which(substr(colnames(out),1,1)=="x"),grep("tau",colnames(out)))]
par(mfrow=c(1,1))
for(i in vars){
  hist(out[,i],main=colnames(out)[i])
}
if(length(vars)>1) pairs(out[,vars])

## Standard Deviations
#layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))
par(mfrow=c(2,3))
prec = out[,grep("tau",colnames(out))]
for(i in 1:ncol(prec)){
  hist(1/sqrt(prec[,i]),main=colnames(prec)[i])
}
cor(prec)

########### NEXT STEPS: ############
#what explain's the year effects? climate
#what explain's the individual effects? size, species, canopy position, plot -> landscape

## random effects
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("ind","year"),
                            n.iter = 2000)
out2 <- as.matrix(jags.out)

par(mfrow=c(1,1))
### YEAR
ci.yr <- apply(out2[,grep("year",colnames(out2))],2,quantile,c(0.025,0.5,0.975))
plot(time,ci.yr[2,],type='n',ylim=range(ci.yr,na.rm=TRUE),ylab="Year Effect")
ciEnvelope(time,ci.yr[1,],ci.yr[3,],col="lightBlue")
lines(time,ci.yr[2,],lty=1,lwd=2)
abline(h=0,lty=2)

### INDIV
boxplot(out2[,grep("ind",colnames(out2))],horizontal=TRUE,outline=FALSE,col=combined$PLOT)
abline(v=0,lty=2)
tapply(apply(out2[,grep("ind",colnames(out2))],2,mean),combined$PLOT,mean)
table(combined$PLOT)

spp = combined$SPP
boxplot(out2[order(spp),grep("ind",colnames(out2))],horizontal=TRUE,outline=FALSE,col=spp[order(spp)])
abline(v=0,lty=2)
spp.code = levels(spp)[table(spp)>0]
legend("bottomright",legend=rev(spp.code),col=rev(which(table(spp)>0)),lwd=4)
tapply(apply(out2[,grep("ind",colnames(out2))],2,mean),combined$SPP,mean)