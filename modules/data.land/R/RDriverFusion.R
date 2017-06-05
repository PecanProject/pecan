#### R driver file for Bayesian fusion of  tree-ring and DBH data
#### based on Clark et al. 2007 hidden process model
#### development by Mike Dietze

#### analysis of Arizona PIPO FIA cores + DBH
#### addition of several fixed effects here
#### including time-varying effects

### prep data files into jags objects
#setwd("C:/Users/mekevans/Documents/CDrive/Bayes/DemogRangeMod/ProofOfConcept/treerings/FIAmetadata/ArizonaData/MergedDatabase/New")
setwd("C:/Users/mekevans/Documents/Cdrive/Bayes/DemogRangeMod/ProofOfConcept/treerings/FIAmetadata/ArizonaData/MergedDatabase/New")

#AZ.PIPO <- read.csv("AZ_FIA_RWL_PRISM_allinone_04192017.txt", header = T, sep = "\t", stringsAsFactors = F) ### 820 trees
AZ.PIPO <- read.delim("AZ_FIA_RWL_PRISM_allinone_04192017.txt", stringsAsFactors = F) ### 820 trees

### merge together three diameter columns
AZ.PIPO$DIA <- ifelse(is.na(AZ.PIPO$TREE_DIA), AZ.PIPO$SITETREE_DIA, AZ.PIPO$TREE_DIA) # combine together FIADB diameter measurements for trees and site trees
AZ.PIPO$DIA <- ifelse(is.na(AZ.PIPO$DIA), AZ.PIPO$DBH, AZ.PIPO$DIA) # replace missing data with DBH recorded from core mounts (DBH)

### filter out those cases where DIA is NA...no size information
AZ.PIPO <- AZ.PIPO[!is.na(AZ.PIPO$DIA),] # 793 trees
### filter out cases where covariate data are missing (SICOND and SDI)
AZ.PIPO <- AZ.PIPO[!is.na(AZ.PIPO$COND_SICOND),] # 643 trees...~150 missing SICOND. Justin suggests they may be PJ (will never have SICOND)
AZ.PIPO <- AZ.PIPO[!is.na(AZ.PIPO$SDI),] # 641

### problem: in minority of cases, the difference between MEASYEAR and DateEnd is other than 0 or 1
### filter out those cases
temp1 <- AZ.PIPO[AZ.PIPO$PLOT_MEASYEAR-AZ.PIPO$DateEnd<2,] # 544 trees
temp2 <- temp1[temp1$PLOT_MEASYEAR-temp1$DateEnd>-1,] # no change


### read in function that creates jags objects from above data
source("BuildJAGSdataobject6.R")
jags.stuff <- buildJAGSdataobject(temp2)
data <- jags.stuff$data
z0 <- jags.stuff$z0
cov.data <- jags.stuff$cov.data
time_data <- jags.stuff$time_data

### read in function that makes/executes a jags model from lmer-like call of a linear model
# note that Evans version of Dietze function comments out creation of state variable initial conditions (z0)
# which is done in the function buildJAGSdataobject instead
source("InventoryGrowthFusionME.R") 

### let's run some models!
model.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                   n.iter=5000, random="(1|PLOT[i])",
                                   fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*wintP.JJ[t]",
                                   time_varying = "wintP.JJ + tmax.JanA + SDI*wintP.JJ[t]",
                                   burnin_plot=FALSE)

model.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                   n.iter=5000, random="(1|PLOT[i])",
                                   fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*wintP.JJ[t]",
                                   time_varying = "wintP.JJ + tmax.JanA + wintP.JJ[t]*tmax.JanA[t] + SDI*wintP.JJ[t]",
                                   burnin_plot=FALSE)

model.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                   n.iter=5000, random="(1|PLOT[i])",
                                   fixed = "~ X + X^2 + SICOND + SDI",
                                   time_varying = "tmax.JanA",
                                   burnin_plot=FALSE)

#### DIAGNOSTICS
#### excerpted/modified from InventoryGrowthFusionDiagnostics.R (Dietze)
out      <- as.matrix(model.out) ### LOADS MCMC OUTPUT INTO OBJECT "OUT"


### 12 temp and 12 precip variables
ppt.betas <- out[,c(grep("betappt", colnames(out)))]
par(mfrow = c(4, 3))
for (curr.month in month.abb) {
  curr.beta <- grep(pattern = curr.month, x = colnames(ppt.betas))
  hist(ppt.betas[, curr.beta], main = colnames(ppt.betas)[curr.beta])
}

tmax.betas <- out[,c(grep("betatmax", colnames(out)))]
par(mfrow = c(4, 3))
for (curr.month in month.abb) {
  curr.beta <- grep(pattern = curr.month, x = colnames(tmax.betas))
  hist(tmax.betas[, curr.beta], main = colnames(tmax.betas)[curr.beta])
}


wintP.JJ.beta <- out[,"betawintP.JJ"]
tmax.JanA.beta <- out[,"betatmax.JanA"]
hist(wintP.JJ.beta, main = "winter P (Jan-Jul)")
hist(tmax.JanA.beta, main = "tmax (Jan-Aug)")

par(mfrow = c(2, 2))
#  for (i in 1:2){ # SDI, SI
#    hist(betas[,i], main = colnames(betas)[i])
#  }   

SDI.beta <- out[,c(grep("betaSDI", colnames(out)))]
SICOND.beta <- out[,c(grep("betaSICOND", colnames(out)))]
X.beta <- out[,c(grep("betaX$", colnames(out)))]
X2.beta <- out[,c(grep("betaX2", colnames(out)))]

hist(SDI.beta, main = "stand density index")
hist(SICOND.beta, main = "site index")  
hist(X.beta, main = "tree size")
hist(X2.beta, main = "quadratic tree size")  

### interaction effects
X.SDI.int <- out[,"betaX_SDI"]
X.SI.int <- out[,"betaX_SICOND"]  
X.wintP.int <- out[,"betaX_wintP.JJ"]
SDI.wintP.int <- out[,"beta_SDI_wintP.JJ"]
hist(X.SDI.int, main = "size*stand density")
hist(X.SI.int, main = "size*site index")
hist(X.wintP.int, main = "size*wintP")
hist(SDI.wintP.int, main = "SDI*wintP")

#### DATA EXPLORATION
hist(apply(z.small, 1, min, na.rm=T)) # size distribution - each first DBH measurement (usually 1996)

### effect of diameter on growth increment (time series)
n=50
x.min <- min(z0[1:n,]); x.max <- max(z0[1:n,])
y.min <- min(y.small[1:n,], na.rm=T); y.max <- max(y.small[1:n,], na.rm=T)
plot(z0[1,], y.small[1,], 
     xlim=c(x.min, x.max), ylim=c(y.min, y.max),
     xlab="diameter", ylab="growth increment", type="n")
for (t in 1:n) {
  lines(z0[t,], y.small[t,])
}


mean.growth <- rowMeans(y.small, na.rm=T)
mean.diam <- rowMeans(z0, na.rm=T)
plot(z0[,1], mean.growth) # starting size
plot(mean.diam, mean.growth)

### effect of site index on growth increment
plot(SICOND, mean.growth)

### effect of stand density index on growth increment
plot(SDI, mean.growth)