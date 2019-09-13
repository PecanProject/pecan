#### R driver file for Bayesian fusion of  tree-ring and DBH data
#### based on Clark et al. 2007 hidden process model
#### development by Mike Dietze

#### analysis of Arizona PIPO FIA cores + DBH
#### addition of several fixed effects here
#### including time-varying effects

### prep data files into jags objects
#setwd("C:/Users/mekevans/Documents/CDrive/Bayes/DemogRangeMod/ProofOfConcept/treerings/FIAmetadata/ArizonaData/MergedDatabase/New")
#setwd("C:/Users/mekevans/Documents/Cdrive/Bayes/DemogRangeMod/ProofOfConcept/treerings/FIAmetadata/ArizonaData/MergedDatabase/New")

### load in the data for trees with increment cores (and 1 or 2 DBH measurements)
#AZ.PIPO <- read.csv("AZ_FIA_RWL_PRISM_allinone_04192017.txt", header = T, sep = "\t", stringsAsFactors = F) ### 820 trees
main.dir <- "/Users/kah/Documents/GrowthFusion"
AZ.PIPO <- read.delim("FIA_inc_data/AZ_FIA_RWL_PRISM_allinone_04192017.txt", stringsAsFactors = F) ### 820 trees

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

### load in the data for trees without increment cores ("tree-to-tree" 2 DBH measurements)
#Tree2Tree <- read.csv("/Users/kah/Documents/GrowthFusion/FIA_inc_data/Tree2Tree.csv", stringsAsFactors = F)

### limit analysis to those trees with second DBH measurement in =< year 2015
### this is because as of 7/2017, the available PRISM data (KNMI) go only to Dec 2015
Tree2Tree <- Tree2Tree[Tree2Tree$T2_MEASYR<=2015,]

### eliminate those cases without SI (SDI seems to always be there)
Tree2Tree <- Tree2Tree[!is.na(Tree2Tree$SICOND),]
Tree2Tree <- Tree2Tree[!is.na(Tree2Tree$SDIc),]

### NOW go get function that makes jags objects out of the above
### setwd to github folder
setwd("/Users/kah/Documents/GrowthFusion/modules/data.land/R/")


### read in function that creates jags objects from above data
source("BuildJAGSdataobject.R")
#jags.stuff <- buildJAGSdataobject(temp2, Tree2Tree, rnd.subset = 5000, trunc.yr = 1966)
# if you don't have trees without cores, use the following line
# or you wish to not include trees without cores
jags.stuff <- buildJAGSdataobject(temp2, rnd.subset = 100, trunc.yr = 1966)
data <- jags.stuff$data
z0 <- jags.stuff$z0
cov.data <- jags.stuff$cov.data
time_data <- jags.stuff$time_data


data$a_dbh <- 512
data$r_dbh <- 256

### it's nice (reassuring) to look at the tree-ring and DBH data (reality check)
View(data$y)
View(data$z)
### initial conditions state variable (DBH)
View(z0)

### read in function that makes/executes a jags model from lmer-like call of a linear model
# note that Evans version of Dietze function comments out creation of state variable initial conditions (z0)
# which is done in the function buildJAGSdataobject instead
source("InventoryGrowthFusion.R") 

### let's run some models!
## these are not good models because ppt and tmax are correlated with one another
#model.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
#                                   n.iter=5000, random="(1|PLOT[i])",
#                                   fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*wintP.JJ[t]",
#                                   time_varying = "wintP.JJ + tmax.JanA + SDI*wintP.JJ[t]",
#                                   burnin_plot=FALSE)

#model.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
#                                   n.iter=5000, random="(1|PLOT[i])",
#                                   fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*wintP.JJ[t]",
#                                   time_varying = "wintP.JJ + tmax.JanA + wintP.JJ[t]*tmax.JanA[t] + SDI*wintP.JJ[t]",
#                                   burnin_plot=FALSE)

### ppt models
fullmodelppt.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                          n.iter=5000, random="(1|PLOT[i])",
                                          fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*wintP.JJ[t] + SICOND*SDI",
                                          time_varying = "wintP.JJ + SDI*wintP.JJ[t] + SICOND*wintP.JJ[t]",
                                          burnin_plot=FALSE)

model2.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                    n.iter=5000, random="(1|PLOT[i])",
                                    fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*wintP.JJ[t] + SICOND*SDI",
                                    time_varying = "wintP.JJ + SICOND*wintP.JJ[t]",
                                    burnin_plot=FALSE)

model3.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                    n.iter=5000, random="(1|PLOT[i])",
                                    fixed = "~ X + X^2 + SICOND + SDI + SICOND*X + X*wintP.JJ[t] + SICOND*SDI",
                                    time_varying = "wintP.JJ + SICOND*wintP.JJ[t]",
                                    burnin_plot=FALSE)

model4.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                    n.iter=15000, random="(1|PLOT[i])",
                                    fixed = "~ X + X^2 + SICOND + SDI + SICOND*X + X*wintP.JJ[t]",
                                    time_varying = "wintP.JJ + SICOND*wintP.JJ[t]",
                                    burnin_plot=FALSE)

model5.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                    n.iter=5000, random="(1|PLOT[i])",
                                    fixed = "~ X + X^2 + SICOND + SDI + X*wintP.JJ[t] + SICOND*SDI",
                                    time_varying = "wintP.JJ + SICOND*wintP.JJ[t]",
                                    burnin_plot=FALSE)

model6.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                    n.iter=5000, random="(1|PLOT[i])",
                                    fixed = "~ X + X^2 + SICOND + SDI + X*wintP.JJ[t]",
                                    time_varying = "wintP.JJ + SICOND*wintP.JJ[t]",
                                    burnin_plot=FALSE)


### tmax models
fullmodeltmax.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                           n.iter=5000, random="(1|PLOT[i])",
                                           fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*tmax.JanA[t] + SICOND*SDI",
                                           time_varying = "tmax.JanA + SDI*tmax.JanA[t] + SICOND*tmax.JanA[t]",
                                           burnin_plot=FALSE)

model8.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                    n.iter=5000, random="(1|PLOT[i])",
                                    fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*tmax.JanA[t] + SICOND*SDI",
                                    time_varying = "tmax.JanA + SICOND*tmax.JanA[t]",
                                    burnin_plot=FALSE)

model9.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                    n.iter=5000, random="(1|PLOT[i])",
                                    fixed = "~ X + X^2 + SICOND + SDI + SICOND*X + X*tmax.JanA[t] + SICOND*SDI",
                                    time_varying = "tmax.JanA + SICOND*tmax.JanA[t]",
                                    burnin_plot=FALSE)

model10.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                     n.iter=5000, random="(1|PLOT[i])",
                                     fixed = "~ X + X^2 + SICOND + SDI + SICOND*X + X*tmax.JanA[t]",
                                     time_varying = "tmax.JanA + SICOND*tmax.JanA[t]",
                                     burnin_plot=FALSE)

model11.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                     n.iter=5000, random="(1|PLOT[i])",
                                     fixed = "~ X + X^2 + SICOND + SDI + X*tmax.JanA[t] + SICOND*SDI",
                                     time_varying = "tmax.JanA + SICOND*tmax.JanA[t]",
                                     burnin_plot=FALSE)

model12.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                     n.iter=5000, n.chunk=5000, save.state=FALSE,
                                     random="(1|PLOT[i])",
                                     fixed = "~ X + X^2 + SICOND + SDI + X*tmax.JanA[t]",
                                     time_varying = "tmax.JanA + SICOND*tmax.JanA[t]",
                                     burnin_plot=FALSE)

#### LONG MCMC RUN WITH TREES WITHOUT CORES
model7.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                    n.iter=500000, n.chunk=1000, save.state=FALSE, random="(1|PLOT[i])",
                                    fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*tmax.JanA[t] + SICOND*SDI",
                                    time_varying = "tmax.JanA + SDI*tmax.JanA[t] + SICOND*tmax.JanA[t]",
                                    burnin_plot=FALSE)

model1.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data, z0=z0,
                                    n.iter=500000, n.chunk=1000, save.state=FALSE, random="(1|PLOT[i])",
                                    fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*wintP.JJ[t] + SICOND*SDI",
                                    time_varying = "wintP.JJ + SDI*wintP.JJ[t] + SICOND*wintP.JJ[t]",
                                    burnin_plot=FALSE)

#### DIAGNOSTICS
#### excerpted/modified from InventoryGrowthFusionDiagnostics.R (Dietze)
# see also https://faculty.washington.edu/jmiyamot/p548/demo.04-2b.convergence.diag.pdf
# and http://sbfnk.github.io/mfiidd/mcmc_diagnostics.html

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

SDI.beta <- out[,"betaSDI"]
SICOND.beta <- out[,"betaSICOND"]
X.beta <- out[,"betaX"]
X2.beta <- out[,"betaX2"]

hist(SDI.beta, main = "stand density index")
hist(SICOND.beta, main = "site index")  
hist(X.beta, main = "tree size")
hist(X2.beta, main = "quadratic tree size")  

### interaction effects
X.SDI.int <- out[,"betaX_SDI"]
X.SI.int <- out[,"betaX_SICOND"]  
X.wintP.int <- out[,"betaX_wintP.JJ"]
SDI.wintP.int <- out[,"betaSDI_wintP.JJ"]
SI.wintP.int <- out[,"betaSICOND_wintP.JJ"]
SI.SDI.int <- out[,"betaSICOND_SDI"]
hist(X.SDI.int, main = "size*stand density")
hist(X.SI.int, main = "size*site index")
hist(X.wintP.int, main = "size*wintP")
hist(SDI.wintP.int, main = "SDI*wintP")
hist(SI.wintP.int, main = "SI * wintP interaction")
hist(SI.SDI.int, main = "SI * SDI interaction")  

hist(out[,"deviance"])
mean(out[,"deviance"])


###############################
#### DATA EXPLORATION
hist(apply(z.small, 1, min, na.rm=T)) # size distribution at the time core was collected - each first DBH measurement (usually 1996)

### effect of diameter on growth increment (time series)
n=500 # number of time series to overplot at once
x.min <- min(z0[1:n,]); x.max <- max(z0[1:n,])
y.min <- min(y.small[1:n,], na.rm=T); y.max <- max(y.small[1:n,], na.rm=T)
plot(z0[1,], y.small[1,], 
     xlim=c(x.min, x.max), ylim=c(y.min, y.max),
     xlab="diameter", ylab="growth increment", type="n")
for (t in 1:n) {
  lines(z0[t,], y.small[t,])
}

# one outlier in the tree-ring data (y.small)
row.max <- apply(y.small, 1, max, na.rm=T) # find the largest diam increment across all years, each tree
max.gi <- max(row.max)
which(row.max == max.gi) # returns 239
# county 5, plot 1570, subplot 4, tree 2 DBH 17.8

# effect of tree size (DBH) on growth increment
mean.growth <- rowMeans(y.small, na.rm=T)
mean.diam <- rowMeans(z0, na.rm=T)
plot(z0[,1], mean.growth) # starting size
plot(mean.diam, mean.growth)

### plot all years*trees, using transparency
#plot(z0, y.small, xlim = c(-15, 70))
pdf("C:/old_laptop/mekevans/Documents/CDrive/Bayes/DemogRangeMod/ProofOfConcept/treerings/FIAmetadata/ArizonaData/MergedDatabase/New/SizeGrowth.pdf") 
plot(z0, y.small, xlim = c(0, 75), main="size vs. growth", col=rgb(0,100,0,10,maxColorValue=255), pch=16)
dev.off()

### effect of site index on growth increment
plot(SICOND, mean.growth)

### effect of stand density index on growth increment
plot(SDI, mean.growth)

### relationship between ppt and tmax
plot(colMeans(tmax.JanA), colMeans(wintP.JJ)) # across all sites (rows), region-wide climate signal
# warmer years are associated with less precipitation
m1 <- lm(colMeans(wintP.JJ) ~ colMeans(tmax.JanA)) # r^2 is 0.486
cor(colMeans(tmax.JanA), colMeans(wintP.JJ)) # correlation -0.697

# plot all years of data, all sites
pdf("C:/old_laptop/mekevans/Documents/CDrive/Bayes/DemogRangeMod/ProofOfConcept/treerings/FIAmetadata/ArizonaData/MergedDatabase/New/pptvstmax.pdf") 
plot(tmax.JanA, wintP.JJ, main="ppt vs. tmax", col=rgb(0,100,0,10,maxColorValue=255), pch=16)
dev.off()
# there's another way to do this with hex binning (function hexbin)
# see: http://www.statmethods.net/graphs/scatterplot.html

# little relationship between SI and SDI
plot(SICOND, SDI)
m1 <- lm(SDI ~ SICOND) # r^2 is 0.005
cor(SDI, SICOND) # correlation 0.0694

# relationship between SI and tree size (DIA.T1)
plot(SICOND, DIA.T1)
m1 <- lm(DIA.T1 ~ SICOND) # effect of SI is significant, r^2 is 0.12
cor(DIA.T1, SICOND) # correlation 0.351

# relationship between SDI and tree size
plot(SDI, DIA.T1)
m1 <- lm(DIA.T1 ~ SDI) # r^2 is 0.031

### correlation matrix
mydata <- cbind(SDI, SICOND, DIA.T1)
#rquery.cormat(mydata)# R can't find this function
library(corrplot)
mcor<-rcorr(as.matrix(mydata))
corrplot(mcor$r, type="upper", order="hclust", tl.col="black", tl.srt=45)
library(Hmisc); library(PerformanceAnalytics)
chart.Correlation(mydata, histogram = T, pch=19)

# 3D plot of growth, size, and SI or SDI
library(scatterplot3d)
scatterplot3d(z0[,1], SICOND, mean.growth, angle=70, pch=16, highlight.3d=TRUE,
              type="h",) # z0[,1] is the starting size

scatterplot3d(z0[,1], SDI, mean.growth, pch=16, highlight.3d=TRUE,
              type="h",) # starting size

### requires libraries car, rgl
library(car); library(rgl)
scatter3d(x = z0[,1], y = SICOND, z = mean.growth, fit = "smooth")
rgl.snapshot(filename = "growthsizeSI.png")
rgl.postscript("growthsizeSI.pdf",fmt="pdf")


