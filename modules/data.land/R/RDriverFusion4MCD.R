#### R driver file for Bayesian fusion of  tree-ring and DBH data
#### based on Clark et al. 2007 hidden process model
#### development by Mike Dietze

#### analysis of Arizona PIPO FIA cores + DBH
#### addition of several fixed effects here
#### including time-varying effects

### prep data files into jags objects
#setwd("C:/Users/mekevans/Documents/CDrive/Bayes/DemogRangeMod/ProofOfConcept/treerings/FIAmetadata/ArizonaData/MergedDatabase/New")

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
source("BuildJAGSdataobject.R")
jags.stuff <- buildJAGSdataobject(temp2, trunc.yr = 1966)
data <- jags.stuff$data
z0 <- jags.stuff$z0
cov.data <- jags.stuff$cov.data
time_data <- jags.stuff$time_data

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
                                    n.iter=5000, random="(1|PLOT[i])",
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
                                     n.iter=5000, random="(1|PLOT[i])",
                                     fixed = "~ X + X^2 + SICOND + SDI + X*tmax.JanA[t]",
                                     time_varying = "tmax.JanA + SICOND*tmax.JanA[t]",
                                     burnin_plot=FALSE)