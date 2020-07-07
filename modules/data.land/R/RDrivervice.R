#### R driver file for Bayesian fusion of  tree-ring and DBH data
#### based on Clark et al. 2007 hidden process model
#### development by Mike Dietze

#### analysis of Arizona PIPO FIA cores + DBH
#### addition of several fixed effects here
#### including time-varying effects
setwd("/home/rstudio")
library(tidyr)
library(tidyverse)
### prep data files into jags objects
AZ.PIPO <- read.delim("INV_FIA_DATA/data/AZ_FIA_RWL_PRISM_allinone_04192017.txt", stringsAsFactors = F) ### 820 trees

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
Tree2Tree <- read.csv("INV_FIA_DATA/data/Tree2Tree.csv", stringsAsFactors = F)

### limit analysis to those trees with second DBH measurement in =< year 2015
### this is because as of 7/2017, the available PRISM data (KNMI) go only to Dec 2015
Tree2Tree <- Tree2Tree[Tree2Tree$T2_MEASYR<=2015,]

### eliminate those cases without SI (SDI seems to always be there)
Tree2Tree <- Tree2Tree[!is.na(Tree2Tree$SICOND),]
Tree2Tree <- Tree2Tree[!is.na(Tree2Tree$SDIc),]


# read in the FIA condition table to get DISTURBYR along with each disturbance code
FIA.COND <- read.csv("INV_FIA_DATA/data/AZ_COND.csv", stringsAsFactors = F)
Tree2Tree <- merge(FIA.COND[c("DSTRBYR1", "DSTRBYR2", "DSTRBYR3", "CN", "PLT_CN", "INVYR", "PLOT")], Tree2Tree, by.x = "PLT_CN", by.y = "T1_PLT_CN")
unique(temp2$Plot) %in% unique(Tree2Tree$T1_PLOT)
incPlot.in.dbhPlot <- (unique(temp2$Plot) %in% unique(Tree2Tree$T1_PLOT) == TRUE)
length(incPlot.in.dbhPlot[incPlot.in.dbhPlot ==TRUE])

# 284 of 338 AZ increment plots are in the T1_PLOT id (this is the id that we use to identify plot random effects in the model, so I assume this is the PLOT id that we should use)
# that means we are missing disturbyear from 54 plots in AZ (or 15%)

distrub.plot <- merge(temp2, unique(Tree2Tree[,c("T1_PLOT", "DSTRBYR1", "DSTRBYR2", "DSTRBYR3")]),  by.y = "T1_PLOT", by.x = "PlotNo", all.x = TRUE, all.y = FALSE)

# reduce to the 284 unique plots where we have the plot ids that match the FIA_COND/tree2tree database.
distrub.plot.2 <- distrub.plot[distrub.plot$PlotNo %in% unique(Tree2Tree$T1_PLOT),]
length(unique(distrub.plot.2$Widths)) # this leaves us with 452 unique series of tree growth

newtemp2 <- distrub.plot.2[!duplicated(distrub.plot.2),] # this just makes sure there were no duplicates (there shouldn't be)



# join trees to cond table

# join cond table

### NOW go get function that makes jags objects out of the above
### setwd to github folder
# setwd("/home/rstudio/pecan/modules/data.land/R")
# 
# setwd("pecan/modules/data.land/R")

### read in function that creates jags objects from above data
source("pecan/modules/data.land/R/BuildJAGSdataobject.R")

# get all the data with cores set up for stage 1
jags.stuff <- buildJAGSdataobject(temp2, rnd.subset = 100, trunc.yr = 1966)
saveRDS(jags.stuff, "FIA_inc_data/jags.data.basic.rds")

saveRDS(jags.stuff, "jags.data.basic.rds")



# or you wish to not include trees without cores
T2T.nodup <- Tree2Tree[!duplicated(Tree2Tree),]

# sample from DBH measurements that only exist in plots that were also cored

Tree2Tree.incored.plots <- Tree2Tree[paste0(Tree2Tree$COUNTYCD, Tree2Tree$PLOT) %in% paste0(newtemp2$CountyNo, newtemp2$PlotNo) ,]

# looks like there are 5794 dbh measurements from plots where we also have tree cores:
length(Tree2Tree.incored.plots$PLT_CN)
# [1] 5794

# check that we only have plot ids with tree cores
#unique(paste0(Tree2Tree.incored.plots$COUNTYCD, Tree2Tree.incored.plots$PLOT)) %in% unique(paste0(newtemp2$CountyNo, newtemp2$PlotNo))

jags.new <- buildJAGSdataobject(newtemp2, YEARDISTURBED = TRUE, rnd.subset = 100, trunc.yr = 1966)

head(jags.new$cov.data)

data <- jags.new$data
z0 <- jags.new$z0
cov.data <- jags.new$cov.data
time_data <- jags.new$time_data

# or you wish to not include trees without cores
T2T.nodup <- Tree2Tree[!duplicated(Tree2Tree),]

# sample from DBH measurements that only exist in plots that were also cored

Tree2Tree.incored.plots <- Tree2Tree[paste0(Tree2Tree$COUNTYCD, Tree2Tree$PLOT) %in% paste0(newtemp2$CountyNo, newtemp2$PlotNo) ,]

# looks like there are 5794 dbh measurements from plots where we also have tree cores:
length(Tree2Tree.incored.plots$PLT_CN)
# [1] 5794

# check that we only have plot ids with tree cores
#unique(paste0(Tree2Tree.incored.plots$COUNTYCD, Tree2Tree.incored.plots$PLOT)) %in% unique(paste0(newtemp2$CountyNo, newtemp2$PlotNo))

data <- jags.new$data
z0 <- jags.new$z0
cov.data <- jags.new$cov.data
time_data <- jags.new$time_data

cov.data$FIREYR1 <- ifelse(cov.data$DSTRBCD1 %in% "30", cov.data$DSTRBYR1, NA)
cov.data$FIREYR2 <- ifelse(cov.data$DSTRBCD2 %in% "30", cov.data$DSTRBYR2, NA)
cov.data$FIREYR3 <- ifelse(cov.data$DSTRBCD3 %in% "30", cov.data$DSTRBYR3, NA)

View(cov.data[,c("FIREYR1", "FIREYR2", "FIREYR3")])

cov.data$FIREYR <- ifelse(is.na(cov.data$FIREYR1) & is.na(cov.data$FIREYR2) & is.na(cov.data$FIREYR2), NA, 
                          ifelse(is.na(cov.data$FIREYR1), cov.data$FIREYR2, cov.data$FIREYR1))


# make a matrix with the DSTRBYR
time_data$FIREYR  <- matrix(rep(cov.data$FIREYR, each = 45), nrow = 515 ,ncol = 45, byrow = TRUE) 

# calculate time since Fire disturbance
time_data$TimeSinceFIRE <-  matrix(rep(1966:2010, 515), nrow = 515 ,ncol = 45, byrow = TRUE) - time_data$FIREYR



time_data$TimeSinceFIRE[is.na(time_data$TimeSinceFIRE)] <- 51
data$a_dbh <- 512
data$r_dbh <- 256

data$a_inc <- 41
data$r_inc <- 1/0.22

### it's nice (reassuring) to look at the tree-ring and DBH data (reality check)
View(data$y)
View(data$z)
### initial conditions state variable (DBH)
View(z0)


# read in the cores for validation:
valid.cores <- read.csv("INV_FIA_DATA/data/validation_cores_SDI_plot.csv")
colnames(cov.data)
validation.cores <- valid.cores %>% select(CC_PLOT, TREE.x, SICOND, SDI, ELEV, SLOPE, ASPECT, STAGE2, STAGE3, STDAGE, TRTCD1, DSTRBCD1, MAP, MAT, T2_FIADB, DIA.x, INVYR, FILE) 
colnames(validation.cores)[1:2] <- c("PLOT", "TREE")


# get the time data for each validation core by matching the plots
time_data_new <- matrix(NA, nrow = length(validation.cores$PLOT), ncol = 45)
head(validation.cores)
cov.data$index <- 1:length(cov.data$PLOT)
cov.plots <- cov.data %>% select(-TREE)
names.df <- colnames(cov.plots)
dups <- duplicated(cov.plots[, names.df[1:14]])
cov.plots.nodups <- cov.plots[!dups, ]


validation.cores$index <- NA
for (i in 1:length(validation.cores$PLOT)){
  if(length(cov.plots.nodups[cov.plots.nodups$PLOT %in% validation.cores[i,]$PLOT, ]$index) == 0){
    validation.cores$index[i] <- NA
    
  }else{
    validation.cores$index[i] <- cov.plots.nodups[cov.plots.nodups$PLOT %in% validation.cores[i,]$PLOT, ]$index
  }
}
validation.cores <- validation.cores[!is.na(validation.cores$index),]
summary(validation.cores$index)
validation.w.files <- validation.cores
validation.cores <- validation.cores %>% select(-FILE)
tmax.fallspr <- wintP.wateryr <- matrix(NA, nrow = length(validation.cores$index), ncol = 45)
for(i in 1:nrow(wintP.wateryr)){
  tmax.fallspr[i,] <- time_data$tmax.fallspr[validation.cores[i,]$index,]
  wintP.wateryr[i,] <- time_data$wintP.wateryr[validation.cores[i,]$index,]
}

time_data_validation <- list(tmax.fallspr = tmax.fallspr, wintP.wateryr =  wintP.wateryr)


# get the DBH data for each validation core
ymat<- zmat <- matrix(NA, nrow = nrow(time_data_validation$tmax.fallspr), ncol = ncol(time_data_validation$tmax.fallspr))

for(i in 1:nrow(zmat)){
  yr.col <- validation.cores[i,]$INVYR
  dbh.val <- validation.cores[i,]$DIA.x
  
  colnames(zmat) <- 1966:2010
  zmat[i,colnames(zmat) %in% yr.col] <- dbh.val
  
}
zmat.validation <- zmat*2.54

data_validation <- list(z = zmat.validation, 
                        y =ymat,
                        time_data = time_data_validation,
                        cov_data = validation.cores)

# lets try just adding the validation data onto to end of the fitting data

data$y <- rbind(data$y, ymat)
data$z <- rbind(data$z, zmat.validation)
cov.data <- rbind(cov.data[1:15], validation.cores[1:15])
data$ni <- length(data$y[,1])
data$startyr <- c(data$startyr, as.vector(rep(1, length(validation.cores[,1]))))
data$startyr2 <- c(data$startyr2, as.vector(rep(2, length(validation.cores[,1]))))
data$endyr <- c(data$endyr, as.vector(rep(45, length(validation.cores[,1]))))

time_data$wintP.wateryr <- rbind(time_data$wintP.wateryr, time_data_validation$wintP.wateryr)
time_data$tmax.fallspr <- rbind(time_data$tmax.fallspr, time_data_validation$tmax.fallspr)
z0 <- rbind(z0, zmat.validation)
### read in function that makes/executes a jags model from lmer-like call of a linear model
# note that Evans version of Dietze function comments out creation of state variable initial conditions (z0)
# which is done in the function buildJAGSdataobject instead
source("pecan/modules/data.land/R/InventoryGrowthFusion_unif.R") 


# # linear model with DBH^2 removed for Precipitation and 500 cores
# ppt.noX2 <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
#                                   n.iter=40000, z0=z0,
#                                   n.chunk=100, save.state=TRUE, random="(1|PLOT[i])  + (1|FIRE[i])",
#                                   fixed = "~ X + SICOND + SDI + SDI*X + SICOND*X + X*wintP.wateryr[t] + SICOND*SDI",
#                                   time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t]",
#                                   burnin_plot=FALSE, save.jags = "PPT.noX2.5000nocores.40000.txt", model.name = "PPT.noX2.5000nocores.40000.txt", 
#                                   output.folder = "IGF_PIPO_AZ_mcmc/", breakearly = FALSE)
cov.data$PLOT <- as.numeric(as.character(cov.data$PLOT))
# linear model with DBH^2 removed for Precipitation + Tmax and 500 cores
ppt.noX2 <- InventoryGrowthFusion_norm(data=data, cov.data=cov.data, time_data=time_data,
                                       n.iter=40000, z0=z0, scale.state = 30,
                                       n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                       fixed = "~ X + X^2 + SDI + SDI*X + X*tmax.fallspr[t] + X*wintP.wateryr[t]",
                                       time_varying = "wintP.wateryr + SDI*wintP.wateryr[t]  + tmax.fallspr + SDI*tmax.fallspr[t] + tmax.fallspr[t]*wintP.wateryr[t]",
                                       burnin_plot=FALSE, save.jags = "Full.model.validation.nadapt5000.txt", model.name = "Full.model.validation.nadapt5000", 
                                       output.folder = "IGF_PIPO_AZ_mcmc/", breakearly = FALSE)

fixed = "~ X + SICOND + SDI + SDI*X + SICOND*X + X*tmax.fallspr[t] + X*wintP.wateryr[t] + SICOND*SDI",
time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t] + tmax.fallspr + SDI*tmax.fallspr[t] + SICOND*tmax.fallspr[t] + tmax.fallspr[t]*wintP.wateryr[t]",



# --------------------------------------------------------------

# --------------------------------------------------------------
# 1. get out data for the second stage (trees w/out cores)
jags.stuff.stage2 <- buildJAGSdataobject(temp2 = newtemp2, Tree2Tree = Tree2Tree.incored.plots, stage.2 =TRUE, rnd.subset = 200, trunc.yr = 1966)
saveRDS(jags.stuff.stage2, "jags.data.200.stage2.rds")

# 2. Read in posterior estimates of the inventory growth fusion model parameters
posterior.ests <- readRDS("/home/rstudio/INV_FIA_DATA/data/IGFPPT.noX2.tau.norm.129_0.0128.rds")

library(coda)
means <- apply(as.matrix(posterior.ests), 2, mean)
vars <- apply(as.matrix(posterior.ests), 2, var)
SD <- apply(as.matrix(posterior.ests), 2, sd)

# generate data frame with a summary of the posterior estimates
posterior.summary <- data.frame(means = apply(as.matrix(posterior.ests), 2, mean),
                                vars = apply(as.matrix(posterior.ests), 2, var),
                                SD = apply(as.matrix(posterior.ests), 2, sd))
posterior.summary$parameter <- rownames(posterior.summary)

# 3. Run InventoryGrowthFusion_stage_2.R 
source("modules/data.land/R/InventoryGrowthFusion_stage_2.R") 


stage.2.out <- InventoryGrowthFusion_stage2(data=jags.stuff.stage2$data, 
                                            cov.data=jags.stuff.stage2$cov.data, 
                                            time_data=jags.stuff.stage2$time_data,
                                            posterior.estimates = posterior.summary, 
                                            informative.time = TRUE, 
                                            informative.site = FALSE,
                                            n.iter=40000, 
                                            z0=jags.stuff.stage2$z0,
                                            n.chunk=100, 
                                            save.state=TRUE, 
                                            random="(1|PLOT[i])",
                                            fixed = "~ X + SICOND + SDI + SDI*X + SICOND*X + X*wintP.wateryr[t] + SICOND*SDI",
                                            time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t]",
                                            burnin_plot=FALSE, 
                                            save.jags = "test.stage2non_inform.site.200.txt", 
                                            model.name = "test.stage2model_non_inform.site.200", 
                                            output.folder = "/home/rstudio/IGF_PIPO_AZ_mcmc/", breakearly = FALSE)




### read in function that makes/executes a jags model from lmer-like call of a linear model

source("pecan/modules/data.land/R/InventoryGrowthFusion.R") 

### let's run some models!
## these are not good models because ppt and tmax are correlated with one another
#model.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
#                                   n.iter=5000, random="(1|PLOT[i])",
#                                   fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*wintP.JJ[t]",
#                                   time_varying = "wintP.JJ + tmax.JanA + SDI*wintP.JJ[t]",
#                                   burnin_plot=FALSE)


#KH this model does not converge because it has both Tempand PPT
# model.out <- InventoryGrowthFusionRestart(data=data, cov.data=cov.data, time_data=time_data,
#                                    n.iter=15000, z0=z0,
#                                   n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
#                                    fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*wintP.JJ[t]",
#                                    time_varying = "wintP.JJ + tmax.JanA + wintP.JJ[t]*tmax.JanA[t] + SDI*wintP.JJ[t]",
#                                    burnin_plot=FALSE, save.jags = "Full.model5000.txt", model.name = "Full.model.15000")

# test out ther restart of the non-converged mcmclist
# need the have pecan function "mcmc.list2init"
# there was an undefined "nr" in the mcmclist2init function
# 

# model.out.restart <- InventoryGrowthFusionRestart(data=data, cov.data=cov.data, time_data=time_data,
#                                                   restart = model.out,
#                                                   n.iter=500, z0=z0,
#                                                   n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
#                                                   fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*wintP.JJ[t]",
#                                                   time_varying = "wintP.JJ + tmax.JanA + wintP.JJ[t]*tmax.JanA[t] + SDI*wintP.JJ[t]",
#                                                   burnin_plot=FALSE, save.jags = "Full.model5000.txt", model.name = "Full.model.15000.restart")
# 
# 
# # very simple test model
# model.simple.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
#                                           n.iter=1000, random="(1|PLOT[i])",
#                                           fixed = "~ X + X^2 + SICOND + SDI + X*wintP.JJ[t]",
#                                           time_varying = "wintP.JJ",
#                                           burnin_plot=FALSE )
# 
# model.simple.old.pecan <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
#                                                 n.iter=1000, random="(1|PLOT[i])",
#                                                 fixed = "~ X + X^2 + SICOND + SDI + X*wintP.JJ[t]",
#                                                 time_varying = "wintP.JJ",
#                                                 burnin_plot=TRUE)

#restart.data <- load("/home/rstudio/pecan/IGF_PIPO_AZ_mcmc/.PPT.noX2.noint.5000.0.101.RData")

#restart.data <- as.mcmc(restart.data)

# linear model with DBH^2 removed for Precipitation and 500 cores
ppt.noX2 <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                  n.iter=40000, z0=z0,
                                  n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                  fixed = "~ X + SICOND + SDI + SDI*X + SICOND*X + X*wintP.wateryr[t] + SICOND*SDI",
                                  time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t]",
                                  burnin_plot=FALSE, save.jags = "PPT.noX2.5000nocores.40000.txt", model.name = "PPT.noX2.5000nocores.40000.txt", 
                                  output.folder = "IGF_PIPO_AZ_mcmc/", breakearly = FALSE)



ppt.noX2.no.int <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                         n.iter=15000, z0=z0,
                                         n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                         fixed = "~ X + SICOND + SDI" ,
                                         time_varying = "wintP.wateryr",
                                         burnin_plot=FALSE, output.folder = "/home/rstudio/pecan/IGF_PIPO_AZ_mcmc/", save.jags = "PPT.noX2.noint.5000.txt", model.name = "PPT.noX2.noint.5000", breakearly = FALSE)



### ppt models
fullmodelppt.out <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                          n.iter=5000, random="(1|PLOT[i])",
                                          fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*wintP.JJ[t] + SICOND*SDI",
                                          time_varying = "wintP.JJ + SDI*wintP.JJ[t] + SICOND*wintP.JJ[t]",
                                          burnin_plot=FALSE)

fullmodel.ppt.restart.out <- InventoryGrowthFusionRestart(data=data, cov.data=cov.data, time_data=time_data,
                                                          n.iter=15000, z0=z0,
                                                          n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                                          fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*wintP.wateryr[t] + SICOND*SDI",
                                                          time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t]",
                                                          burnin_plot=FALSE, save.jags = "WateryearPPT.only.l5000.txt", model.name = "WateryearPPT.only.15000", breakearly = FALSE)

source("InventoryGrowthFusion.R")

# linear model with DBH^2 removed for Precipitation:
ppt.noX2 <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                  n.iter=5000, z0=z0,
                                  n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                  fixed = "~ X + SICOND + SDI + SDI*X + SICOND*X + X*wintP.wateryr[t] + SICOND*SDI",
                                  time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t]",
                                  burnin_plot=FALSE, save.jags = "PPT.noX2.5000.txt", model.name = "PPT.noX2.5000", breakearly = FALSE)

# ppt.noX2.restart.noMu looks like it may have better convergence....lets run it longer...like to 10000

load(paste0("/home/rstudio/pecan/IGF_PIPO_AZ_mcmc/PPT.noX2.5000.50.RData"))

restart.noX2 <- jags.out

ppt.noX2.restarted <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                            n.iter=5000, z0=z0, restart = restart.noX2,
                                            n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                            fixed = "~ X + SICOND + SDI + SDI*X + SICOND*X + X*wintP.wateryr[t] + SICOND*SDI",
                                            time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t]",
                                            burnin_plot=FALSE, save.jags = "PPT.noX2.test.r.5000.txt", model.name = "PPT.noX2.test.r.5000", breakearly = FALSE)





ppt.noX2.restart.2 <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                            n.iter=25000, z0=z0,
                                            restart = ppt.noX2.restarted,
                                            n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                            fixed = "~ X + SICOND + SDI + SDI*X + SICOND*X + X*wintP.wateryr[t] + SICOND*SDI",
                                            time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t]",
                                            burnin_plot=FALSE, save.jags = "PPT.noX2.noMu.r2.25000.txt", model.name = "PPT.noX2.noMu.r2.25000", breakearly = FALSE)



# linear model with DBH^2 removed for Precipitation, with no X interactions:
ppt.noX2.no.int <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                         n.iter=15000, z0=z0,
                                         n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                         fixed = "~ X + SICOND + SDI + SICOND*SDI",
                                         time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t]",
                                         burnin_plot=FALSE, save.jags = "PPT.noX2.noXint.15000.txt", model.name = "PPT.noX2.noXint.15000", breakearly = FALSE)

# linear model with DBH^2 removed for Precipitation, with no interactions:

ppt.noX2.simple.no.int <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                                n.iter=15000, z0=z0,
                                                n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                                fixed = "~ X + SICOND + SDI",
                                                time_varying = "wintP.wateryr",
                                                burnin_plot=FALSE, save.jags = "PPT.noX2.noint.15000.txt", model.name = "PPT.noX2.noint.15000", breakearly = FALSE)



# linear model with DBH^2 removed for Precipitation, with no SICOND:

ppt.noX2.SDI.only <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                           n.iter=15000, z0=z0,
                                           n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                           fixed = "~ X + SDI + SDI*X +  X*wintP.wateryr[t]",
                                           time_varying = "wintP.wateryr + SDI*wintP.wateryr[t]",
                                           burnin_plot=FALSE, save.jags = "PPT.noX2.SDIonly.15000.txt", model.name = "PPT.noX2.SDIonly.15000", breakearly = FALSE)


# linear model with DBH^2 removed for Precipitation, with no SICOND:

ppt.noX2.SICOND.only <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                              n.iter=15000, z0=z0,
                                              n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                              fixed = "~ X + SICOND + SICOND*X +  X*wintP.wateryr[t]",
                                              time_varying = "wintP.wateryr + SICOND*wintP.wateryr[t]",
                                              burnin_plot=FALSE, save.jags = "PPT.noX2.SICONDonly.15000.txt", model.name = "PPT.noX2.SICONDonly.15000", breakearly = FALSE)


# linear model with DBH^2 removed for Precipitation + Temperature:
ppt.tmax.noX2 <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                       n.iter=25000, z0=z0,
                                       n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                       fixed = "~ X + SICOND + SDI + SDI*X + SICOND*X + X*tmax.fallspr[t] + X*wintP.wateryr[t] + SICOND*SDI",
                                       time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t] + tmax.fallspr + SDI*tmax.fallspr[t] + SICOND*tmax.fallspr[t] + tmax.fallspr[t]*wintP.wateryr[t]",
                                       burnin_plot=FALSE, save.jags = "PPT.TMAXfsnoX2.25000.txt", model.name = "PPT.TMAXfs.noX2.25000", breakearly = FALSE)

restart.noX2.t <- load(paste0("/home/rstudio/pecan/modules/data.land/R/PPT.TMAXfs.noX2.r.l0000.0.1.RData"))



tmax.ppt.nox2.mcmc <- as.mcmc(restart.noX2.t)

ppt.tmax.noX2_restart <- InventoryGrowthFusionRestart(data=data, cov.data=cov.data, time_data=time_data,
                                                      n.iter=15000, z0=z0,
                                                      n.chunk=50, save.state=TRUE, random="(1|PLOT[i])",
                                                      fixed = "~ X + SICOND + SDI + SDI*X + SICOND*X + X*tmax.fallspr[t] + X*wintP.wateryr[t] + SICOND*SDI",
                                                      time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t] + tmax.fallspr + SDI*tmax.fallspr[t] + SICOND*tmax.fallspr[t] + tmax.fallspr[t]*wintP.wateryr[t]",
                                                      burnin_plot=FALSE, save.jags = "PPT.TMAXfsnoX2.r.l0000.txt", model.name = "PPT.TMAXfs.noX2.r.l0000", breakearly = FALSE, restart = tmax.ppt.nox2.mcmc)


source("InventoryGrowthFusionME.R")
ppt.noXint.restart.out <- InventoryGrowthFusionRestart(data=data, cov.data=cov.data, time_data=time_data,
                                                       n.iter=5000, z0=z0,
                                                       n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                                       fixed = "~ X + X^2 + SICOND + SDI + SICOND*SDI",
                                                       time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t]",
                                                       burnin_plot=FALSE, save.jags = "PPT.noXint.only.l0000.txt", model.name = "PPT.noXint.only.l0000", breakearly = FALSE)


ppt.noX2.restart.out <- InventoryGrowthFusionRestart(data=data, cov.data=cov.data, time_data=time_data,
                                                     n.iter=5000, z0=z0,
                                                     n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                                     fixed = "~ X + SICOND + SDI + SDI*X + SICOND*X + X*wintP.wateryr[t] + SICOND*SDI",
                                                     time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t]",
                                                     burnin_plot=FALSE, save.jags = "PPT.noXint.only.l0000.txt", model.name = "PPT.noXint.only.l0000", breakearly = FALSE)


reducedmodel.ppt.restart.out <- InventoryGrowthFusionRestart(data=data, cov.data=cov.data, time_data=time_data,
                                                             n.iter=10000, z0=z0,
                                                             n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                                             fixed = "~ X + X^2 + SICOND + SDI + X*wintP.wateryr[t]",
                                                             time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t]",
                                                             burnin_plot=FALSE, save.jags = "WateryearPPT.red.l0000.txt", model.name = "WateryearPPT.red.10000")


fullmodel.ppt.restart.no.random.out <- InventoryGrowthFusionRestart(data=data, cov.data=cov.data, time_data=time_data,
                                                                    n.iter=15000, z0=z0,
                                                                    n.chunk=100, save.state=TRUE, random=NULL,
                                                                    fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*wintP.wateryr[t] + SICOND*SDI",
                                                                    time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t]",
                                                                    burnin_plot=FALSE, save.jags = "WateryearPPT.no.random.l5000.txt", model.name = "WateryearPPT.no.random.15000")


fullmodel.ppt.restart.out <- InventoryGrowthFusionRestart(data=data, cov.data=cov.data, time_data=time_data,
                                                          n.iter=15000, z0=z0,
                                                          n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                                          fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*wintP.JJ[t] + SICOND*SDI",
                                                          time_varying = "wintP.JJ + SDI*wintP.JJ[t] + SICOND*wintP.JJ[t]",
                                                          burnin_plot=FALSE, save.jags = "PPT.only.l5000.txt", model.name = "WintPPT.only.15000")

fullmodel.ppt.restart.out <- InventoryGrowthFusionRestart(data=data, cov.data=cov.data, time_data=time_data,
                                                          n.iter=15000, z0=z0,
                                                          n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                                          fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*wintP.JJ[t] + SICOND*SDI",
                                                          time_varying = "wintP.JJ + SDI*wintP.JJ[t] + SICOND*wintP.JJ[t]",
                                                          burnin_plot=FALSE, save.jags = "PPT.only.l5000.txt", model.name = "WintPPT.only.15000", breakearly = FALSE)



fullmodel.tmax.spr.fall.restart.out <- InventoryGrowthFusionRestart(data=data, cov.data=cov.data, time_data=time_data,
                                                                    n.iter=15000, z0=z0,
                                                                    n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                                                    fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*tmax.fallspr[t] + SICOND*SDI",
                                                                    time_varying = "tmax.fallspr + SDI*tmax.fallspr[t] + SICOND*tmax.fallspr[t]",
                                                                    burnin_plot=FALSE, save.jags = "tmax.fallspr.Tmax.only.l5000.txt", model.name = "tmax.fallspr.Tmax.only.15000")

fullmodel.tmax.spr.ppt.fall.restart.out <- InventoryGrowthFusionRestart(data=data, cov.data=cov.data, time_data=time_data,
                                                                        n.iter=15000, z0=z0,
                                                                        n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                                                        fixed = "~ X + X^2 + SICOND + SDI + SDI*X + SICOND*X + X*tmax.fallspr[t] + X*wintP.wateryr[t] + SICOND*SDI",
                                                                        time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t] + tmax.fallspr + SDI*tmax.fallspr[t] + SICOND*tmax.fallspr[t] + tmax.fallspr[t]*wintP.wateryr[t]",
                                                                        burnin_plot=FALSE, save.jags = "ppt.wateryear.tmax.fallspr.Tmax.only.l5000.txt", model.name = "ppt.wateryear.tmax.fallspr.Tmax.only.15000")


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
model.out <- jags.out
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
#main.dir <- "/Users/kah/Documents/GrowthFusion"
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
Tree2Tree <- read.csv("FIA_inc_data/Tree2Tree.csv", stringsAsFactors = F)

### limit analysis to those trees with second DBH measurement in =< year 2015
### this is because as of 7/2017, the available PRISM data (KNMI) go only to Dec 2015
Tree2Tree <- Tree2Tree[Tree2Tree$T2_MEASYR<=2015,]

### eliminate those cases without SI (SDI seems to always be there)
Tree2Tree <- Tree2Tree[!is.na(Tree2Tree$SICOND),]
Tree2Tree <- Tree2Tree[!is.na(Tree2Tree$SDIc),]


FIA.COND <- read.csv("/Users/kah/Documents/docker_pecan/pecan/FIA_inc_data/AZ_COND.csv", stringsAsFactors = F)
Tree2Tree <- merge(FIA.COND[c("DSTRBYR1", "DSTRBYR2", "DSTRBYR3", "CN", "PLT_CN", "INVYR", "PLOT")], Tree2Tree, by.x = "PLT_CN", by.y = "T1_PLT_CN")
unique(temp2$Plot) %in% unique(Tree2Tree$T1_PLOT)

distrub.plot <- merge(temp2, unique(Tree2Tree[,c("T1_PLOT", "DSTRBYR1", "DSTRBYR2", "DSTRBYR3")]),  by.y = "T1_PLOT", by.x = "PlotNo", all.x = TRUE, all.y = FALSE)

newtemp2 <- distrub.plot[!duplicated(distrub.plot),]

#ggplot(distrub.plot, aes())

# join trees to cond table

# join cond table

### NOW go get function that makes jags objects out of the above
### setwd to github folder
setwd("/home/rstudio/pecan/modules/data.land/R")

setwd("pecan/modules/data.land/R")

### read in function that creates jags objects from above data
source("modules/data.land/R/BuildJAGSdataobject.R")
#jags.stuff <- buildJAGSdataobject(temp2, Tree2Tree, rnd.subset = 5000, trunc.yr = 1966)
# if you don't have trees without cores, use the following line
# or you wish to not include trees without cores
T2T.nodup <- Tree2Tree[!duplicated(Tree2Tree),]

# sample from DBH measurements that only exist in plots that were also cored

Tree2Tree.incored.plots <- Tree2Tree[paste0(Tree2Tree$COUNTYCD, Tree2Tree$PLOT) %in% paste0(newtemp2$CountyNo, newtemp2$PlotNo) ,]

# looks like there are 5794 dbh measurements from plots where we also have tree cores:
# > length(Tree2Tree.incored.plots$PLT_CN)
# [1] 5794

# check that we only have plot ids with tree cores
#unique(paste0(Tree2Tree.incored.plots$COUNTYCD, Tree2Tree.incored.plots$PLOT)) %in% unique(paste0(newtemp2$CountyNo, newtemp2$PlotNo))


jags.stuff <- buildJAGSdataobject(temp2 = newtemp2, Tree2Tree = Tree2Tree.incored.plots, rnd.subset = 100, trunc.yr = 1966)


# 
#jags.stuff <- buildJAGSdataobject(newtemp2,  rnd.subset = 100, trunc.yr = 1966)

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
source("/Users/kah/Documents/docker_pecan/pecan/modules/data.land/R/InventoryGrowthFusion.R") 


# linear model with DBH^2 removed for Precipitation and 500 cores
ppt.noX2 <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                  n.iter=40000, z0=z0,
                                  n.chunk=100, save.state=TRUE, random="(1|PLOT[i])  + (1|FIRE[i])",
                                  fixed = "~ X + SICOND + SDI + SDI*X + SICOND*X + X*wintP.wateryr[t] + SICOND*SDI",
                                  time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t]",
                                  burnin_plot=FALSE, save.jags = "PPT.noX2.5000nocores.40000.txt", model.name = "PPT.noX2.5000nocores.40000.txt", 
                                  output.folder = "IGF_PIPO_AZ_mcmc/", breakearly = FALSE)


# linear model with DBH^2 removed for Precipitation + Tmax and 500 cores
ppt.noX2 <- InventoryGrowthFusion(data=data, cov.data=cov.data, time_data=time_data,
                                  n.iter=40000, z0=z0,
                                  n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                  fixed = "~ X + SICOND + SDI + SDI*X + SICOND*X + X*wintP.wateryr[t] + SICOND*SDI",
                                  time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t]",
                                  burnin_plot=FALSE, save.jags = "PPT.noX2.5000nocores.40000.txt", model.name = "PPT.noX2.5000nocores.40000.txt", 
                                  output.folder = "IGF_PIPO_AZ_mcmc/", breakearly = FALSE)




source("mcmc.list2initIGF.R")

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

