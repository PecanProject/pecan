#### R driver file for Bayesian fusion of  tree-ring and DBH data
#### based on Clark et al. 2007 hidden process model
#### development by Mike Dietze, with contributions by Margaret Evans, Kelly Heilman

#### analysis of Arizona PIPO FIA cores + DBH
#### addition of several fixed effects here
#### including time-varying effects

setwd("/home/rstudio")
library(tidyr)
library(tidyverse)
library(here)


### prep data files into jags objects
AZ.PIPO <- read.delim("data/AZ_FIA_RWL_PRISM_allinone_04192017.txt", stringsAsFactors = F) ### 820 trees

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
Tree2Tree <- read.csv("data/Tree2Tree.csv", stringsAsFactors = F)

### limit analysis to those trees with second DBH measurement in =< year 2015
### this is because as of 7/2017, the available PRISM data (KNMI) go only to Dec 2015
Tree2Tree <- Tree2Tree[Tree2Tree$T2_MEASYR<=2015,]

### eliminate those cases without SI (SDI seems to always be there)
Tree2Tree <- Tree2Tree[!is.na(Tree2Tree$SICOND),]
Tree2Tree <- Tree2Tree[!is.na(Tree2Tree$SDIc),]


# read in the FIA condition table to get DISTURBYR along with each disturbance code
FIA.COND <- read.csv("data/AZ_COND.csv", stringsAsFactors = F)
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

jags.new <- buildJAGSdataobject(newtemp2, YEARDISTURBED = TRUE, rnd.subset = 100, trunc.yr = 1966, forecast = TRUE)
saveRDS(jags.new, paste0("jags.new.", output.base.name, ".rds"))
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


data$a_dbh <- 512
data$r_dbh <- 256

data$a_inc <- 41
data$r_inc <- 1/0.22




### read in function that makes/executes a jags model from lmer-like call of a linear model
# note that Evans version of Dietze function comments out creation of state variable initial conditions (z0)
# which is done in the function buildJAGSdataobject instead
source("pecan/modules/data.land/R/InventoryGrowthFusion_unif.R") 


# # linear model with DBH^2 removed for Precipitation and 500 cores
cov.data$PLOT <- as.numeric(as.character(cov.data$PLOT))

# here we include calls for each model in our manuscript, but note that each one of these models will take ~24 hours to run with 40000 iterations

# Model 1: null model with just X and X^2 as covariates
model1 <- InventoryGrowthFusion_norm(data=data, cov.data=cov.data, time_data=time_data,
                                     n.iter=40000, z0=z0, scale.state = 30,
                                     n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                     fixed = "~ X + X^2",
                                     time_varying = NULL,
                                     burnin_plot=FALSE, save.jags = "Null.model.txt", model.name = "Null.model", 
                                     output.folder = "IGF_PIPO_AZ_mcmc/", breakearly = FALSE)

# Model 2: model with plot random interecept and slope on X^2 
model2 <- InventoryGrowthFusion_norm(data=data, cov.data=cov.data, time_data=time_data,
                                     n.iter=40000, z0=z0, scale.state = 30, rand.X = TRUE, 
                                     n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                     fixed = "~ X + X^2 + SDI",
                                     time_varying = "wintP.wateryr + tmax.fallspr + tmax.fallspr[t]*wintP.wateryr[t]",
                                     burnin_plot=FALSE, save.jags = "random_effects.txt", model.name = "random_effects", 
                                     output.folder = "IGF_PIPO_AZ_mcmc/", breakearly = FALSE)

# Model 3: model with plot random interecept and slope on X^2 effects + interactions
model3 <- InventoryGrowthFusion_norm(data=data, cov.data=cov.data, time_data=time_data,
                                     n.iter=40000, z0=z0, scale.state = 30, rand.X = TRUE, 
                                     n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                     fixed = "~ X + X^2 + SDI + SDI*X + X*tmax.fallspr[t] + X*wintP.wateryr[t]",
                                     time_varying = "wintP.wateryr + SDI*wintP.wateryr[t]  + tmax.fallspr + SDI*tmax.fallspr[t] + tmax.fallspr[t]*wintP.wateryr[t]",
                                     burnin_plot=FALSE, save.jags = "Random_effects_interactions.txt", model.name = "Random_effects_interactions", 
                                     output.folder = "IGF_PIPO_AZ_mcmc/", breakearly = FALSE)

# Model 4: model with plot random intercept + fixed interactions
model4 <- InventoryGrowthFusion_norm(data=data, cov.data=cov.data, time_data=time_data,
                                     n.iter=40000, z0=z0, scale.state = 30, rand.X = NULL, 
                                     n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                     fixed = "~ X + X^2 + SDI + SDI*X + X*tmax.fallspr[t] + X*wintP.wateryr[t]",
                                     time_varying = "wintP.wateryr + SDI*wintP.wateryr[t]  + tmax.fallspr + SDI*tmax.fallspr[t] + tmax.fallspr[t]*wintP.wateryr[t]",
                                     burnin_plot=FALSE, save.jags = "Fixed_interactions.txt", model.name = "Fixed_interactions", 
                                     output.folder = "IGF_PIPO_AZ_mcmc/", breakearly = FALSE)

# Model 5: model with plot random intercept + fixed interactions + Site index
model5 <- InventoryGrowthFusion_norm(data=data, cov.data=cov.data, time_data=time_data,
                                     n.iter=40000, z0=z0, scale.state = 30, rand.X = NULL, 
                                     n.chunk=100, save.state=TRUE, random="(1|PLOT[i])",
                                     fixed = "~ X + X^2 + SDI + SICOND + SDI*X + SICOND*X + X*tmax.fallspr[t] + X*wintP.wateryr[t]",
                                     time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t] + tmax.fallspr + SDI*tmax.fallspr[t] + SICOND*tmax.fallspr[t] + tmax.fallspr[t]*wintP.wateryr[t]",
                                     burnin_plot=FALSE, save.jags = "Fixed_interactions_site_quality.txt", model.name = "Fixed_interactions_site_quality", 
                                     output.folder = "IGF_PIPO_AZ_mcmc/", breakearly = FALSE)

# Workflow/analysis for the AZ PIPO data fusion analysis:
# for each model we then run:


# 1. conditional_effects_mcmc_plots_xscaled.R, which plots traceplots, interaction and main effects, and parameter correlations
# 2. plot_held_out_dbh_stage1.R to run the model validation with held out diameters from the same dataset
# 3. get_future_climate_timeseries.R this aggregates the climate data for making forecasts
# 4. plot_proportion_var.R run future forecasts & partition uncertainty into components  
# 5. parse_climate_size_forecast.R is the script that runs no dbh, dbh change, no climate change, climate change scenarios for the manuscript

# --------------------------------------------------------------
# Stage 2 model with trees with dbh measurements, but no tree cores
# --------------------------------------------------------------
# 1. get out data for the second stage (trees w/out cores)
jags.stuff.stage2 <- buildJAGSdataobject(temp2 = newtemp2, Tree2Tree = Tree2Tree.incored.plots, stage.2 =TRUE, rnd.subset = 1000, trunc.yr = 1966)
saveRDS(jags.stuff.stage2, "jags.data.200.stage2.rds")

# 2. Read in posterior estimates of the inventory growth fusion model parameters
posterior.ests <- readRDS("data/IGFFull.model.validation.nadapt5000.rds")
out <- as.matrix(posterior.ests)
betas <-out[,grep(pattern = "beta",colnames(out))]
taus <-out[,grep(pattern = "tau",colnames(out))]

posterior.ests <- cbind(betas, taus)
library(coda)
means <- apply(as.matrix(posterior.ests), 2, mean)
vars <- apply(as.matrix(posterior.ests), 2, var)
SD <- apply(as.matrix(posterior.ests), 2, sd)

# generate data frame with a summary of the posterior estimates
posterior.summary <- data.frame(means = apply(as.matrix(posterior.ests), 2, mean),
                                vars = apply(as.matrix(posterior.ests), 2, var),
                                SD = apply(as.matrix(posterior.ests), 2, sd))
posterior.summary$parameter <- rownames(posterior.summary)


# 3. Run InventoryGrowthFusion_stage_2_mvn.R 
source("pecan/modules/data.land/R/Inventory_Growth_Fusion_stage_2.R") 
# this specifies informative normal priors for each parameter (except the plot random effects)

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
                                            fixed = "~ X + X^2 + SDI + SICOND + SDI*X + SICOND*X + X*tmax.fallspr[t] + X*wintP.wateryr[t]",
                                            time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t] + tmax.fallspr + SDI*tmax.fallspr[t] + SICOND*tmax.fallspr[t] + tmax.fallspr[t]*wintP.wateryr[t]",
                                            burnin_plot=FALSE, 
                                            save.jags = "stage2_model5_mvn.site.200.txt", 
                                            model.name = "stage2_model5_mvn.site.200", 
                                            output.folder = "/home/rstudio/IGF_PIPO_AZ_mcmc/", breakearly = FALSE)



# 4. Run InventoryGrowthFusion_stage_2_mvn.R 
source("pecan/modules/data.land/R/Inventory_Growth_Fusion_stage_2_mvn.R") 
# this specifies informative multivariate normal prior for all parameters (except the plot random effects)
# note: InventoryGrowthFusion_stage_2_mvn.R currently just sets up the data and priors from the posterior estimates
#       It doesnt yet automatically generate the multivariate prior. This was written manually in the jags files.

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
                                            fixed = "~ X + X^2 + SDI + SICOND + SDI*X + SICOND*X + X*tmax.fallspr[t] + X*wintP.wateryr[t]",
                                            time_varying = "wintP.wateryr + SDI*wintP.wateryr[t] + SICOND*wintP.wateryr[t] + tmax.fallspr + SDI*tmax.fallspr[t] + SICOND*tmax.fallspr[t] + tmax.fallspr[t]*wintP.wateryr[t]",
                                            burnin_plot=FALSE, 
                                            save.jags = "stage2_model5_mvn.site.200.txt", 
                                            model.name = "stage2_model5_mvn.site.200", 
                                            output.folder = "/home/rstudio/IGF_PIPO_AZ_mcmc/", breakearly = FALSE)




### read in function that makes/executes a jags model from lmer-like call of a linear model

