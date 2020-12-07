# script to make forecasts of tree level responses to climate change 
# and to parse climate response vs. changes in tree size.

# basic idea: 
# Use posteriors to forecast:
# 1. Initial Condition Uncertainty
# 2. Parameter Uncertainty
# 3. Process Uncertainty
# 4. Driver uncertainty (?)
library(tidyverse)
library(psych)
library(gridExtra)
library(pryr)
library(cowplot)
stage2 = FALSE
output.base.name <- "SDI_SI.norand.X.nadapt.5000"

# assuming that we already ran the conditional_effects_mcmc_plots script, and that output.base.name is in our env
jags.comb.params <- readRDS(file=paste0("IGF",output.base.name,".rds"))
jags.comb.params <- readRDS(file=paste0("/home/rstudio/data/IGFFull.model.validation.nadapt5000.rds"))

out <- as.matrix(jags.comb.params)
summary(out)
betas <- out[,grep(pattern = "beta",colnames(out))]
B0 <- out[,"mu"]


# get the alpha_PLOT intercept random effects:
alphas <- out[,grep(pattern = "alpha_PLOT",colnames(out))]

# get the betaX_PLOT slope random effects:
# note that I forgot to save the betaX_PLOTS on this run, so we dont have these estimates, but lets build the framework for generating the postior ests:
betaXplots <- out[,grep(pattern = "betaX_PLOT",colnames(out))]

# get the estimated x values for each tree/plot:
xval.ests<- readRDS(paste0("/home/rstudio/data/IGF_xvals_",output.base.name,".rds"))
x.mat <- as.matrix(xval.ests)
x.ci      <- apply(x.mat , 2, quantile, c(0.025, 0.5, 0.975))
x.ci[, "x[1,24]"]
hist(x.mat[,"x[1,45]"])

# make sure we have the average climate conditions for each plot/tree:
# We have plot MAP and MAT standarized in cov.data:


# Add the plot index to cov.data
plot.table <- data.frame(PLOT = unique(cov.data$PLOT), 
                         plotid = 1:length(unique(cov.data$PLOT)))
cov.data <- jags.new$cov.data
cov.data <- left_join(plot.table, cov.data, by  = "PLOT")
cov.data$id <- 1:length(cov.data$PLOT)

# KH note: need to standardize SDIseq first sdece I ran the model with standardized covariates
SDIseq.real <- seq(0,400,by=10)
SDIseq <- (SDIseq.real-mean(temp2$SDI))/sd(temp2$SDI)

# read in clim.data so we can properly scale:

clim.data <- readRDS("PRISM_non_scaled.rds")

# lets get the average climate for the 2020's:
clim.na <- readRDS(gzcon(url("https://de.cyverse.org/dl/d/134F7273-EDDC-48E1-BFD6-1632502F61FE/pipo.cores.with.8.5.2080s.climate.NA.rds")))

clim.na$cov.data$MAP.2020s.stand <-(clim.na$cov.data$MAP.2020s-mean(as.matrix(clim.data$wintP.wateryr)))/sd(as.matrix(clim.data$wintP.wateryr))
clim.na$cov.data$tmax.2020s.stand <-(clim.na$cov.data$tmax.2020s-mean(as.matrix(clim.data$tmax.fallspr)))/sd(as.matrix(clim.data$tmax.fallspr))

# for rcp 8.5:
clim.na$cov.data$MAP.85.2020s.stand <-(clim.na$cov.data$MAP.85.2020s-mean(as.matrix(clim.data$wintP.wateryr)))/sd(as.matrix(clim.data$wintP.wateryr))
clim.na$cov.data$tmax.85.2020s.stand <-(clim.na$cov.data$tmax85.2020s-mean(as.matrix(clim.data$tmax.fallspr)))/sd(as.matrix(clim.data$tmax.fallspr))


cov.data$MAP.85.2020s.stand <- clim.na$cov.data$MAP.85.2020s.stand 
cov.data$tmax.85.2020s.stand <-clim.na$cov.data$tmax.85.2020s.stand 



# get the time series of future climate:

clim.ts <- readRDS(gzcon(url("https://de.cyverse.org/dl/d/C38AB4DD-04D7-42DE-ABC0-C2CD2EA50BF9/pipo.cores.with.downscaled.hydro.ppt.climatev3")))

clim.ts.df <- clim.ts$future.climate.ts
clim.ts.df$tmax.fall.spr[is.nan(clim.ts.df$tmax.fall.spr)] <- NA
#tmax.fallspr.df <- tmax.fallspr


# need to scale future climate data on the same scale as the past climate

unscale_function <- function(zVar, myVar){(zVar * sd(myVar)) + mean(myVar)}


clim.ts.df$ppt.scale <-(clim.ts.df$year.ppt-mean(as.matrix(clim.data$wintP.wateryr)))/sd(as.matrix(clim.data$wintP.wateryr))
clim.ts.df$tmax.scaled <-(clim.ts.df$tmax.fall.spr-mean(as.matrix(clim.data$tmax.fallspr)))/sd(as.matrix(clim.data$tmax.fallspr))

# merge climate data with jags.cov data for stage 1 to get plot number, then crosswalk climate from there...why dont the LL's match? rounding

temp2.df.ll <- temp2 [, c("CountyNo", "PlotNo", "PLOT_LAT", "PLOT_LON")]
temp2.df.ll$PLOT <- paste0(temp2.df.ll$CountyNo, temp2.df.ll$PlotNo)
cov.data.test <- left_join(cov.data, temp2.df.ll, by = "PLOT")

unique(paste0(cov.data$PLOT)) %in% unique(paste0(temp2$CountyNo,temp2$PlotNo))  

# note that the plot lat and long are slightly different. Looks like one might be subplot and the other plot
plot(cov.data.test$LON, cov.data.test$PLOT_LON)
abline(a = 0, b = 1)

plot(cov.data.test$LAT, cov.data.test$PLOT_LAT)
abline(a = 0, b = 1)

# use the plot lat and plot lon to join with cliamte ensemble means

rm(tmax.fallspr)
climate.ensemble.means <- clim.ts.df %>% group_by(lat, lon, year, rcp) %>% 
  dplyr::summarise(mean.tmax.fs = quantile(tmax.scaled, na.rm = TRUE, 0.5), 
                   SD.tmax = var(tmax.scaled, na.rm = TRUE),
                   mean.ppt = quantile(ppt.scale, na.rm = TRUE, 0.5), 
                   SD.ppt = sd(ppt.scale, na.rm = TRUE),
                   n = n()) 
hist(climate.ensemble.means$mean.ppt )
hist(climate.ensemble.means$mean.tmax.fs )
#climate.ensemble.means$SD.tmax <- rnorm(length(climate.ensemble.means$SD.tmax), mean = 3, sd = 2)

new.table <- cov.data.test[,c("PLOT_LAT", "PLOT_LON",  "id", "plotid")]
colnames(new.table)[1:2] <- c("lat", "lon")

# merge with the climate.ensemble.means data base:
ens.means <- merge(new.table, climate.ensemble.means, by = c("lat", "lon"))
sample.ll <- ens.means[ens.means$id %in% 81,]
clim.ts.df.full <- left_join(new.table, clim.ts.df, by = c("lat", "lon"))
#ts.all  <- merge(new.table, clim.ts.df, by = c("lat", "lon"))
ts.all <- clim.ts.df.full
head(time_data)

head(ts.all)
ggplot(ts.all[ts.all$id %in% 81,], aes(x = year, y = year.ppt, color = modelrun))+geom_line()#+facet_wrap(~rcp)
ggplot(ts.all[ts.all$id %in% 92,], aes(x = as.numeric(year), y = tmax.fall.spr, color = modelrun))+geom_line() + stat_smooth()#+facet_wrap(~rcp)
ggplot(ts.all, aes(x = rcp, y = tmax.fall.spr, color = rcp))+geom_boxplot()#+facet_wrap(~rcp)

ts.all$period <- ifelse(ts.all$year >= 2018 & ts.all$year <2050, "2018 - 2050",
                        ifelse(ts.all$year >= 2050 & ts.all$year <2070, "2050 - 2070","2070-2099"))
ensemble.temp.plot <- ggplot(ts.all, aes(x = rcp, y = tmax.fall.spr, fill = period))+geom_boxplot()+theme_bw(base_size = 12)+
  theme(panel.grid = element_blank(), legend.title= element_blank())+ylab("Fall - Spring Maximum Temperature")+xlab("Scenario")

ggplot(ts.all, aes(x = rcp, y = tmax.fall.spr, color = period))+geom_boxplot()+facet_wrap(~modelrun)
ensemble.ppt.plot <-ggplot(ts.all, aes(x = rcp, y = year.ppt, fill = period))+geom_boxplot()+theme_bw(base_size = 12)+
  theme(panel.grid = element_blank(), legend.title= element_blank())+ylab("Total Precipitation")+xlab("Scenario")

png(height = 6, width = 10, units = "in", res = 300, "future_climate_ensemble_boxplots.png")
cowplot::plot_grid(ensemble.ppt.plot, ensemble.temp.plot, ncol = 2, align = "hv")
dev.off()

set.seed (11)


# ------------------------------------------------------------------------------------
# Make full forecast and parse the effects of climate vs tree size
# ------------------------------------------------------------------------------------
plot.future.forecast.clim.dbh.scenario <- function(m,  scenario = "rcp26",  print =TRUE){
  
  if(file.exists(paste0("scenarios/tree_", m,"_Diameter", "_", scenario,"_",output.base.name,"_scenario_preds", ".csv")) == FALSE){
  #-----------------------------------------------------------------------
  # Partitioning uncertainty
  #-----------------------------------------------------------------------
  # Create a function for our process model (linear state space model)
  
  iterate_statespace.dbh <- function( x = x.mat[,"x[1,53]"], m = m, betas.all, alpha, SDdbh, SDinc = 0, covariates) {
    
    #j <- 1
    
    
    # pseudocode for now
    tree.growth <- x +  alpha + betas.all$b0 + 
      betas.all$bSDI*covariates$SDI + 
      betas.all$bSDI_ppt*covariates$SDI*covariates$ppt + 
      betas.all$bX*(x-30) + 
      betas.all$bX2*(x-30)*(x-30) + 
      betas.all$bX_SDI*(x-30)*covariates$SDI + 
      betas.all$bX_ppt*covariates$ppt*(x-30) + 
      betas.all$bSI*covariates$SICOND + 
      betas.all$bSI_X*(x-30)*covariates$SICOND + 
      betas.all$bSI_wintP.wateryr*covariates$ppt*covariates$SICOND + 
      betas.all$bSI_tmax.fallspr*covariates$ppt*covariates$SICOND + 
      betas.all$bppt*covariates$ppt + 
      betas.all$btmax*covariates$tmax + 
      betas.all$bX_tmax*(x-30)*covariates$tmax + 
      betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
      betas.all$btmax_ppt*covariates$tmax*covariates$ppt 
    #tree.growth
    
    
    # Stochastic process model
    xpred <- rnorm(length(tree.growth), tree.growth, SDdbh) 
    
    #inc = xpred - x
    #yinc <-  rnorm(length(inc), inc, SDinc) 
    
    #if(rw == "ringwidth"){
    #  yinc
    #}else{
    xpred
    #}
  }
 
  
  
  
  iterate_statespace.no.change.dbh <- function( x = x.mat[,"x[1,53]"],x.dbh =x.mat[,"x[1,53]"] , m = m, betas.all, alpha, SDdbh, SDinc = 0, covariates) {
    
    #j <- 1
    
    
    # pseudocode for now
    tree.growth <- x +  alpha + betas.all$b0 + 
      betas.all$bSDI*covariates$SDI + 
      betas.all$bSDI_ppt*covariates$SDI*covariates$ppt + 
      betas.all$bX*(x.dbh-30) + 
      betas.all$bX2*(x.dbh-30)*(x.dbh-30) + 
      betas.all$bX_SDI*(x.dbh-30)*covariates$SDI + 
      betas.all$bX_ppt*covariates$ppt*(x.dbh-30) + 
      betas.all$bSI*covariates$SICOND + 
      betas.all$bSI_X*(x.dbh-30)*covariates$SICOND + 
      betas.all$bSI_wintP.wateryr*covariates$ppt*covariates$SICOND + 
      betas.all$bSI_tmax.fallspr*covariates$ppt*covariates$SICOND + 
      betas.all$bppt*covariates$ppt + 
      betas.all$btmax*covariates$tmax + 
      betas.all$bX_tmax*(x.dbh-30)*covariates$tmax + 
      betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
      betas.all$btmax_ppt*covariates$tmax*covariates$ppt 
    #tree.growth
    
    
    # Stochastic process model
    xpred <- rnorm(length(tree.growth), tree.growth, SDdbh) 
    
    #inc = xpred - x
    #yinc <-  rnorm(length(inc), inc, SDinc) 
    
    #if(rw == "ringwidth"){
    #  yinc
    #}else{
    xpred
    #}
  }
  #---------------------------------------------------------------------------
  ##  Uncertainty from Initial conditions AND beta parameters uncertainty
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  alpha = rep(quantile(alphas[, alphaplotid],0.5), 3299)
  #alpha <- rnorm(length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), mean = mean(alphas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]), alphaplotid]), sd = sd(alphas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]), alphaplotid]))
  bSDI <- rnorm(length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betaSDI"]), sd = sd(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betaSDI"]))
  bSDI_ppt <- rnorm(length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betaSDI_wintP.wateryr"]), sd = sd(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betaSDI_wintP.wateryr"]))
  if(!"betaX" %in% colnames(betas)){
    bX <-  rnorm(length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), mean = mean(betas[,paste0("betaX_PLOT[", cov.data[m,]$plotid, "]")]), sd = sd(betas[,paste0("betaX_PLOT[", cov.data[m,]$plotid, "]")]))
    
  }else{
    bX <-  rnorm(length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betaX"]), sd = sd(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betaX"]))
    
  }
  bX2 <- rnorm(length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betaX2"]), sd = sd(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betaX2"]))
  
  if("betaX_SDI" %in% colnames(betas)){
    bX_SDI <- rnorm(length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betaX_SDI"]), sd = sd(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betaX_SDI"]))
    
  }else{
    bX_SDI <- rep(0, 3300)
  }
  mcmc.its<- (length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])
  
  if("betaSICOND" %in% colnames(betas) == FALSE ){
    bSI <- rep(0,length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])) )
    bSI_X <- rep(0,length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])) )
    bSI_wintP.wateryr <- rep(0,length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])) )
    bSI_tmax.fallspr <- rep(0,length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])) )
  }else{
    bSI <- rnorm(length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), mean = mean(betas[mcmc.its,"betaSICOND"]), sd = sd(betas[mcmc.its,"betaSICOND"]))
    bSI_X <- rnorm(length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), mean = mean(betas[mcmc.its,"betaX_SICOND"]), sd = sd(betas[mcmc.its,"betaX_SICOND"]))
    bSI_wintP.wateryr <- rnorm(length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), mean = mean(betas[mcmc.its,"betaSICOND_wintP.wateryr"]), sd = sd(betas[mcmc.its,"betaSICOND_wintP.wateryr"]))
    
    bSI_tmax.fallspr <- rnorm(length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), mean = mean(betas[mcmc.its,"betaSICOND_tmax.fallspr"]), sd = sd(betas[mcmc.its,"betaSICOND_tmax.fallspr"]))
    
  }
  
  bX_ppt <- rnorm(length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betaX_wintP.wateryr"]), sd = sd(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betaX_wintP.wateryr"]))
  bppt <- rnorm(length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betawintP.wateryr"]), sd = sd(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betawintP.wateryr"]))
  btmax <- rnorm(length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betatmax.fallspr"]), sd = sd(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betatmax.fallspr"]))
  bX_tmax <- rnorm(length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betaX_tmax.fallspr"]), sd = sd(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betaX_tmax.fallspr"]))
  bSDI_tmax <- rnorm(length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betaSDI_tmax.fallspr"]), sd = sd(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betaSDI_tmax.fallspr"]))
  btmax_ppt <- rnorm(length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), mean = mean(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betatmax.fallspr_wintP.wateryr"]), sd = sd(betas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]),"betaSDI_tmax.fallspr"]))
  b0 <- rnorm(length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), mean = mean(B0[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])]), sd = sd(B0[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])]))
  
 
  alpha <- rnorm(length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), mean = mean(alphas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]), alphaplotid]), sd = sd(alphas[(length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"]), alphaplotid]))
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bSI, bSI_tmax.fallspr, bSI_wintP.wateryr,
                          bSI_X, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  
  future.proj <- unique(ens.means[ens.means$id == m & ens.means$rcp == scenario, ])
  proj.ordered <- future.proj[order(future.proj$year),]
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from Initial conditions AND parameters uncertainty but means drawn from past climate recycled
  #---------------------------------------------------------------------------
  ppt <- tmax <- SDI <- SICOND <-matrix(NA, nrow =length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), ncol = 82 )
  for(i in 1:82){
    
    ppt[,i]<- rnorm(n = length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])) , mean = mean(time_data$wintP.wateryr[m,]), sd = sd(time_data$wintP.wateryr[m,]))
    tmax[,i]<- rnorm(n = length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])) , mean = mean(time_data$tmax.fallspr[m,]), sd = sd(time_data$tmax.fallspr[m,]))
    SDI[,i]<- rep(cov.data[m, ]$SDI, length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])))
    SICOND[,i]<- rep(cov.data[m, ]$SICOND, length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])))
  }
  
  covariates <- list()
  covariates$SDI <- SDI
  covariates$ppt <- ppt
  covariates$tmax <- tmax
  covariates$SICOND <- SICOND

  
  time_steps <- 82
  nMCMC <- length(x.mat[,paste0("x[1,",jags.new$data$nt,"]")])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", jags.new$data$nt,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = data.frame(SDI = covariates$SDI[,t], 
                                                                                                                                                                                ppt = covariates$ppt[,t], 
                                                                                                                                                                                tmax = covariates$tmax[,t],
                                                                                                                                                                                SICOND = covariates$SICOND[,t]))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-x.mat[,paste0("x[", m,",", jags.new$data$nt,"]")]
      
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = data.frame(SDI = covariates$SDI[,t], 
                                                                                                                                             ppt = covariates$ppt[,t], 
                                                                                                                                             tmax = covariates$tmax[,t],
                                                                                                                                             SICOND = covariates$SICOND[,t]))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-forecast[,t-1]
      
    }  }
  varianceIC_Parameters_past <- apply(na.omit(forecast),2,function(x){var(x, na.rm = TRUE)})
  forecast.ic.param.past <- apply(na.omit(forecast), 2,function(x){quantile(x, c(0.025, 0.25, 0.5, 0.975), na.rm = TRUE)})
  
  var.inc.IC_Parameters_past <- apply(na.omit(inc),2,function(x){var(x, na.rm = TRUE)})
  inc.ic.param.past <- apply(na.omit(inc), 2,function(x){quantile(x, c(0.025, 0.25, 0.5, 0.975), na.rm = TRUE)})
  
 #---------------------------------------------------------------------------
 # model with no climate change and no change in DBH
 #---------------------------------------------------------------------------
  time_steps <- 82
  nMCMC <- length(x.mat[,paste0("x[1,",jags.new$data$nt,"]")])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.no.change.dbh(x = x.mat[,paste0("x[", m,",", jags.new$data$nt,"]")],x.dbh =  x.mat[,paste0("x[", m,",", jags.new$data$nt,"]")],m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = data.frame(SDI = covariates$SDI[,t], 
                                                                                                                                                                                ppt = covariates$ppt[,t], 
                                                                                                                                                                                tmax = covariates$tmax[,t],
                                                                                                                                                                                SICOND = covariates$SICOND[,t]))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-x.mat[,paste0("x[", m,",", jags.new$data$nt,"]")]
      
    }else{
      dbh.pred <- iterate_statespace.no.change.dbh(x = forecast[,t-1],x.dbh =  x.mat[,paste0("x[", m,",", jags.new$data$nt,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = data.frame(SDI = covariates$SDI[,t], 
                                                                                                                                             ppt = covariates$ppt[,t], 
                                                                                                                                             tmax = covariates$tmax[,t],
                                                                                                                                             SICOND = covariates$SICOND[,t]))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-forecast[,t-1]
      
    }  }
  varianceIC_Parameters_past.nodbh <- apply(na.omit(forecast),2,function(x){var(x, na.rm = TRUE)})
  forecast.ic.param.past.nodbh <- apply(na.omit(forecast), 2,function(x){quantile(x, c(0.025, 0.25, 0.5, 0.975), na.rm = TRUE)})
  
  var.inc.IC_Parameters_past.nodbh <- apply(na.omit(inc),2,function(x){var(x, na.rm = TRUE)})
  inc.ic.param.past.nodbh <- apply(na.omit(inc), 2,function(x){quantile(x, c(0.025, 0.25, 0.5, 0.975), na.rm = TRUE)})
  
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from Initial conditions AND parameters uncertainty AND future Driver uncertainty
  #---------------------------------------------------------------------------
  
  # use all of the parameter MCMCS:
  
  # 
  # just use the ensemble means (w.out driver uncertainty)
  ppt <- tmax <- SDI <- SICOND <-matrix(NA, nrow =length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])), ncol = 82 )
  for(i in 1:82){
    
    ppt[,i]<- rnorm(n = length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])) , mean = proj.ordered[i,]$mean.ppt, sd = proj.ordered[i,]$SD.ppt)
    tmax[,i]<- rnorm(n = length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])) , mean = proj.ordered[i,]$mean.tmax.fs, sd = proj.ordered[i,]$SD.tmax)
    SDI[,i]<- rep(cov.data[m, ]$SDI, length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])))
    SICOND[,i]<- rep(cov.data[m, ]$SICOND, length((length(betas[,"betaSDI"])-3299) : length(betas[,"betaSDI"])))
  }
  
 
  covariates <- list()
  covariates$SDI <- SDI
  covariates$ppt <- ppt
  covariates$tmax <- tmax
  covariates$SICOND <- SICOND
  
  #covariates <- list(SDI, ppt, tmax)
  
  
  
  time_steps <- 82
  nMCMC <- length(x.mat[,paste0("x[1,",jags.new$data$nt,"]")])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", jags.new$data$nt,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = data.frame(SDI = covariates$SDI[,t], 
                                                                                                                                                                                         ppt = covariates$ppt[,t], 
                                                                                                                                                                                         tmax = covariates$tmax[,t],
                                                                                                                                                                                         SICOND = covariates$SICOND[,t]))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-x.mat[,paste0("x[", m,",", jags.new$data$nt,"]")]
      
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = data.frame(SDI = covariates$SDI[,t], 
                                                                                                                                             ppt = covariates$ppt[,t], 
                                                                                                                                             tmax = covariates$tmax[,t],
                                                                                                                                             SICOND = covariates$SICOND[,t]))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-forecast[,t-1]
      
    }  }
  varianceIC_Parameters_driver <- apply(na.omit(forecast),2,function(x){var(x, na.rm = TRUE)})
  forecast.ic.param.d <- apply(na.omit(forecast), 2,function(x){quantile(x, c(0.025, 0.25, 0.5, 0.975), na.rm = TRUE)})
  
  var.inc.IC_Parameters_driver <- apply(na.omit(inc),2,function(x){var(x, na.rm = TRUE)})
  inc.ic.param.d <- apply(na.omit(inc), 2,function(x){quantile(x, c(0.025, 0.25, 0.5, 0.975), na.rm = TRUE)})
  
  

  #---------------------------------------------------------------------------
  ##  Uncertainty from Initial conditions AND parameters uncertainty AND future Driver uncertainty
  # but we dont include effects of tree size--set x == xmat at year = 2018
  #---------------------------------------------------------------------------
  
  
  
  time_steps <- 82
  nMCMC <- length(x.mat[,paste0("x[1,",jags.new$data$nt,"]")])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.no.change.dbh(x = x.mat[,paste0("x[", m,",", jags.new$data$nt,"]")], x.dbh = x.mat[,paste0("x[", m,",", jags.new$data$nt,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = data.frame(SDI = covariates$SDI[,t], 
                                                                                                                                                                                ppt = covariates$ppt[,t], 
                                                                                                                                                                                tmax = covariates$tmax[,t],
                                                                                                                                                                                SICOND = covariates$SICOND[,t]))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-x.mat[,paste0("x[", m,",", jags.new$data$nt,"]")]
      
    }else{
      dbh.pred <- iterate_statespace.no.change.dbh(x = forecast[,t-1],x.dbh = x.mat[,paste0("x[", m,",", jags.new$data$nt,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = data.frame(SDI = covariates$SDI[,t], 
                                                                                                                                             ppt = covariates$ppt[,t], 
                                                                                                                                             tmax = covariates$tmax[,t],
                                                                                                                                             SICOND = covariates$SICOND[,t]))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-forecast[,t-1]
      
    }  }
  varianceIC_Parameters_driver_nodbh <- apply(na.omit(forecast),2,function(x){var(x, na.rm = TRUE)})
  forecast.ic.param.d_nodbh <- apply(na.omit(forecast), 2,function(x){quantile(x, c(0.025, 0.25, 0.5, 0.975), na.rm = TRUE)})
  
  var.inc.IC_Parameters_driver_nodbh <- apply(na.omit(inc),2,function(x){var(x, na.rm = TRUE)})
  inc.ic.param.d_nodbh <- apply(na.omit(inc), 2, function(x){quantile(x, c(0.025, 0.25, 0.5, 0.975), na.rm = TRUE)})
  
  
  # #---------------------------------------------------------------------------
  # ##  Uncertainty from IC AND parameters uncertainty AND process error
  # #---------------------------------------------------------------------------
  
  # combine variances:
  if(type == "dbh"){
    V.pred.sim     <- rbind(varianceIC_Parameters_past,varianceIC_Parameters_driver, varianceIC_Parameters_driver_nodbh, varianceIC_Parameters_past.nodbh)
    
    # combine forecasts:
    pred.sims     <- data.frame(IPP.0 =  forecast.ic.param.past[1,],
                                IPPnodbh.0 =  forecast.ic.param.past.nodbh[1,],
                                IPD.0 = forecast.ic.param.d[1,],
                                IPA.0 =  forecast.ic.param.d_nodbh[1,],
                                IPP.50 =  forecast.ic.param.past[3,],
                                IPPnodbh.50 =  forecast.ic.param.past.nodbh[3,],
                                IPD.50 =  forecast.ic.param.d[3,],
                                IPA.50 =  forecast.ic.param.d_nodbh[3,],
                                IPP.100 = forecast.ic.param.past[4,],
                                IPPnodbh.100 =  forecast.ic.param.past.nodbh[4,],
                                IPD.100 = forecast.ic.param.d[4,],
                                IPA.100 = forecast.ic.param.d_nodbh[4,],
                                year = 2018:2099)
    
    axis.name <- "Diameter"
    
  }else{
    V.pred.sim.inc     <- rbind(var.inc.IC_Parameters_past, var.inc.IC_Parameters_driver, var.inc.IC_Parameters_driver_nodbh, var.inc.IC_Parameters_past.nodbh)
    
    pred.sims.inc     <- data.frame(IPP.0 = inc.ic.param.past[1,],
                                IPPnodbh.0 = inc.ic.param.past.nodbh[1,],
                                IPD.0 = inc.ic.param.d[1,],
                                IPA.0 =  inc.ic.param.d_nodbh[1,],
                                IPP.50 = inc.ic.param.past[3,],
                                IPPnodbh.50 = inc.ic.param.past.nodbh[3,],
                                IPD.50 = inc.ic.param.d[3,],
                                IPA.50 =  inc.ic.param.d_nodbh[3,],
                                IPP.100 = inc.ic.param.past[4,],
                                IPPnodbh.100 = inc.ic.param.past.nodbh[4,],
                                IPD.100 = inc.ic.param.d[4,],
                                IPA.100 = inc.ic.param.d_nodbh[4,],
                                year = 2018:2099)
    axis.name.inc <- "Increment"
  }
  V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
  
  
  pred.sims.m <- reshape2::melt(pred.sims, id.vars = "year")
  pred.sims.class <- pred.sims.m %>% separate(col = variable, sep = "[.]", into = c("unc","lo")) %>%
    spread(key = lo, value = value)
  
  if("uncertainty" %in% colnames(pred.sims.class)){
  pred.sims.class$Forecast.type <- ifelse(pred.sims.class$uncertainty %in% "IPA", "Climate change only",
                                          ifelse(pred.sims.class$uncertainty %in% "IPP","No Climate Change + DBH change",
                                                 ifelse(pred.sims.class$uncertainty %in% "IPPnodbh","No Climate Change + no DBH change","Climate change + DBH change")))
  }else{
    pred.sims.class$Forecast.type <- ifelse(pred.sims.class$unc %in% "IPA", "Climate change only",
                                            ifelse(pred.sims.class$unc %in% "IPP","No Climate Change + DBH change",
                                                   ifelse(pred.sims.class$unc %in% "IPPnodbh","No Climate Change + no DBH change","Climate change + DBH change")))
    
  }
  colnames(pred.sims.class) <- c("year", "uncertainty", "Low", "High", "Median","Forecast.type")
  my_cols <- c("#1b9e77",
               "#d95f02",
               "black",
               "#7570b3", 
               "grey")
  
  pred.sims.class$uncertainty <- factor(pred.sims.class$uncertainty, levels = c("IPP","IPD","IPA"))
  
  
  predY_plot <- ggplot(data = pred.sims.class, aes(x=year, fill = Forecast.type))+
    geom_ribbon(aes(ymin=Low, ymax=High), color = "grey", alpha = 0.5)+geom_line(aes(x = year, y = Median, color = Forecast.type))+
    ylab(axis.name)+
    xlab("Year")+theme_bw()+
    scale_fill_manual(values = my_cols, name = NULL)+ scale_color_manual(values = my_cols, name = NULL)+ theme( panel.grid = element_blank())#+facet_wrap(~Forecast.type, ncol = 1, scales = "free_y")
  
  predY_plot 
  
  # make predictions plot for tree ring increment:
  V.pred.sim.rel.inc <- apply(V.pred.sim.inc,2,function(x) {x/max(x)})
  
  
  pred.sims.m.inc <- reshape2::melt(pred.sims.inc, id.vars = "year")
  pred.sims.class.inc <- pred.sims.m.inc %>% separate(col = variable, sep = "[.]", into = c("unc","lo")) %>%
    spread(key = lo, value = value)
  
 
    pred.sims.class.inc$Forecast.type <- ifelse(pred.sims.class.inc$unc %in% "IPA", "Climate change only",
                                            ifelse(pred.sims.class.inc$unc %in% "IPP","No Climate Change + DBH change",
                                                   ifelse(pred.sims.class.inc$unc %in% "IPPnodbh","No Climate Change + no DBH change","Climate change + DBH change")))
    
  
  colnames(pred.sims.class.inc) <- c("year", "uncertainty", "Low", "High", "Median","Forecast.type")
  my_cols <- c("#1b9e77",
               "#d95f02",
               "black",
               "#7570b3", 
               "grey")
  
  
  
  predYinc_plot <- ggplot(data = pred.sims.class.inc, aes(x=year, fill = Forecast.type))+
    geom_ribbon(aes(ymin=Low, ymax=High), color = "grey", alpha = 0.5)+geom_line(aes(x = year, y = Median, color = Forecast.type))+
    ylab(axis.name.inc)+geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey" )+
    xlab("Year")+theme_bw()+
    scale_fill_manual(values = my_cols, name = NULL)+ scale_color_manual(values = my_cols, name = NULL)+ theme( panel.grid = element_blank())#+facet_wrap(~Forecast.type, ncol = 1, scales = "free_y")
  
  predYinc_plot 
  
  
  legend.future.scenario <- get_legend(predYinc_plot)
  # make plots for both tree ring increment and tree diameter:
  
 #png(height = 5.5, width = 8, units = "in", res = 300, paste0("scenarios/tree_", m,"_climate_dbh_changes_inc_dbh.png"))
  p1 <- cowplot::plot_grid(plot_grid(predYinc_plot +theme(legend.position = "none"), 
                     predY_plot + theme(legend.position = "none"), labels = "AUTO", ncol = 2, align = "hv"),
                     legend.future.scenario, ncol = 1, rel_heights = c(1, 0.25))
 save_plot(filename = paste0("scenarios/tree_", m,"_",scenario,"_climate_dbh_changes_inc_dbh.png"), plot = p1, base_height = 5.5, base_width = 8)
  
  # ##--------------------------------------------------------------
  # #  Plot the difference
  # #--------------------------------------------------------------
  # var_rel_preds <- as.data.frame(t(V.pred.sim.rel*100))
  # var_rel_preds$x <- 1:nrow(var_rel_preds)
  # 
  # 
  # # pretty colored plot:
  # 
  # my_cols <- c("#1b9e77",
  #              "#d95f02",
  #              "black",
  #              "#7570b3", 
  #              "grey")
  # tmpvar <- var_rel_preds
  # tmpvar$year <- 2018:2099
  # colnames(tmpvar) <- c( "Climate Change + DBH change","Climate change only", "x", "year")
  # variance.df <- tmpvar %>%
  #   gather(simtype, variance, -x, -year)
  # 
  # variance.df$simtype <- factor(variance.df$simtype, levels = c("Climate Change + DBH change","Climate change only"))
  # 
  # prop.var <- ggplot(variance.df, aes(x=year, fill = simtype))+
  #   geom_ribbon(aes(ymin=0, ymax=variance), color = "grey")+
  #   ylab(paste("% of total variance for ", axis.name))+    xlab("Year")+
  #   scale_fill_manual(values = my_cols, name = NULL)+#, 
  #   #labels = c("Process error", "Driver uncertainty",  "Parameter uncertainty","alpha uncertainty", "Initial conditions"))+
  #   scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
  #                      expand = c(0, 0))+
  #   theme_bw()+theme(panel.grid = element_blank())
  # 
  write.csv(pred.sims.class.inc, paste0("scenarios/tree_", m,"_", axis.name.inc, "_", scenario,"_",output.base.name,"_scenario_preds", ".csv"))
  write.csv(pred.sims.class, paste0("scenarios/tree_", m,"_", axis.name, "_", scenario,"_",output.base.name,"_scenario_preds", ".csv"))
  }
  
}
plot.future.forecast.clim.dbh.scenario(m = 43,  scenario = "rcp26",  print = TRUE)
plot.future.forecast.clim.dbh.scenario(m = 43,  scenario = "rcp45",  print = TRUE)
plot.future.forecast.clim.dbh.scenario(m = 43,  scenario = "rcp60",  print = TRUE)
plot.future.forecast.clim.dbh.scenario(m = 43,  scenario = "rcp85",  print = TRUE)


plot.future.forecast.clim.dbh.scenario(m = 1,  scenario = "rcp26",  print = TRUE)
plot.future.forecast.clim.dbh.scenario(m = 1,  scenario = "rcp45",  print = TRUE)
plot.future.forecast.clim.dbh.scenario(m = 1,  scenario = "rcp60",  print = TRUE)
plot.future.forecast.clim.dbh.scenario(m = 1,  scenario = "rcp85",  print = TRUE)
treeds <- 1:515
lapply(treeds, function(x){plot.future.forecast.clim.dbh.scenario(m = x, scenario = "rcp26")})
lapply(treeds, function(x){plot.future.forecast.clim.dbh.scenario(m = x, scenario = "rcp45")})
lapply(treeds, function(x){plot.future.forecast.clim.dbh.scenario(m = x, scenario = "rcp60")})
lapply(treeds, function(x){plot.future.forecast.clim.dbh.scenario(m = x, scenario = "rcp85")})



#---------------------------------------------------------------------------------------------
# read in all and find the difference between each scenario
#---------------------------------------------------------------------------------------------
increment.projs <- read.csv("scenarios/tree_1_Increment_rcp26_SDI_SI.norand.X.nadapt.5000_scenario_preds.csv")

low.est <- increment.projs %>% select( year, Low, Forecast.type)%>% group_by(year) %>% spread( Forecast.type, Low, drop = TRUE)
median.est <- increment.projs %>% select( year, Median, Forecast.type)%>% group_by(year) %>% spread( Forecast.type, Median, drop = TRUE)
high.est <- increment.projs %>% select( year, High, Forecast.type)%>% group_by(year) %>% spread( Forecast.type, High, drop = TRUE)

low.est$DBH.change.diff <- low.est$`No Climate Change + DBH change`- low.est$`No Climate Change + no DBH change`  
low.est$climate.change.diff <- low.est$`Climate change only`- low.est$`No Climate Change + no DBH change`  
low.est$climate.change.DBH.change.diff <- low.est$`Climate change + DBH change`- low.est$`No Climate Change + no DBH change`  

median.est$DBH.change.diff <- median.est$`No Climate Change + DBH change`- median.est$`No Climate Change + no DBH change`  
median.est$climate.change.diff <- median.est$`Climate change only`- median.est$`No Climate Change + no DBH change`  
median.est$climate.change.DBH.change.diff <- median.est$`Climate change + DBH change`- median.est$`No Climate Change + no DBH change`  

high.est$DBH.change.diff <- high.est$`No Climate Change + DBH change`- high.est$`No Climate Change + no DBH change`  
high.est$climate.change.diff <- high.est$`Climate change only`- high.est$`No Climate Change + no DBH change`  
high.est$climate.change.DBH.change.diff <- high.est$`Climate change + DBH change`- high.est$`No Climate Change + no DBH change`  

low.est.df <- low.est[,c("year", "DBH.change.diff", "climate.change.diff", "climate.change.DBH.change.diff")]
median.est.df <- median.est[,c("year", "DBH.change.diff", "climate.change.diff", "climate.change.DBH.change.diff")]
high.est.df <- high.est[,c("year", "DBH.change.diff", "climate.change.diff", "climate.change.DBH.change.diff")]

low.est.df$CI <- "low"
median.est.df$CI <- "median"
high.est.df$CI <- "high"

low.est.df.m <- reshape2::melt(low.est.df, id.vars = c("CI", "year"))
median.est.df.m <- reshape2::melt(median.est.df, id.vars = c("CI", "year"))
high.est.df.m <- reshape2::melt(high.est.df, id.vars = c("CI", "year"))

diff.df <- rbind(low.est.df.m, median.est.df.m, high.est.df.m)

diff.spread <- diff.df%>% spread(value = value, key = CI, drop = TRUE)


my_cols <- c("#1b9e77",
             "#d95f02",
             "black",
             "#7570b3", 
             "grey")


diff_plot <- ggplot(data = diff.spread, aes(x=year, fill = variable))+
  geom_ribbon(aes(ymin=low, ymax=high), color = "grey", alpha = 0.5)+geom_line(aes(x = year, y = median, color = variable))+
  ylab("Change in Increment")+
  xlab("Year")+theme_bw()+
  scale_fill_manual(values = my_cols, name = NULL)+ scale_color_manual(values = my_cols, name = NULL)+ theme( panel.grid = element_blank())#+facet_wrap(~Forecast.type, ncol = 1, scales = "free_y")

diff_plot 
