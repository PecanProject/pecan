# script to partition out uncertainty in future forecasts:

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

# lets get the average climate for the 2020's:
clim.na <- readRDS(gzcon(url("https://de.cyverse.org/dl/d/134F7273-EDDC-48E1-BFD6-1632502F61FE/pipo.cores.with.8.5.2080s.climate.NA.rds")))

clim.na$cov.data$MAP.2020s.stand <-(clim.na$cov.data$MAP.2020s-mean(as.matrix(clim.data$wintP.wateryr)))/sd(as.matrix(clim.data$wintP.wateryr))
clim.na$cov.data$tmax.2020s.stand <-(clim.na$cov.data$tmax.2020s-mean(as.matrix(clim.data$tmax.fallspr)))/sd(as.matrix(clim.data$tmax.fallspr))

# for rcp 8.5:
clim.na$cov.data$MAP.85.2020s.stand <-(clim.na$cov.data$MAP.85.2020s-mean(as.matrix(clim.data$wintP.wateryr)))/sd(as.matrix(clim.data$wintP.wateryr))
clim.na$cov.data$tmax.85.2020s.stand <-(clim.na$cov.data$tmax85.2020s-mean(as.matrix(clim.data$tmax.fallspr)))/sd(as.matrix(clim.data$tmax.fallspr))


cov.data$MAP.85.2020s.stand <- clim.na$cov.data$MAP.85.2020s.stand 
cov.data$tmax.85.2020s.stand <-clim.na$cov.data$tmax.85.2020s.stand 

#-----------------------------------------------------------------------
# Partitioning uncertainty
#-----------------------------------------------------------------------
# Create a function for our process model (linear state space model)





plot.prop.variance <- function(m){
  #-----------------------------------------------------------------------
  # Partitioning uncertainty
  #-----------------------------------------------------------------------
  # Create a function for our process model (linear state space model)
  
  iterate_statespace <- function( x = x.mat[,"x[1,45]"], m = 1, betas.all, alpha, SDinc, covariates) {
    
    j <- 1
    
    # alpha <- alphas[j, alphaplotid] 
    # bSDI <- betas[j,"betaSDI"]
    # bSDI_ppt <- betas[j,"betaSDI_wintP.wateryr"]
    # bX <-  betas[j,"betaX"]
    # bX2 <- betas[j,"betaX2"]
    # bX_SDI <- betas[j,"betaX_SDI"]
    # bX_ppt <- betas[j,"betaX_wintP.wateryr"]
    # bppt <- betas[j,"betawintP.wateryr"]
    # btmax <- betas[j,"betatmax.fallspr"] 
    # bX_tmax <- betas[j,"betaX_tmax.fallspr"]
    # bSDI_tmax <- betas[j,"betaSDI_tmax.fallspr"]
    # btmax_ppt <- betas[j,"betatmax.fallspr_wintP.wateryr"]
    # 
    
    # pseudocode for now
    tree.growth <- alpha + betas.all$b0 + 
      betas.all$bSDI*covariates$SDI + 
      betas.all$bSDI_ppt*covariates$SDI*covariates$ppt + 
      betas.all$bX*(x-30) + 
      betas.all$bX2*(x-30)*(x-30) + 
      betas.all$bX_SDI*(x-30)*covariates$SDI + 
      betas.all$bX_ppt*covariates$ppt*(x-30) + 
      betas.all$bppt*covariates$ppt + 
      betas.all$btmax*covariates$tmax + 
      betas.all$bX_tmax*(x-30)*covariates$tmax + 
      betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
      betas.all$btmax_ppt*covariates$tmax*covariates$ppt 
    #tree.growth
    
    
    # Stochastic process model
    ypred <- rnorm(length(tree.growth), tree.growth, SDinc) 
    
    ypred
  }
  alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
  
  alpha <- quantile(alphas[, alphaplotid],0.5)
  bSDI <- quantile(betas[,"betaSDI"],0.5)
  bSDI_ppt <- quantile(betas[,"betaSDI_wintP.wateryr"],0.5)
  bX <-  quantile(betas[,"betaX"],0.5)
  bX2 <- quantile(betas[,"betaX2"],0.5)
  bX_SDI <- quantile(betas[,"betaX_SDI"],0.5)
  bX_ppt <- quantile(betas[,"betaX_wintP.wateryr"],0.5)
  bppt <- quantile(betas[,"betawintP.wateryr"],0.5)
  btmax <- quantile(betas[,"betatmax.fallspr"] ,0.5)
  bX_tmax <- quantile(betas[,"betaX_tmax.fallspr"],0.5)
  bSDI_tmax <- quantile(betas[,"betaSDI_tmax.fallspr"],0.5)
  btmax_ppt <- quantile(betas[,"betatmax.fallspr_wintP.wateryr"],0.5)
  b0 <- quantile(B0, 0.5)
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  #x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  
  
  
  
  test <- iterate_statespace(x = x.mat[,"x[1,45]"], m = 1, betas.all = betas.all, alpha, SDinc = 0, covariates[1,])
  #---------------------------------------------------------------------------
  ##  Initial condition uncertainty: make forecasts from all MCMC iterations of
  ##    the final year, but use mean parameter values and no process error.
  time_steps <- 45
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  
  # just for 1 tree
  for(t in 1:time_steps){
    inc.pred <- iterate_statespace(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDinc = 0, covariates = covariates[t,])
    forecast[,t] <- inc.pred
  }
  varianceIC <- apply(forecast,2,var) # get variance from IC
  forecast.ic <- apply(forecast, 2, quantile)
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from Initial conditions AND parameters uncertainty
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  alpha <- alphas[57001:60300, alphaplotid]
  bSDI <- betas[57001:60300,"betaSDI"]
  bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
  bX <-  betas[57001:60300,"betaX"]
  bX2 <- betas[57001:60300,"betaX2"]
  bX_SDI <- betas[57001:60300,"betaX_SDI"]
  bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
  bppt <- betas[57001:60300,"betawintP.wateryr"]
  btmax <- betas[57001:60300,"betatmax.fallspr"]
  bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
  bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
  btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
  b0 <- B0[57001:60300]
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  
  
  
  time_steps <- 45
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  for(t in 1:time_steps){
    inc.pred <- iterate_statespace(x = x.mat[,paste0("x[", m,",", t,"]")], m = 1, betas.all = betas.all, alpha = alpha, SDinc = 0, covariates = covariates[t,])
    forecast[,t] <- inc.pred
  }
  varianceIC_Parameters <- apply(forecast,2,var)
  forecast.ic.param <- apply(forecast, 2, quantile)
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from IC AND parameters uncertainty AND process error
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  alpha <- alphas[57001:60300, alphaplotid]
  bSDI <- betas[57001:60300,"betaSDI"]
  bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
  bX <-  betas[57001:60300,"betaX"]
  bX2 <- betas[57001:60300,"betaX2"]
  bX_SDI <- betas[57001:60300,"betaX_SDI"]
  bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
  bppt <- betas[57001:60300,"betawintP.wateryr"]
  btmax <- betas[57001:60300,"betatmax.fallspr"]
  bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
  bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
  btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
  b0 <- B0[57001:60300]
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  Sdev <- 1/quantile(out[,"tau_inc"], 0.5)
  
  
  for(t in 1:time_steps){
    inc.pred <- iterate_statespace(x = x.mat[,paste0("x[",m,",", t,"]")], m = m, betas.all = betas.all, alpha, SDinc = Sdev, covariates[t,])
    forecast[,t] <- inc.pred
  }
  varianceIC_Parameters_process <- apply(forecast,2,var)
  forecast.ic.param.process <- apply(forecast, 2, quantile)
  
  
  # combine variances:
  V.pred.sim     <- rbind(varianceIC_Parameters_process,varianceIC_Parameters,varianceIC)
  V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
  
  # combine forecasts:
  pred.sims     <- data.frame(IPP.0 = forecast.ic.param.process[1,],
                              IP.0 =forecast.ic.param[1,],
                              I.0 =forecast.ic[1,],
                              IPP.100= forecast.ic.param.process[5,],
                              IP.100=forecast.ic.param[5,],
                              I.100=forecast.ic[5,],
                              year = 1:45)
  my_cols <- c("#1b9e77",
               "#d95f02",
               "#7570b3")
  
  predY_plot <- ggplot(data=pred.sims, aes(x=year))+
    geom_ribbon(aes(ymin=IPP.0, ymax=IPP.100), fill=my_cols[3])+
    geom_ribbon(aes(ymin=IP.0, ymax=IP.100), fill=my_cols[2])+
    geom_ribbon(aes(ymin=I.0, ymax=I.100), fill=my_cols[1])+
    ylab("Predicted Tree Growth")+
    xlab("Year")+
    #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
    #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
    #scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
    theme_bw()
  
  # pred.sim.0rel <- apply(pred.sim.0,2,function(x) {x/max(x)})
  # 
  # pred.sim.75     <- rbind(forecast.ic.param.process[5,],
  #                          forecast.ic.param[5,],
  #                          forecast.ic[5,])
  # pred.sim.75rel <- apply(pred.sim.75,2,function(x) {x/max(x)})
  # 
  # plot(1:45, pred.sim.0[1,], type = "l")
  # lines(1:45, pred.sim.75[1,], col= "red")
  ####
  ####  PLOT THE FORECASTING UNCERTAINTY PARTITION -------------------------------
  ####
  var_rel_preds <- as.data.frame(t(V.pred.sim.rel*100))
  var_rel_preds$x <- 1:nrow(var_rel_preds)
  #my_cols <- c("black", "grey55", "grey70")
  my_cols <- c("#1b9e77",
               "#d95f02",
               "#7570b3")
  variance_plot <- ggplot(data=var_rel_preds, aes(x=x))+
    geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters_process), fill=my_cols[3])+
    geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters), fill=my_cols[2])+
    geom_ribbon(aes(ymin=0, ymax=varianceIC), fill=my_cols[1])+
    ylab("Percent of uncertainty")+
    xlab("Year")+
    #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
    #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
    scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
    theme_bw()
  
  variance_plot 
  
  # now do this for all of the trees:
  
  
  
  
  ##  For presentations
  
  my_cols <- c("#1b9e77",
               "#d95f02",
               "#7570b3")
  tmpvar <- var_rel_preds
  colnames(tmpvar) <- c( "BvarIPD", "CvarIP", "DvarI", "x")
  var2 <- tmpvar %>%
    gather(simtype, variance, -x)
  
  ggplot(var2, aes(x=x, fill = simtype))+
    geom_ribbon(aes(ymin=0, ymax=variance), color = "black")+
    ylab("Percentage of total variance (%)")+
    xlab("Year")+
    scale_fill_manual(values = my_cols, name = NULL, 
                      labels = c("Process error", "Parameter uncertainty", "Initial conditions"))+
    scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                       expand = c(0, 0))+
    theme_bw()
  
  
}
plot.prop.variance (m=515)


plot.increment.variance <- function(m){
  #-----------------------------------------------------------------------
  # Partitioning uncertainty
  #-----------------------------------------------------------------------
  # Create a function for our process model (linear state space model)
  
  iterate_statespace <- function( x = x.mat[,"x[1,45]"], m = m, betas.all, alpha, SDinc, covariates) {
    
    j <- 1
    
    # alpha <- alphas[j, alphaplotid] 
    # bSDI <- betas[j,"betaSDI"]
    # bSDI_ppt <- betas[j,"betaSDI_wintP.wateryr"]
    # bX <-  betas[j,"betaX"]
    # bX2 <- betas[j,"betaX2"]
    # bX_SDI <- betas[j,"betaX_SDI"]
    # bX_ppt <- betas[j,"betaX_wintP.wateryr"]
    # bppt <- betas[j,"betawintP.wateryr"]
    # btmax <- betas[j,"betatmax.fallspr"] 
    # bX_tmax <- betas[j,"betaX_tmax.fallspr"]
    # bSDI_tmax <- betas[j,"betaSDI_tmax.fallspr"]
    # btmax_ppt <- betas[j,"betatmax.fallspr_wintP.wateryr"]
    # 
    
    # pseudocode for now
    tree.growth <- alpha + betas.all$b0 + 
      betas.all$bSDI*covariates$SDI + 
      betas.all$bSDI_ppt*covariates$SDI*covariates$ppt + 
      betas.all$bX*(x-30) + 
      betas.all$bX2*(x-30)*(x-30) + 
      betas.all$bX_SDI*(x-30)*covariates$SDI + 
      betas.all$bX_ppt*covariates$ppt*(x-30) + 
      betas.all$bppt*covariates$ppt + 
      betas.all$btmax*covariates$tmax + 
      betas.all$bX_tmax*(x-30)*covariates$tmax + 
      betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
      betas.all$btmax_ppt*covariates$tmax*covariates$ppt 
    #tree.growth
    
    
    # Stochastic process model
    ypred <- rnorm(length(tree.growth), tree.growth, SDinc) 
    
    ypred
  }
  alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
  
  alpha <- quantile(alphas[, alphaplotid],0.5)
  bSDI <- quantile(betas[,"betaSDI"],0.5)
  bSDI_ppt <- quantile(betas[,"betaSDI_wintP.wateryr"],0.5)
  bX <-  quantile(betas[,"betaX"],0.5)
  bX2 <- quantile(betas[,"betaX2"],0.5)
  bX_SDI <- quantile(betas[,"betaX_SDI"],0.5)
  bX_ppt <- quantile(betas[,"betaX_wintP.wateryr"],0.5)
  bppt <- quantile(betas[,"betawintP.wateryr"],0.5)
  btmax <- quantile(betas[,"betatmax.fallspr"] ,0.5)
  bX_tmax <- quantile(betas[,"betaX_tmax.fallspr"],0.5)
  bSDI_tmax <- quantile(betas[,"betaSDI_tmax.fallspr"],0.5)
  btmax_ppt <- quantile(betas[,"betatmax.fallspr_wintP.wateryr"],0.5)
  b0 <- quantile(B0, 0.5)
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  #x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)

  
  
  
  
 # test <- iterate_statespace(x = x.mat[,"x[1,45]"], m = 1, betas.all = betas.all, alpha, SDinc = 0, covariates[1,])
  #---------------------------------------------------------------------------
  ##  Initial condition uncertainty: make forecasts from all MCMC iterations of
  ##    the final year, but use mean parameter values and no process error.
  time_steps <- 45
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  
  # just for 1 tree
  for(t in 1:time_steps){
    inc.pred <- iterate_statespace(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDinc = 0, covariates = covariates[t,])
    forecast[,t] <- inc.pred
  }
  varianceIC <- apply(forecast,2,var) # get variance from IC
  forecast.ic <- apply(forecast, 2, quantile)
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from Initial conditions AND parameters uncertainty
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  alpha <- alphas[57001:60300, alphaplotid]
  bSDI <- betas[57001:60300,"betaSDI"]
  bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
  bX <-  betas[57001:60300,"betaX"]
  bX2 <- betas[57001:60300,"betaX2"]
  bX_SDI <- betas[57001:60300,"betaX_SDI"]
  bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
  bppt <- betas[57001:60300,"betawintP.wateryr"]
  btmax <- betas[57001:60300,"betatmax.fallspr"]
  bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
  bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
  btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
  b0 <- B0[57001:60300]
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  
  
  
  time_steps <- 45
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  for(t in 1:time_steps){
    inc.pred <- iterate_statespace(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDinc = 0, covariates = covariates[t,])
    forecast[,t] <- inc.pred
  }
  varianceIC_Parameters <- apply(forecast,2,var)
  forecast.ic.param <- apply(forecast, 2, quantile)
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from IC AND parameters uncertainty AND process error
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  alpha <- alphas[57001:60300, alphaplotid]
  bSDI <- betas[57001:60300,"betaSDI"]
  bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
  bX <-  betas[57001:60300,"betaX"]
  bX2 <- betas[57001:60300,"betaX2"]
  bX_SDI <- betas[57001:60300,"betaX_SDI"]
  bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
  bppt <- betas[57001:60300,"betawintP.wateryr"]
  btmax <- betas[57001:60300,"betatmax.fallspr"]
  bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
  bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
  btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
  b0 <- B0[57001:60300]
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  Sdev <- 1/quantile(out[,"tau_inc"], 0.5)
  
  
  for(t in 1:time_steps){
    inc.pred <- iterate_statespace(x = x.mat[,paste0("x[",m,",", t,"]")], m = m, betas.all = betas.all, alpha, SDinc = Sdev, covariates[t,])
    forecast[,t] <- inc.pred
  }
  varianceIC_Parameters_process <- apply(forecast,2,var)
  forecast.ic.param.process <- apply(forecast, 2, quantile)
  
  
  # combine variances:
  V.pred.sim     <- rbind(varianceIC_Parameters_process,varianceIC_Parameters,varianceIC)
  V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
  
  # combine forecasts:
  pred.sims     <- data.frame(IPP.0 = forecast.ic.param.process[1,],
                              IP.0 =forecast.ic.param[1,],
                              I.0 =forecast.ic[1,],
                              IPP.100= forecast.ic.param.process[5,],
                              IP.100=forecast.ic.param[5,],
                              I.100=forecast.ic[5,],
                              year = 1:45)
  pred.sims.m <- reshape2::melt(pred.sims, id.vars = "year")
  pred.sims.m$simtype <- ifelse(pred.sims.m$variable %in% c("IPP.0", "IPP.100"), "IPP", 
                                ifelse(pred.sims.m$variable %in% c("IP.0", "IP.100"), "IP", "I"))
                                      
  pred.sims.m$cat <- ifelse(pred.sims.m$variable %in% c("IPP.0", "IP.0", "I.0"), "lo", "hi")
  pred.sims.by <- pred.sims.m %>% select(-variable) %>% group_by(year, simtype) %>% spread(key = cat, value = value)
  
  my_cols <- c("#1b9e77",
               "#d95f02",
               "#7570b3")
  pred.sims.by$simtype <- factor(pred.sims.by$simtype, levels = c(  "IPP","IP", "I") )
  
  predY_plot <- ggplot(data=pred.sims.by, aes(x=year))+
    geom_ribbon(data=pred.sims.by, aes(ymin=lo, ymax=hi, fill=simtype))+
       scale_fill_manual(values = my_cols, name = NULL, 
                         labels = c("Process error", "Parameter uncertainty", "Initial conditions"))+
    ylab("Predicted Tree Growth")+
    xlab("Year")+
    #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
    #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
    #scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
    theme_bw()
  predY_plot
  
  # pred.sim.0rel <- apply(pred.sim.0,2,function(x) {x/max(x)})
  # 
  # pred.sim.75     <- rbind(forecast.ic.param.process[5,],
  #                          forecast.ic.param[5,],
  #                          forecast.ic[5,])
  # pred.sim.75rel <- apply(pred.sim.75,2,function(x) {x/max(x)})
  # 
  # plot(1:45, pred.sim.0[1,], type = "l")
  # lines(1:45, pred.sim.75[1,], col= "red")
  ####
  # ####  PLOT THE FORECASTING UNCERTAINTY PARTITION -------------------------------
  # ####
  # var_rel_preds <- as.data.frame(t(V.pred.sim.rel*100))
  # var_rel_preds$x <- 1:nrow(var_rel_preds)
  # #my_cols <- c("black", "grey55", "grey70")
  # my_cols <- c("#1b9e77",
  #              "#d95f02",
  #              "#7570b3")
  # variance_plot <- ggplot(data=var_rel_preds, aes(x=x))+
  #   geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters_process), fill=my_cols[3])+
  #   geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters), fill=my_cols[2])+
  #   geom_ribbon(aes(ymin=0, ymax=varianceIC), fill=my_cols[1])+
  #   ylab("Percent of uncertainty")+
  #   xlab("Year")+
  #   #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
  #   #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
  #   scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
  #   theme_bw()
  # 
  # variance_plot 
  
  # now do this for all of the trees:
  
  
  
  
  # ##  For presentations
  # 
  # my_cols <- c("#1b9e77",
  #              "#d95f02",
  #              "#7570b3")
  # tmpvar <- var_rel_preds
  # colnames(tmpvar) <- c( "BvarIPD", "CvarIP", "DvarI", "x")
  # var2 <- tmpvar %>%
  #   gather(simtype, variance, -x)
  
  # ggplot(var2, aes(x=x, fill = simtype))+
  #   geom_ribbon(aes(ymin=0, ymax=variance), color = "black")+
  #   ylab("Percentage of total variance (%)")+
  #   xlab("Year")+
  #   scale_fill_manual(values = my_cols, name = NULL, 
  #                     labels = c("Process error", "Parameter uncertainty", "Initial conditions"))+
  #   scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
  #                      expand = c(0, 0))+
  #   theme_bw()
  # 
  
}
plot.increment.variance (m=300)

for(i in 1:10){
cowplot::plot_grid(plot.increment.variance (m=100) + theme(legend.position = "none"), plot.prop.variance(m = 100)+ theme(legend.position = "none"), ncol = 1)
}


#------------------------------------------------
# same but for DBH:
#------------------------------------------------

plot.prop.variance.dbh <- function(m){
  #-----------------------------------------------------------------------
  # Partitioning uncertainty
  #-----------------------------------------------------------------------
  # Create a function for our process model (linear state space model)
  
  iterate_statespace.dbh <- function( x = x.mat[,"x[1,45]"], m = m, betas.all, alpha, SDdbh, covariates) {
    
    j <- 1
    
    # alpha <- alphas[j, alphaplotid] 
    # bSDI <- betas[j,"betaSDI"]
    # bSDI_ppt <- betas[j,"betaSDI_wintP.wateryr"]
    # bX <-  betas[j,"betaX"]
    # bX2 <- betas[j,"betaX2"]
    # bX_SDI <- betas[j,"betaX_SDI"]
    # bX_ppt <- betas[j,"betaX_wintP.wateryr"]
    # bppt <- betas[j,"betawintP.wateryr"]
    # btmax <- betas[j,"betatmax.fallspr"] 
    # bX_tmax <- betas[j,"betaX_tmax.fallspr"]
    # bSDI_tmax <- betas[j,"betaSDI_tmax.fallspr"]
    # btmax_ppt <- betas[j,"betatmax.fallspr_wintP.wateryr"]
    # 
    
    # pseudocode for now
    dbh.new <-x + alpha + betas.all$b0 + 
      betas.all$bSDI*covariates$SDI + 
      betas.all$bSDI_ppt*covariates$SDI*covariates$ppt + 
      betas.all$bX*(x-30) + 
      betas.all$bX2*(x-30)*(x-30) + 
      betas.all$bX_SDI*(x-30)*covariates$SDI + 
      betas.all$bX_ppt*covariates$ppt*(x-30) + 
      betas.all$bppt*covariates$ppt + 
      betas.all$btmax*covariates$tmax + 
      betas.all$bX_tmax*(x-30)*covariates$tmax + 
      betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
      betas.all$btmax_ppt*covariates$tmax*covariates$ppt 
    #tree.growth
    
    
    # Stochastic process model
    xpred <- rnorm(length(dbh.new), dbh.new, SDdbh) 
    
    xpred
  }
  alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
  
  alpha <- quantile(alphas[, alphaplotid],0.5)
  bSDI <- quantile(betas[,"betaSDI"],0.5)
  bSDI_ppt <- quantile(betas[,"betaSDI_wintP.wateryr"],0.5)
  bX <-  quantile(betas[,"betaX"],0.5)
  bX2 <- quantile(betas[,"betaX2"],0.5)
  bX_SDI <- quantile(betas[,"betaX_SDI"],0.5)
  bX_ppt <- quantile(betas[,"betaX_wintP.wateryr"],0.5)
  bppt <- quantile(betas[,"betawintP.wateryr"],0.5)
  btmax <- quantile(betas[,"betatmax.fallspr"] ,0.5)
  bX_tmax <- quantile(betas[,"betaX_tmax.fallspr"],0.5)
  bSDI_tmax <- quantile(betas[,"betaSDI_tmax.fallspr"],0.5)
  btmax_ppt <- quantile(betas[,"betatmax.fallspr_wintP.wateryr"],0.5)
  b0 <- quantile(B0, 0.5)
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  #x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  
  
  
  
  test <- iterate_statespace.dbh(x = x.mat[,"x[1,45]"], m = m, betas.all = betas.all, alpha, SDdbh = 0, covariates[1,])
  #---------------------------------------------------------------------------
  ##  Initial condition uncertainty: make forecasts from all MCMC iterations of
  ##    the final year, but use mean parameter values and no process error.
  time_steps <- 45
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  
  # just for 1 tree
  for(t in 1:time_steps){
    dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
    forecast[,t] <- dbh.pred
  }
  varianceIC <- apply(forecast,2,var) # get variance from IC
  forecast.ic <- apply(forecast, 2, quantile)
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from Initial conditions AND parameters uncertainty
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  alpha <- alphas[57001:60300, alphaplotid]
  bSDI <- betas[57001:60300,"betaSDI"]
  bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
  bX <-  betas[57001:60300,"betaX"]
  bX2 <- betas[57001:60300,"betaX2"]
  bX_SDI <- betas[57001:60300,"betaX_SDI"]
  bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
  bppt <- betas[57001:60300,"betawintP.wateryr"]
  btmax <- betas[57001:60300,"betatmax.fallspr"]
  bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
  bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
  btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
  b0 <- B0[57001:60300]
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  
  
  
  time_steps <- 45
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  for(t in 1:time_steps){
    inc.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
    forecast[,t] <- inc.pred
  }
  varianceIC_Parameters <- apply(forecast,2,var)
  forecast.ic.param <- apply(forecast, 2, quantile)
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from IC AND parameters uncertainty AND process error
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  alpha <- alphas[57001:60300, alphaplotid]
  bSDI <- betas[57001:60300,"betaSDI"]
  bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
  bX <-  betas[57001:60300,"betaX"]
  bX2 <- betas[57001:60300,"betaX2"]
  bX_SDI <- betas[57001:60300,"betaX_SDI"]
  bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
  bppt <- betas[57001:60300,"betawintP.wateryr"]
  btmax <- betas[57001:60300,"betatmax.fallspr"]
  bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
  bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
  btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
  b0 <- B0[57001:60300]
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  Sdev <- 1/quantile(out[,"tau_dbh"], 0.5)
  
  
  for(t in 1:time_steps){
    inc.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[",m,",", t,"]")], m = m, betas.all = betas.all, alpha, SDdbh = Sdev, covariates[t,])
    forecast[,t] <- inc.pred
  }
  varianceIC_Parameters_process <- apply(forecast,2,var)
  forecast.ic.param.process <- apply(forecast, 2, quantile)
  
  
  # combine variances:
  V.pred.sim     <- rbind(varianceIC_Parameters_process,varianceIC_Parameters,varianceIC)
  V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
  
  # combine forecasts:
  pred.sims     <- data.frame(IPP.0 = forecast.ic.param.process[1,],
                              IP.0 =forecast.ic.param[1,],
                              I.0 =forecast.ic[1,],
                              IPP.100= forecast.ic.param.process[5,],
                              IP.100=forecast.ic.param[5,],
                              I.100=forecast.ic[5,],
                              year = 1:45)
  my_cols <- c("#1b9e77",
               "#d95f02",
               "#7570b3")
  
  predY_plot <- ggplot(data=pred.sims, aes(x=year))+
    geom_ribbon(aes(ymin=IPP.0, ymax=IPP.100), fill=my_cols[3])+
    geom_ribbon(aes(ymin=I.0, ymax=I.100), fill=my_cols[1])+
    geom_ribbon(aes(ymin=IP.0, ymax=IP.100), fill=my_cols[2])+
    
    ylab("Predicted DBH")+
    xlab("Year")+
    #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
    #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
    #scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
    theme_bw()
  
  # pred.sim.0rel <- apply(pred.sim.0,2,function(x) {x/max(x)})
  # 
  # pred.sim.75     <- rbind(forecast.ic.param.process[5,],
  #                          forecast.ic.param[5,],
  #                          forecast.ic[5,])
  # pred.sim.75rel <- apply(pred.sim.75,2,function(x) {x/max(x)})
  # 
  # plot(1:45, pred.sim.0[1,], type = "l")
  # lines(1:45, pred.sim.75[1,], col= "red")
  ####
  ####  PLOT THE FORECASTING UNCERTAINTY PARTITION -------------------------------
  ####
  var_rel_preds <- as.data.frame(t(V.pred.sim.rel*100))
  var_rel_preds$x <- 1:nrow(var_rel_preds)
  #my_cols <- c("black", "grey55", "grey70")
  my_cols <- c("#1b9e77",
               "#d95f02",
               "#7570b3")
  variance_plot <- ggplot(data=var_rel_preds, aes(x=x))+
    geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters_process), fill=my_cols[3])+
    geom_ribbon(aes(ymin=0, ymax=varianceIC), fill=my_cols[1])+
    geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters), fill=my_cols[2])+
    
    ylab("Percent of uncertainty")+
    xlab("Year")+
    #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
    #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
    scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
    theme_bw()
  
  variance_plot 
  
  # now do this for all of the trees:
  
  
  
  
  ##  For presentations
  
  my_cols <- c("#1b9e77",
               "#d95f02",
               "#7570b3")
  tmpvar <- var_rel_preds
  colnames(tmpvar) <- c( "BvarIPD", "CvarIP", "DvarI", "x")
  var2 <- tmpvar %>%
    gather(simtype, variance, -x)
  
  var2$simtype <- factor(var2$simtype, levels = c("BvarIPD", "DvarI", "CvarIP"))
  ggplot(var2, aes(x=x, fill = simtype))+
    geom_ribbon(aes(ymin=0, ymax=variance), color = "black")+
    ylab("Percentage of total variance (%)")+
    xlab("Year")+
    scale_fill_manual(values = my_cols, name = NULL, 
                      labels = c("Process error", "Parameter uncertainty", "Initial conditions"))+
    scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                       expand = c(0, 0))+
    theme_bw()
  
  
}
plot.prop.variance.dbh (m=400)

plot.dbh.variance <- function(m){
  #-----------------------------------------------------------------------
  # Partitioning uncertainty
  #-----------------------------------------------------------------------
  # Create a function for our process model (linear state space model)
  
  iterate_statespace <- function( x = x.mat[,"x[1,45]"], m = m, betas.all, alpha, SDdbh, covariates) {
    
    j <- 1
    
    
    
    # pseudocode for now
    dbh.new <- x + alpha + betas.all$b0 + 
      betas.all$bSDI*covariates$SDI + 
      betas.all$bSDI_ppt*covariates$SDI*covariates$ppt + 
      betas.all$bX*(x-30) + 
      betas.all$bX2*(x-30)*(x-30) + 
      betas.all$bX_SDI*(x-30)*covariates$SDI + 
      betas.all$bX_ppt*covariates$ppt*(x-30) + 
      betas.all$bppt*covariates$ppt + 
      betas.all$btmax*covariates$tmax + 
      betas.all$bX_tmax*(x-30)*covariates$tmax + 
      betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
      betas.all$btmax_ppt*covariates$tmax*covariates$ppt 
    #tree.growth
    
    
    # Stochastic process model
    xpred <- rnorm(length(dbh.new), dbh.new, SDdbh) 
    
    xpred
  }
  
  
  alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
  
  alpha <- quantile(alphas[, alphaplotid],0.5)
  bSDI <- quantile(betas[,"betaSDI"],0.5)
  bSDI_ppt <- quantile(betas[,"betaSDI_wintP.wateryr"],0.5)
  bX <-  quantile(betas[,"betaX"],0.5)
  bX2 <- quantile(betas[,"betaX2"],0.5)
  bX_SDI <- quantile(betas[,"betaX_SDI"],0.5)
  bX_ppt <- quantile(betas[,"betaX_wintP.wateryr"],0.5)
  bppt <- quantile(betas[,"betawintP.wateryr"],0.5)
  btmax <- quantile(betas[,"betatmax.fallspr"] ,0.5)
  bX_tmax <- quantile(betas[,"betaX_tmax.fallspr"],0.5)
  bSDI_tmax <- quantile(betas[,"betaSDI_tmax.fallspr"],0.5)
  btmax_ppt <- quantile(betas[,"betatmax.fallspr_wintP.wateryr"],0.5)
  b0 <- quantile(B0, 0.5)
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  #x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  
  
  
  
  # test <- iterate_statespace(x = x.mat[,"x[1,45]"], m = 1, betas.all = betas.all, alpha, SDinc = 0, covariates[1,])
  #---------------------------------------------------------------------------
  ##  Initial condition uncertainty: make forecasts from all MCMC iterations of
  ##    the final year, but use mean parameter values and no process error.
  time_steps <- 45
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  
  # just for 1 tree
  for(t in 1:time_steps){
    inc.pred <- iterate_statespace(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
    forecast[,t] <- inc.pred
  }
  varianceIC <- apply(forecast,2,var) # get variance from IC
  forecast.ic <- apply(forecast, 2, quantile)
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from Initial conditions AND parameters uncertainty
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  alpha <- alphas[57001:60300, alphaplotid]
  bSDI <- betas[57001:60300,"betaSDI"]
  bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
  bX <-  betas[57001:60300,"betaX"]
  bX2 <- betas[57001:60300,"betaX2"]
  bX_SDI <- betas[57001:60300,"betaX_SDI"]
  bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
  bppt <- betas[57001:60300,"betawintP.wateryr"]
  btmax <- betas[57001:60300,"betatmax.fallspr"]
  bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
  bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
  btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
  b0 <- B0[57001:60300]
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  
  
  
  time_steps <- 45
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  for(t in 1:time_steps){
    inc.pred <- iterate_statespace(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
    forecast[,t] <- inc.pred
  }
  varianceIC_Parameters <- apply(forecast,2,var)
  forecast.ic.param <- apply(forecast, 2, quantile)
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from IC AND parameters uncertainty AND process error
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  alpha <- alphas[57001:60300, alphaplotid]
  bSDI <- betas[57001:60300,"betaSDI"]
  bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
  bX <-  betas[57001:60300,"betaX"]
  bX2 <- betas[57001:60300,"betaX2"]
  bX_SDI <- betas[57001:60300,"betaX_SDI"]
  bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
  bppt <- betas[57001:60300,"betawintP.wateryr"]
  btmax <- betas[57001:60300,"betatmax.fallspr"]
  bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
  bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
  btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
  b0 <- B0[57001:60300]
  
  betas.all.mcmc <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  Sdev <- 1/quantile(out[,"tau_dbh"], 0.5)
  
  
  for(t in 1:time_steps){
    inc.pred <- iterate_statespace(x = x.mat[,paste0("x[",m,",", t,"]")], m = m, betas.all = betas.all.mcmc, alpha, SDdbh = Sdev, covariates[t,])
    forecast[,t] <- inc.pred
  }
  varianceIC_Parameters_process <- apply(forecast,2,var)
  forecast.ic.param.process <- apply(forecast, 2, quantile)
  
  
  # combine variances:
  V.pred.sim     <- rbind(varianceIC_Parameters_process,varianceIC_Parameters,varianceIC)
  V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
  
  # combine forecasts:
  pred.sims     <- data.frame(IPP.0 = forecast.ic.param.process[1,],
                              IP.0 =forecast.ic.param[1,],
                              I.0 =forecast.ic[1,],
                              IPP.100= forecast.ic.param.process[5,],
                              IP.100=forecast.ic.param[5,],
                              I.100=forecast.ic[5,],
                              year = 1:45)
  pred.sims.m <- reshape2::melt(pred.sims, id.vars = "year")
  pred.sims.m$simtype <- ifelse(pred.sims.m$variable %in% c("IPP.0", "IPP.100"), "IPP", 
                                ifelse(pred.sims.m$variable %in% c("IP.0", "IP.100"), "IP", "I"))
  
  pred.sims.m$cat <- ifelse(pred.sims.m$variable %in% c("IPP.0", "IP.0", "I.0"), "lo", "hi")
  pred.sims.by <- pred.sims.m %>% select(-variable) %>% group_by(year, simtype) %>% spread(key = cat, value = value)
  
  my_cols <- c("#1b9e77",
               "#d95f02",
               "#7570b3")
  pred.sims.by$simtype <- factor(pred.sims.by$simtype, levels = c(  "IPP","IP", "I") )
  
  predY_plot <- ggplot(data=pred.sims.by, aes(x=year))+
    geom_ribbon(data=pred.sims.by, aes(ymin=lo, ymax=hi, fill=simtype))+
    scale_fill_manual(values = my_cols, name = NULL, 
                      labels = c("Process error", "Parameter uncertainty", "Initial conditions"))+
    ylab("Predicted Tree Growth")+
    xlab("Year")+
    #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
    #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
    #scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
    theme_bw()
  predY_plot
  
  # pred.sim.0rel <- apply(pred.sim.0,2,function(x) {x/max(x)})
  # 
  # pred.sim.75     <- rbind(forecast.ic.param.process[5,],
  #                          forecast.ic.param[5,],
  #                          forecast.ic[5,])
  # pred.sim.75rel <- apply(pred.sim.75,2,function(x) {x/max(x)})
  # 
  # plot(1:45, pred.sim.0[1,], type = "l")
  # lines(1:45, pred.sim.75[1,], col= "red")
  ####
  # ####  PLOT THE FORECASTING UNCERTAINTY PARTITION -------------------------------
  # ####
  # var_rel_preds <- as.data.frame(t(V.pred.sim.rel*100))
  # var_rel_preds$x <- 1:nrow(var_rel_preds)
  # #my_cols <- c("black", "grey55", "grey70")
  # my_cols <- c("#1b9e77",
  #              "#d95f02",
  #              "#7570b3")
  # variance_plot <- ggplot(data=var_rel_preds, aes(x=x))+
  #   geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters_process), fill=my_cols[3])+
  #   geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters), fill=my_cols[2])+
  #   geom_ribbon(aes(ymin=0, ymax=varianceIC), fill=my_cols[1])+
  #   ylab("Percent of uncertainty")+
  #   xlab("Year")+
  #   #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
  #   #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
  #   scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
  #   theme_bw()
  # 
  # variance_plot 
  
  # now do this for all of the trees:
  
  
  
  
  # ##  For presentations
  # 
  # my_cols <- c("#1b9e77",
  #              "#d95f02",
  #              "#7570b3")
  # tmpvar <- var_rel_preds
  # colnames(tmpvar) <- c( "BvarIPD", "CvarIP", "DvarI", "x")
  # var2 <- tmpvar %>%
  #   gather(simtype, variance, -x)
  
  # ggplot(var2, aes(x=x, fill = simtype))+
  #   geom_ribbon(aes(ymin=0, ymax=variance), color = "black")+
  #   ylab("Percentage of total variance (%)")+
  #   xlab("Year")+
  #   scale_fill_manual(values = my_cols, name = NULL, 
  #                     labels = c("Process error", "Parameter uncertainty", "Initial conditions"))+
  #   scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
  #                      expand = c(0, 0))+
  #   theme_bw()
  # 
  
}
plot.dbh.variance (m=400)
cowplot::plot_grid(plot.dbh.variance (m=100) + theme(legend.position = "none"), plot.prop.variance.dbh(m = 100)+ theme(legend.position = "none"), ncol = 1)



#------------------------------------------------
# set up to forecast through time from just the X initial condiation
#------------------------------------------------
# this is important for when we start to forecast with climate drivers
plot.prop.variance.dbh.forecast <- function(m, prop =TRUE){
  #-----------------------------------------------------------------------
  # Partitioning uncertainty
  #-----------------------------------------------------------------------
  # Create a function for our process model (linear state space model)
  
  iterate_statespace.dbh <- function( x = x.mat[,"x[1,45]"], m = m, betas.all, alpha, SDdbh, covariates) {
    
    j <- 1
    
    # alpha <- alphas[j, alphaplotid] 
    # bSDI <- betas[j,"betaSDI"]
    # bSDI_ppt <- betas[j,"betaSDI_wintP.wateryr"]
    # bX <-  betas[j,"betaX"]
    # bX2 <- betas[j,"betaX2"]
    # bX_SDI <- betas[j,"betaX_SDI"]
    # bX_ppt <- betas[j,"betaX_wintP.wateryr"]
    # bppt <- betas[j,"betawintP.wateryr"]
    # btmax <- betas[j,"betatmax.fallspr"] 
    # bX_tmax <- betas[j,"betaX_tmax.fallspr"]
    # bSDI_tmax <- betas[j,"betaSDI_tmax.fallspr"]
    # btmax_ppt <- betas[j,"betatmax.fallspr_wintP.wateryr"]
    # 
    
    # pseudocode for now
    dbh.new <-x + alpha + betas.all$b0 + 
      betas.all$bSDI*covariates$SDI + 
      betas.all$bSDI_ppt*covariates$SDI*covariates$ppt + 
      betas.all$bX*(x-30) + 
      betas.all$bX2*(x-30)*(x-30) + 
      betas.all$bX_SDI*(x-30)*covariates$SDI + 
      betas.all$bX_ppt*covariates$ppt*(x-30) + 
      betas.all$bppt*covariates$ppt + 
      betas.all$btmax*covariates$tmax + 
      betas.all$bX_tmax*(x-30)*covariates$tmax + 
      betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
      betas.all$btmax_ppt*covariates$tmax*covariates$ppt 
    #tree.growth
    
    
    # Stochastic process model
    xpred <- rnorm(length(dbh.new), dbh.new, SDdbh) 
    
    xpred
  }
  alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
  
  alpha <- quantile(alphas[, alphaplotid],0.5)
  bSDI <- quantile(betas[,"betaSDI"],0.5)
  bSDI_ppt <- quantile(betas[,"betaSDI_wintP.wateryr"],0.5)
  bX <-  quantile(betas[,"betaX"],0.5)
  bX2 <- quantile(betas[,"betaX2"],0.5)
  bX_SDI <- quantile(betas[,"betaX_SDI"],0.5)
  bX_ppt <- quantile(betas[,"betaX_wintP.wateryr"],0.5)
  bppt <- quantile(betas[,"betawintP.wateryr"],0.5)
  btmax <- quantile(betas[,"betatmax.fallspr"] ,0.5)
  bX_tmax <- quantile(betas[,"betaX_tmax.fallspr"],0.5)
  bSDI_tmax <- quantile(betas[,"betaSDI_tmax.fallspr"],0.5)
  btmax_ppt <- quantile(betas[,"betatmax.fallspr_wintP.wateryr"],0.5)
  b0 <- quantile(B0, 0.5)
  
  betas.point <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  #x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  
  
  
  
  test <- iterate_statespace.dbh(x = x.mat[,"x[1,45]"], m = m, betas.all = betas.point, alpha, SDdbh = 0, covariates[1,])
  #---------------------------------------------------------------------------
  ##  Initial condition uncertainty: make forecasts from all MCMC iterations of
  ##    the final year, but use mean parameter values and no process error.
  time_steps <- 45
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  
  # just for 1 tree
  for(t in 1:time_steps){
    if(t == 1){
    dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.point, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
    forecast[,t] <- dbh.pred
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.point, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      
    }
  }
  varianceIC <- apply(forecast,2,var) # get variance from IC
  forecast.ic <- apply(forecast, 2, quantile)
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from Initial conditions AND parameters uncertainty
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  alpha <- alphas[57001:60300, alphaplotid]
  bSDI <- betas[57001:60300,"betaSDI"]
  bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
  bX <-  betas[57001:60300,"betaX"]
  bX2 <- betas[57001:60300,"betaX2"]
  bX_SDI <- betas[57001:60300,"betaX_SDI"]
  bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
  bppt <- betas[57001:60300,"betawintP.wateryr"]
  btmax <- betas[57001:60300,"betatmax.fallspr"]
  bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
  bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
  btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
  b0 <- B0[57001:60300]
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  
  
  
  time_steps <- 45
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      
    }  }
  varianceIC_Parameters <- apply(forecast,2,var)
  forecast.ic.param <- apply(forecast, 2, quantile)
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from IC AND parameters uncertainty AND process error
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  alpha <- alphas[57001:60300, alphaplotid]
  bSDI <- betas[57001:60300,"betaSDI"]
  bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
  bX <-  betas[57001:60300,"betaX"]
  bX2 <- betas[57001:60300,"betaX2"]
  bX_SDI <- betas[57001:60300,"betaX_SDI"]
  bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
  bppt <- betas[57001:60300,"betawintP.wateryr"]
  btmax <- betas[57001:60300,"betatmax.fallspr"]
  bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
  bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
  btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
  b0 <- B0[57001:60300]
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  Sdev <- 1/quantile(out[,"tau_dbh"], 0.5)
  
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = Sdev, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = Sdev, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      
    }  }
  varianceIC_Parameters_process <- apply(forecast,2,var)
  forecast.ic.param.process <- apply(forecast, 2, quantile)
  
  
  # combine variances:
  V.pred.sim     <- rbind(varianceIC_Parameters_process,varianceIC_Parameters,varianceIC)
  V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
  
  # combine forecasts:
  pred.sims     <- data.frame(IPP.0 = forecast.ic.param.process[1,],
                              IP.0 =forecast.ic.param[1,],
                              I.0 =forecast.ic[1,],
                              IPP.100= forecast.ic.param.process[5,],
                              IP.100=forecast.ic.param[5,],
                              I.100=forecast.ic[5,],
                              year = 1:45)
  my_cols <- c("#1b9e77",
               "#d95f02",
               "#7570b3")
  
  predY_plot <- ggplot(data=pred.sims, aes(x=year))+
    geom_ribbon(aes(ymin=IPP.0, ymax=IPP.100), fill=my_cols[3])+
    geom_ribbon(aes(ymin=IP.0, ymax=IP.100), fill=my_cols[2])+
    geom_ribbon(aes(ymin=I.0, ymax=I.100), fill=my_cols[1])+
    
    
    ylab("Predicted DBH")+
    xlab("Year")+
    #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
    #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
    #scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
    theme_bw()
  
  # pred.sim.0rel <- apply(pred.sim.0,2,function(x) {x/max(x)})
  # 
  # pred.sim.75     <- rbind(forecast.ic.param.process[5,],
  #                          forecast.ic.param[5,],
  #                          forecast.ic[5,])
  # pred.sim.75rel <- apply(pred.sim.75,2,function(x) {x/max(x)})
  # 
  # plot(1:45, pred.sim.0[1,], type = "l")
  # lines(1:45, pred.sim.75[1,], col= "red")
  ####
  ####  PLOT THE FORECASTING UNCERTAINTY PARTITION -------------------------------
  ####
  var_rel_preds <- as.data.frame(t(V.pred.sim.rel*100))
  var_rel_preds$x <- 1:nrow(var_rel_preds)
  #my_cols <- c("black", "grey55", "grey70")
  my_cols <- c("#1b9e77",
               "#d95f02",
               "#7570b3")
  variance_plot <- ggplot(data=var_rel_preds, aes(x=x))+
    geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters_process), fill=my_cols[3])+
    geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters),fill=my_cols[2])+
    geom_ribbon(aes(ymin=0, ymax=varianceIC), fill=my_cols[1])+
     
    
    ylab("Percent of uncertainty")+
    xlab("Year")+
    #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
    #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
    scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
    theme_bw()
  
  variance_plot 
  
  # now do this for all of the trees:
  
  
  
  
  ##  For presentations
  
  my_cols <- c("#1b9e77",
               "#d95f02",
               "#7570b3")
  tmpvar <- var_rel_preds
  colnames(tmpvar) <- c( "BvarIPD", "CvarIP", "DvarI", "x")
  var2 <- tmpvar %>%
    gather(simtype, variance, -x)
  
  #var2$simtype <- factor(var2$simtype, levels = c("BvarIPD", "DvarI", "CvarIP"))
  prop.var <- ggplot(var2, aes(x=x, fill = simtype))+
    geom_ribbon(aes(ymin=0, ymax=variance), color = "black")+
    ylab("Percentage of total variance (%)")+
    xlab("Year")+
    scale_fill_manual(values = my_cols, name = NULL, 
                      labels = c("Process error", "Parameter uncertainty", "Initial conditions"))+
    scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                       expand = c(0, 0))+
    theme_bw()
  
  if(prop == TRUE){
   prop.var
  }else{
    predY_plot
  }
}

plot.prop.variance.dbh.forecast(2, prop =TRUE)
plot.prop.variance.dbh.forecast(2, prop =FALSE)

plot.prop.variance.inc.forecast <- function(m, prop =TRUE){
  #-----------------------------------------------------------------------
  # Partitioning uncertainty
  #-----------------------------------------------------------------------
  # Create a function for our process model (linear state space model)
  
  iterate_statespace.dbh <- function( x = x.mat[,"x[1,45]"], m = m, betas.all, alpha, SDdbh, covariates) {
    
    j <- 1
    
    # alpha <- alphas[j, alphaplotid] 
    # bSDI <- betas[j,"betaSDI"]
    # bSDI_ppt <- betas[j,"betaSDI_wintP.wateryr"]
    # bX <-  betas[j,"betaX"]
    # bX2 <- betas[j,"betaX2"]
    # bX_SDI <- betas[j,"betaX_SDI"]
    # bX_ppt <- betas[j,"betaX_wintP.wateryr"]
    # bppt <- betas[j,"betawintP.wateryr"]
    # btmax <- betas[j,"betatmax.fallspr"] 
    # bX_tmax <- betas[j,"betaX_tmax.fallspr"]
    # bSDI_tmax <- betas[j,"betaSDI_tmax.fallspr"]
    # btmax_ppt <- betas[j,"betatmax.fallspr_wintP.wateryr"]
    # 
    
    # pseudocode for now
    tree.growth <- alpha + betas.all$b0 + 
      betas.all$bSDI*covariates$SDI + 
      betas.all$bSDI_ppt*covariates$SDI*covariates$ppt + 
      betas.all$bX*(x-30) + 
      betas.all$bX2*(x-30)*(x-30) + 
      betas.all$bX_SDI*(x-30)*covariates$SDI + 
      betas.all$bX_ppt*covariates$ppt*(x-30) + 
      betas.all$bppt*covariates$ppt + 
      betas.all$btmax*covariates$tmax + 
      betas.all$bX_tmax*(x-30)*covariates$tmax + 
      betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
      betas.all$btmax_ppt*covariates$tmax*covariates$ppt 
    #tree.growth
    
    
    # Stochastic process model
    xpred <- rnorm(length(tree.growth), tree.growth, SDdbh) 
    
    xpred
  }
  alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
  
  alpha <- quantile(alphas[, alphaplotid],0.5)
  bSDI <- quantile(betas[,"betaSDI"],0.5)
  bSDI_ppt <- quantile(betas[,"betaSDI_wintP.wateryr"],0.5)
  bX <-  quantile(betas[,"betaX"],0.5)
  bX2 <- quantile(betas[,"betaX2"],0.5)
  bX_SDI <- quantile(betas[,"betaX_SDI"],0.5)
  bX_ppt <- quantile(betas[,"betaX_wintP.wateryr"],0.5)
  bppt <- quantile(betas[,"betawintP.wateryr"],0.5)
  btmax <- quantile(betas[,"betatmax.fallspr"] ,0.5)
  bX_tmax <- quantile(betas[,"betaX_tmax.fallspr"],0.5)
  bSDI_tmax <- quantile(betas[,"betaSDI_tmax.fallspr"],0.5)
  btmax_ppt <- quantile(betas[,"betatmax.fallspr_wintP.wateryr"],0.5)
  b0 <- quantile(B0, 0.5)
  
  betas.point <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                            bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  #x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  
  
  
  
  test <- iterate_statespace.dbh(x = x.mat[,"x[1,45]"], m = m, betas.all = betas.point, alpha, SDdbh = 0, covariates[1,])
  #---------------------------------------------------------------------------
  ##  Initial condition uncertainty: make forecasts from all MCMC iterations of
  ##    the final year, but use mean parameter values and no process error.
  time_steps <- 45
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  
  # just for 1 tree
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.point, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.point, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      
    }
  }
  varianceIC <- apply(forecast,2,var) # get variance from IC
  forecast.ic <- apply(forecast, 2, quantile)
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from Initial conditions AND parameters uncertainty
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  alpha <- alphas[57001:60300, alphaplotid]
  bSDI <- betas[57001:60300,"betaSDI"]
  bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
  bX <-  betas[57001:60300,"betaX"]
  bX2 <- betas[57001:60300,"betaX2"]
  bX_SDI <- betas[57001:60300,"betaX_SDI"]
  bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
  bppt <- betas[57001:60300,"betawintP.wateryr"]
  btmax <- betas[57001:60300,"betatmax.fallspr"]
  bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
  bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
  btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
  b0 <- B0[57001:60300]
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  
  
  
  time_steps <- 45
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      
    }  }
  varianceIC_Parameters <- apply(forecast,2,var)
  forecast.ic.param <- apply(forecast, 2, quantile)
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from IC AND parameters uncertainty AND process error
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  alpha <- alphas[57001:60300, alphaplotid]
  bSDI <- betas[57001:60300,"betaSDI"]
  bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
  bX <-  betas[57001:60300,"betaX"]
  bX2 <- betas[57001:60300,"betaX2"]
  bX_SDI <- betas[57001:60300,"betaX_SDI"]
  bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
  bppt <- betas[57001:60300,"betawintP.wateryr"]
  btmax <- betas[57001:60300,"betatmax.fallspr"]
  bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
  bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
  btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
  b0 <- B0[57001:60300]
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  Sdev <- 1/quantile(out[,"tau_inc"], 0.5)
  
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = Sdev, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = Sdev, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      
    }  }
  varianceIC_Parameters_process <- apply(forecast,2,var)
  forecast.ic.param.process <- apply(forecast, 2, quantile)
  
  
  # combine variances:
  V.pred.sim     <- rbind(varianceIC_Parameters_process,varianceIC_Parameters,varianceIC)
  V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
  
  # combine forecasts:
  pred.sims     <- data.frame(IPP.0 = forecast.ic.param.process[1,],
                              IP.0 =forecast.ic.param[1,],
                              I.0 =forecast.ic[1,],
                              IPP.100= forecast.ic.param.process[5,],
                              IP.100=forecast.ic.param[5,],
                              I.100=forecast.ic[5,],
                              year = 1:45)
  my_cols <- c("#1b9e77",
               "#d95f02",
               "#7570b3")
  
  predY_plot <- ggplot(data=pred.sims, aes(x=year))+
    geom_ribbon(aes(ymin=IPP.0, ymax=IPP.100), fill=my_cols[3])+
    geom_ribbon(aes(ymin=IP.0, ymax=IP.100), fill=my_cols[2])+
    geom_ribbon(aes(ymin=I.0, ymax=I.100+0.001), fill=my_cols[1])+
    
    
    ylab("Predicted DBH")+
    xlab("Year")+
    #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
    #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
    #scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
    theme_bw()
  predY_plot
  # pred.sim.0rel <- apply(pred.sim.0,2,function(x) {x/max(x)})
  # 
  # pred.sim.75     <- rbind(forecast.ic.param.process[5,],
  #                          forecast.ic.param[5,],
  #                          forecast.ic[5,])
  # pred.sim.75rel <- apply(pred.sim.75,2,function(x) {x/max(x)})
  # 
  # plot(1:45, pred.sim.0[1,], type = "l")
  # lines(1:45, pred.sim.75[1,], col= "red")
  ####
  ####  PLOT THE FORECASTING UNCERTAINTY PARTITION -------------------------------
  ####
  var_rel_preds <- as.data.frame(t(V.pred.sim.rel*100))
  var_rel_preds$x <- 1:nrow(var_rel_preds)
  #my_cols <- c("black", "grey55", "grey70")
  my_cols <- c("#1b9e77",
               "#d95f02",
               "#7570b3")
  variance_plot <- ggplot(data=var_rel_preds, aes(x=x))+
    geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters_process), fill=my_cols[3])+
    geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters),fill=my_cols[2])+
    geom_ribbon(aes(ymin=0, ymax=varianceIC), fill=my_cols[1])+
    
    
    ylab("Percent of uncertainty")+
    xlab("Year")+
    #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
    #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
    scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
    theme_bw()
  
  variance_plot 
  
  # now do this for all of the trees:
  
  
  
  
  ##  For presentations
  
  my_cols <- c("#1b9e77",
               "#d95f02",
               "#7570b3")
  tmpvar <- var_rel_preds
  colnames(tmpvar) <- c( "BvarIPD", "CvarIP", "DvarI", "x")
  var2 <- tmpvar %>%
    gather(simtype, variance, -x)
  
  #var2$simtype <- factor(var2$simtype, levels = c("BvarIPD", "DvarI", "CvarIP"))
  prop.var <- ggplot(var2, aes(x=x, fill = simtype))+
    geom_ribbon(aes(ymin=0, ymax=variance), color = "black")+
    ylab("Percentage of total variance (%)")+
    xlab("Year")+
    scale_fill_manual(values = my_cols, name = NULL, 
                      labels = c("Process error", "Parameter uncertainty", "Initial conditions"))+
    scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                       expand = c(0, 0))+
    theme_bw()
  
  if(prop == TRUE){
    prop.var
  }else{
    predY_plot
  }
}
plot.prop.variance.inc.forecast(2, prop =TRUE)
plot.prop.variance.inc.forecast(2, prop =FALSE)

#-------------------------------------------------------------------
# set up to forecast through time from just the last X condiation
# using future climate time series (2018-2099)
# note that this version also includes driver uncertainty
#-------------------------------------------------------------------

# driver uncertainty:
# for each scenario, in each year, for each plot, we sample from the distribution of ppt and tmax
# that has a mean of the ensemble mean, and variance of the ensemble variance

# read the rds with climate scenarios:

clim.ts <- readRDS(gzcon(url("https://de.cyverse.org/dl/d/C38AB4DD-04D7-42DE-ABC0-C2CD2EA50BF9/pipo.cores.with.downscaled.hydro.ppt.climatev2.rds")))
clim.ts.df <- clim.ts$future.climate.ts
clim.ts.df$tmax.fall.spr[is.nan(clim.ts.df$tmax.fall.spr)] <- NA
#tmax.fallspr.df <- tmax.fallspr


# need to scale future climate data on the same scale as the past climate
clim.ts.df$ppt.scale <-(clim.ts.df$year.ppt-mean(as.matrix(clim.data$wintP.wateryr)))/sd(as.matrix(clim.data$wintP.wateryr))
clim.ts.df$tmax.scaled <-(clim.ts.df$tmax.fall.spr-mean(as.matrix(clim.data$tmax.fallspr)))/sd(as.matrix(clim.data$tmax.fallspr))


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

new.table <- cov.data[,c("LAT", "LON", "id", "plotid")]
colnames(new.table)[1:2] <- c("lat", "lon")

# merge with the climate.ensemble.means data base:
ens.means <- merge(new.table, climate.ensemble.means, by = c("lat", "lon"))
sample.ll <- ens.means[ens.means$id %in% 81,]
clim.ts.df.full <- left_join(new.table, clim.ts.df, by = c("lat", "lon"))
ts.all  <- merge(new.table, clim.ts.df, by = c("lat", "lon"))

ggplot(ts.all[ts.all$id %in% 81,], aes(x = year, y = year.ppt, color = modelrun))+geom_line()#+facet_wrap(~rcp)
ggplot(ts.all[ts.all$id %in% 92,], aes(x = year, y = tmax.fall.spr, color = modelrun))+geom_line() + stat_smooth()#+facet_wrap(~rcp)
ggplot(ts.all, aes(x = rcp, y = tmax.fall.spr, color = rcp))+geom_boxplot()#+facet_wrap(~rcp)

ts.all$period <- ifelse(ts.all$year >= 2018 & ts.all$year <2050, "2018 - 2050",
                        ifelse(ts.all$year >= 2050 & ts.all$year <2070, "2050 - 2070","2070-2099"))
ggplot(ts.all, aes(x = rcp, y = tmax.fall.spr, color = period))+geom_boxplot()

ggplot(ts.all, aes(x = rcp, y = tmax.fall.spr, color = period))+geom_boxplot()+facet_wrap(~modelrun)
ggplot(ts.all, aes(x = rcp, y = year.ppt, color = period))+geom_boxplot()+facet_wrap(~modelrun)
set.seed (11)




# function for partitioning uncertinaty into driver, IC, random effects, parameter error, and process error
plot.prop.variance.future.forecast.driver <- function(m, prop =TRUE, scenario = "rcp26", type = "dbh"){
  #-----------------------------------------------------------------------
  # Partitioning uncertainty
  #-----------------------------------------------------------------------
  # Create a function for our process model (linear state space model)
  
  iterate_statespace.dbh <- function( x = x.mat[,"x[1,45]"], m = m, betas.all, alpha, SDdbh, SDinc = 0, covariates) {
    
    #j <- 1
    
    
    # pseudocode for now
    tree.growth <- x +  alpha + betas.all$b0 + 
      betas.all$bSDI*covariates$SDI + 
      betas.all$bSDI_ppt*covariates$SDI*covariates$ppt + 
      betas.all$bX*(x-30) + 
      betas.all$bX2*(x-30)*(x-30) + 
      betas.all$bX_SDI*(x-30)*covariates$SDI + 
      betas.all$bX_ppt*covariates$ppt*(x-30) + 
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
  alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
  
  alpha <- quantile(alphas[, alphaplotid],0.5)
  bSDI <- quantile(betas[,"betaSDI"],0.5)
  bSDI_ppt <- quantile(betas[,"betaSDI_wintP.wateryr"],0.5)
  bX <-  quantile(betas[,"betaX"],0.5)
  bX2 <- quantile(betas[,"betaX2"],0.5)
  bX_SDI <- quantile(betas[,"betaX_SDI"],0.5)
  bX_ppt <- quantile(betas[,"betaX_wintP.wateryr"],0.5)
  bppt <- quantile(betas[,"betawintP.wateryr"],0.5)
  btmax <- quantile(betas[,"betatmax.fallspr"] ,0.5)
  bX_tmax <- quantile(betas[,"betaX_tmax.fallspr"],0.5)
  bSDI_tmax <- quantile(betas[,"betaSDI_tmax.fallspr"],0.5)
  btmax_ppt <- quantile(betas[,"betatmax.fallspr_wintP.wateryr"],0.5)
  b0 <- quantile(B0, 0.5)
  
  betas.point <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                            bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  
  #scenario <- "rcp26"
  # code toe filter out future climate data for the tree id and the rcp:
  # filter out for the means
  future.proj <- ens.means[ens.means$id == m & ens.means$rcp == scenario, ]
  proj.ordered <- future.proj[order(future.proj$year),]
  
  # now filter for the full ensemble
  future.ens <- clim.ts.df.full[clim.ts.df.full$id == m & clim.ts.df.full$rcp == scenario,]
  ens.proj.ordered <-  future.ens[order( future.ens$year),]
  #ggplot(proj.ordered, aes(as.numeric(year), mean.ppt))+geom_point()+stat_smooth()
  # just use the ensemble means (w.out driver uncertainty)
  ppt <- proj.ordered$mean.ppt
  tmax <- proj.ordered$mean.tmax.fs
  #ppt <- time_data$wintP.wateryr[m,]
  #tmax <- time_data$tmax.fallspr[m,]
  SDI <- rep(cov.data[m, ]$SDI, length(ppt))
  #x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  
  
  
  
  #test <- iterate_statespace.dbh(x = x.mat[,"x[1,45]"], m = m, betas.all = betas.point, alpha, SDdbh = 0, covariates[1,], rw = type)
  #---------------------------------------------------------------------------
  ##  Initial condition uncertainty: make forecasts from all MCMC iterations of
  ##    the final year, but use mean parameter values and no process error.
  time_steps <- length(ppt)
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  # just for 1 tree # note that we start at x = 45
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", 45,"]")], m = m, betas.all = betas.point, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t]-x.mat[,paste0("x[", m,",", 45,"]")]
        
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.point, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t]-forecast[,t-1]
      
    }
    
   
  }
  varianceIC <- apply(forecast,2,var) # get variance from IC
  forecast.ic <- apply(forecast, 2, quantile)
  
  var.inc.ic <- apply(inc, 2, var)
  inc.ic <- apply(inc, 2, quantile)
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from Initial conditions AND beta parameters uncertainty
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  alpha = quantile(alphas[, alphaplotid],0.5)
  #alpha <- rnorm(length(57001:60300), mean = mean(alphas[57001:60300, alphaplotid]), sd = sd(alphas[57001:60300, alphaplotid]))
  bSDI <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betaSDI"]), sd = sd(betas[57001:60300,"betaSDI"]))
  bSDI_ppt <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betaSDI_wintP.wateryr"]), sd = sd(betas[57001:60300,"betaSDI_wintP.wateryr"]))
  bX <-  rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betaX"]), sd = sd(betas[57001:60300,"betaX"]))
  bX2 <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betaX2"]), sd = sd(betas[57001:60300,"betaX2"]))
  bX_SDI <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betaX_SDI"]), sd = sd(betas[57001:60300,"betaX_SDI"]))
  bX_ppt <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betaX_wintP.wateryr"]), sd = sd(betas[57001:60300,"betaX_wintP.wateryr"]))
  bppt <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betawintP.wateryr"]), sd = sd(betas[57001:60300,"betawintP.wateryr"]))
  btmax <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betatmax.fallspr"]), sd = sd(betas[57001:60300,"betatmax.fallspr"]))
  bX_tmax <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betaX_tmax.fallspr"]), sd = sd(betas[57001:60300,"betaX_tmax.fallspr"]))
  bSDI_tmax <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betaSDI_tmax.fallspr"]), sd = sd(betas[57001:60300,"betaSDI_tmax.fallspr"]))
  btmax_ppt <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]), sd = sd(betas[57001:60300,"betaSDI_tmax.fallspr"]))
  b0 <- rnorm(length(57001:60300), mean = mean(B0[57001:60300]), sd = sd(B0[57001:60300]))
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  #x <- x.mat[,"x[1,45]"]
  
  # code toe filter out future climate data for the tree id and the rcp:
  
  #future.proj <- ens.means[ens.means$id == m & ens.means$rcp == scenario, ]
  #proj.ordered <- future.proj[order(future.proj$year),]
  
  # just use the ensemble means (w.out driver uncertainty)
  ppt <- proj.ordered$mean.ppt
  tmax <- proj.ordered$mean.tmax.fs
  #ppt <- time_data$wintP.wateryr[m,]
  #tmax <- time_data$tmax.fallspr[m,]
  SDI <- rep(cov.data[m, ]$SDI, length(ppt))
  #x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  time_steps <- length(ppt)
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", 45,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t]-x.mat[,paste0("x[", m,",", 45,"]")]
      
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t]-forecast[,t-1]
      
      
    }  }
  varianceIC_Parameters <- apply(forecast,2,var)
  forecast.ic.param <- apply(forecast, 2, quantile)
  
  var.inc.IC_Parameters <- apply(inc,2,var)
  inc.ic.param <- apply(inc, 2, quantile)
  #---------------------------------------------------------------------------
  ##  Uncertainty from Initial conditions AND parameters uncertainty AND plot random effects
  #---------------------------------------------------------------------------
  
  
  alpha <- rnorm(length(57001:60300), mean = mean(alphas[57001:60300, alphaplotid]), sd = sd(alphas[57001:60300, alphaplotid]))
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 45)
  #ppt <- time_data$wintP.wateryr[m,]
  #tmax <- time_data$tmax.fallspr[m,]
  #x <- x.mat[,"x[1,45]"]
  
  # code toe filter out future climate data for the tree id and the rcp:
  
  #future.proj <- ens.means[ens.means$id == m & ens.means$rcp == scenario, ]
  #proj.ordered <- future.proj[order(future.proj$year),]
  
  # just use the ensemble means (w.out driver uncertainty)
  ppt <- proj.ordered$mean.ppt
  tmax <- proj.ordered$mean.tmax.fs
  #ppt <- time_data$wintP.wateryr[m,]
  #tmax <- time_data$tmax.fallspr[m,]
  SDI <- rep(cov.data[m, ]$SDI, length(ppt))
  #x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  time_steps <- length(ppt)
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", 45,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t]-x.mat[,paste0("x[", m,",", 45,"]")]
      
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t]-forecast[,t-1]
      
    }  }
  varianceIC_Parameters.alpha <- apply(forecast,2,var)
  forecast.ic.param.alpha <- apply(forecast, 2, quantile)
  
  var.inc.IC_Parameters.alpha <- apply(inc,2,var)
  inc.ic.param.alpha <- apply(inc, 2, quantile)
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from Initial conditions AND parameters uncertainty AND future Driver uncertainty
  #---------------------------------------------------------------------------
  
  # use all of the parameter MCMCS:
  
  # 
  # just use the ensemble means (w.out driver uncertainty)
  ppt <- tmax <- SDI <- matrix(NA, nrow =length(57001:60300), ncol = 82 )
  for(i in 1:82){
    
    ppt[,i]<- rnorm(n = length(57001:60300) , mean = proj.ordered[i,]$mean.ppt, sd = proj.ordered[i,]$SD.ppt)
    tmax[,i]<- rnorm(n = length(57001:60300) , mean = proj.ordered[i,]$mean.tmax.fs, sd = proj.ordered[i,]$SD.tmax)
    SDI[,i]<- rep(cov.data[m, ]$SDI, length(57001:60300))
    
  }
  
  # for(i in 2018:2099){
  # 
  #   
  #   ppt[,i]<- sample(x =  ens.proj.ordered[ens.proj.ordered$year == i,]$ppt.scale, replace = TRUE, size = length(57001:60300))
  #   tmax[,i]<- sample(n = length(57001:60300) , ens.proj.ordered[ens.proj.ordered$year == i,]$tmax.scale)
  #   SDI[,i]<- rep(cov.data[m, ]$SDI, length(57001:60300))
  #   
  # }
  #tmax <- proj.ordered$mean.tmax.fs
  #ppt <- time_data$wintP.wateryr[m,]
  #tmax <- time_data$tmax.fallspr[m,]
  #SDI <- rep(cov.data[m, ]$SDI, length(ppt))
  #x <- x.mat[,"x[1,45]"]
  covariates <- list()
  covariates$SDI <- SDI
  covariates$ppt <- ppt
  covariates$tmax <- tmax
  
  #covariates <- list(SDI, ppt, tmax)
  
  
  
  time_steps <- 82
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", 45,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = data.frame(SDI = covariates$SDI[,t], 
                                                                                                                                                               ppt = covariates$ppt[,t], 
                                                                                                                                                               tmax = covariates$tmax[,t]))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-x.mat[,paste0("x[", m,",", 45,"]")]
      
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = data.frame(SDI = covariates$SDI[,t], 
                                                                                                                                          ppt = covariates$ppt[,t], 
                                                                                                                                          tmax = covariates$tmax[,t]))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-forecast[,t-1]
      
    }  }
  varianceIC_Parameters_driver <- apply(forecast,2,var)
  forecast.ic.param.d <- apply(forecast, 2, quantile)
  
  var.inc.IC_Parameters_driver <- apply(inc,2,var)
  inc.ic.param.d <- apply(inc, 2, quantile)
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from IC AND parameters uncertainty AND process error
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  
  
  time_steps <- 82
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  
  Sdev <- sqrt(1/quantile(out[,"tau_add"], 0.5))
  Sdevinc <- 1/quantile(out[,"tau_inc"], 0.5)
  
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", 45,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = Sdev,SDinc = Sdevinc,  covariates = data.frame(SDI = covariates$SDI[,t], 
                                                                                                                                                                   ppt = covariates$ppt[,t], 
                                                                                                                                                                   tmax = covariates$tmax[,t]))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-x.mat[,paste0("x[", m,",", 45,"]")]
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = Sdev, SDinc = Sdevinc,covariates = data.frame(SDI = covariates$SDI[,t], 
                                                                                                                                             ppt = covariates$ppt[,t], 
                                                                                                                                             tmax = covariates$tmax[,t]))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-forecast[,t-1]
      
    }  }
  varianceIC_Parameters_process.driver <- apply(forecast,2,var)
  forecast.ic.param.process.driver <- apply(forecast, 2, quantile)
  
  var.inc.IC_Parameters_process.driver <- apply(inc,2,var)
  inc.ic.param.process.driver <- apply(inc, 2, quantile)
  
  # combine variances:
  if(type == "dbh"){
  V.pred.sim     <- rbind(varianceIC_Parameters_process.driver, varianceIC_Parameters_driver,varianceIC_Parameters.alpha, 
                          varianceIC_Parameters,varianceIC)
  
  # combine forecasts:
  pred.sims     <- data.frame(IPP.0 = forecast.ic.param.process.driver[1,],
                              
                              IPD.0 = forecast.ic.param.d[1,],
                              IPA.0 =forecast.ic.param.alpha[1,],
                              IP.0 =forecast.ic.param[1,],
                              I.0 =forecast.ic[1,],
                              IPP.100= forecast.ic.param.process.driver[5,],
                              IPD.100 = forecast.ic.param.d[5,],
                              IPA.100 =forecast.ic.param.alpha[5,],
                              IP.100=forecast.ic.param[5,],
                              I.100=forecast.ic[5,],
                              year = 2018:2099)
  
  axis.name <- "Diameter"
  
  }else{
  V.pred.sim     <- rbind(var.inc.IC_Parameters_process.driver, var.inc.IC_Parameters_driver,var.inc.IC_Parameters.alpha, 
                          var.inc.IC_Parameters,var.inc.ic)
  
  pred.sims     <- data.frame(IPP.0 = inc.ic.param.process.driver[1,],
                              
                              IPD.0 = inc.ic.param.d[1,],
                              IPA.0 = inc.ic.param.alpha[1,],
                              IP.0 =inc.ic.param[1,],
                              I.0 =inc.ic[1,],
                              IPP.100= inc.ic.param.process.driver[5,],
                              IPD.100 = inc.ic.param.d[5,],
                              IPA.100 = inc.ic.param.alpha[5,],
                              IP.100=inc.ic.param[5,],
                              I.100=inc.ic[5,],
                              year = 2018:2099)
  axis.name <- "Increment"
  }
  V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
  
  
  pred.sims.m <- reshape2::melt(pred.sims, id.vars = "year")
  pred.sims.class <- pred.sims.m %>% separate(col = variable, sep = "[.]", into = c("unc","lo")) %>%
    spread(key = lo, value = value)
  colnames(pred.sims.class) <- c("year", "uncertainty", "Low", "High")
  my_cols <- c("#1b9e77",
               "#d95f02",
               "black",
               "#7570b3", 
               "grey")
  
  pred.sims.class$uncertainty <- factor(pred.sims.class$uncertainty, levels = c("IPP","IPD","IPA", "IP", "I"))
  
 
  predY_plot <- ggplot(data = pred.sims.class, aes(x=year, fill = uncertainty))+
    geom_ribbon(aes(ymin=Low, ymax=High), color = "grey")+
    ylab(axis.name)+
    xlab("Year")+theme_bw()+
    scale_fill_manual(values = my_cols, name = NULL)+ theme(legend.position = "none", panel.grid = element_blank())
  
  predY_plot
  
  ##--------------------------------------------------------------
  #  Plot the proportion of uncertainty
  #--------------------------------------------------------------
  var_rel_preds <- as.data.frame(t(V.pred.sim.rel*100))
  var_rel_preds$x <- 1:nrow(var_rel_preds)
  
  
  # pretty colored plot:
  
  my_cols <- c("#1b9e77",
               "#d95f02",
               "black",
               "#7570b3", 
               "grey")
  tmpvar <- var_rel_preds
  tmpvar$year <- 2018:2099
  colnames(tmpvar) <- c( "Process Error","Driver Uncertainty","Plot random effect",
                         "Parameter Uncertainty", "Initial Conditions", "x", "year")
  variance.df <- tmpvar %>%
    gather(simtype, variance, -x, -year)
  
  variance.df$simtype <- factor(variance.df$simtype, levels = c("Process Error","Driver Uncertainty","Plot random effect", "Parameter Uncertainty", "Initial Conditions"))
  
  prop.var <- ggplot(variance.df, aes(x=year, fill = simtype))+
    geom_ribbon(aes(ymin=0, ymax=variance), color = "grey")+
    ylab(paste("% of total variance for ", axis.name))+    xlab("Year")+
    scale_fill_manual(values = my_cols, name = NULL)+#, 
                      #labels = c("Process error", "Driver uncertainty",  "Parameter uncertainty","alpha uncertainty", "Initial conditions"))+
    scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                       expand = c(0, 0))+
    theme_bw()+theme(panel.grid = element_blank())
  
  if(prop == TRUE){
    prop.var
  }else{
    predY_plot
  }
}




prop.inc<- plot.prop.variance.future.forecast.driver(m = 400, prop = TRUE, scenario = "rcp26", type = "ringwidth")
total.inc <- plot.prop.variance.future.forecast.driver(m = 400, prop = FALSE, scenario = "rcp26", type = "ringwidth")
prop.dbh <- plot.prop.variance.future.forecast.driver(m = 400, prop = TRUE, scenario = "rcp26", type = "dbh")
total.dbh <- plot.prop.variance.future.forecast.driver(m = 400, prop = FALSE, scenario = "rcp26", type = "dbh")
legend.colors <- get_legend(prop.inc)


png(height = 6, width = 10, units = "in", res = 300, "Uncertainty_partition_tree_400.png")
cowplot::plot_grid(
    cowplot::plot_grid(prop.inc+theme(legend.position = "none"), total.inc, 
                        prop.dbh+theme(legend.position = "none"), total.dbh, ncol = 2, align = "hv"),
                  legend.colors, ncol = 2, rel_widths = c(1, 0.25))

dev.off()


# -------------------Plot multipaged pdf-------------------------
multiplot.list <- list()


#system.time(for(i in 1:10){
  
  prop.inc<- plot.prop.variance.future.forecast.driver(m = i, prop = TRUE, scenario = "rcp26", type = "ringwidth")
  total.inc <- plot.prop.variance.future.forecast.driver(m = i, prop = FALSE, scenario = "rcp26", type = "ringwidth")
  prop.dbh <- plot.prop.variance.future.forecast.driver(m = i, prop = TRUE, scenario = "rcp26", type = "dbh")
  total.dbh <- plot.prop.variance.future.forecast.driver(m = i, prop = FALSE, scenario = "rcp26", type = "dbh")
  legend.colors <- get_legend(prop.inc)
  
  # now add the title
  title <- ggdraw() + 
    draw_label(
      paste("Uncertanity Partitioning for Tree ", i),
      fontface = 'bold',
      x = 0.25,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  
  
multiplot.list[[i]] <- cowplot::plot_grid(title,
  cowplot::plot_grid(
  cowplot::plot_grid(prop.inc+theme(legend.position = "none"), total.inc, 
                     prop.dbh+theme(legend.position = "none"), total.dbh, ncol = 2, align = "hv"),
  legend.colors, ncol = 2, rel_widths = c(1, 0.25)),
  ncol = 1, rel_heights = c(0.1, 1))


})
# for 10 trees:
# user  system elapsed 
# 51.335   2.349  54.587 

multiplot.unc <- function(i){
  
  prop.inc<- plot.prop.variance.future.forecast.driver(m = i, prop = TRUE, scenario = "rcp26", type = "ringwidth")
  total.inc <- plot.prop.variance.future.forecast.driver(m = i, prop = FALSE, scenario = "rcp26", type = "ringwidth")
  prop.dbh <- plot.prop.variance.future.forecast.driver(m = i, prop = TRUE, scenario = "rcp26", type = "dbh")
  total.dbh <- plot.prop.variance.future.forecast.driver(m = i, prop = FALSE, scenario = "rcp26", type = "dbh")
  legend.colors <- get_legend(prop.inc)
  
  # now add the title
  title <- ggdraw() + 
    draw_label(
      paste("Uncertanity Partitioning for Tree ", i),
      fontface = 'bold',
      x = 0.25,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  
  
  multiplot.list[[i]] <- cowplot::plot_grid(title,
                                            cowplot::plot_grid(
                                              cowplot::plot_grid(prop.inc+theme(legend.position = "none"), total.inc, 
                                                                 prop.dbh+theme(legend.position = "none"), total.dbh, ncol = 2, align = "hv"),
                                              legend.colors, ncol = 2, rel_widths = c(1, 0.25)),
                                            ncol = 1, rel_heights = c(0.1, 1))
  
  
}
system.time(lapply(1:100, multiplot.unc))

# for 10 trees:
# user  system elapsed 
# 50.239   0.788  51.323 

library(ggpubr)
# use ggarrange to make a list of what should be on each page (you can mix cowplot and ggarrange, because ggarrange is a wrapper for cowplot)
multi.page <- ggarrange( plotlist = multiplot.list,
                        nrow = 1, ncol = 1)
#returns a list of two pages with two plots per page. You can visualize each page as follow:
  
multi.page[[1]] # Visualize page 1
multi.page[[2]] # Visualize page 2

ggexport(plotlist = multi.page, filename = "Uncertainty_partition_all_trees.pdf",width = 10, height = 6)

#---------------------------------------------------------------------------------------
# breaking down the driver uncertainty
#---------------------------------------------------------------------------------------
plot.prop.variance.driver <- function(m, prop =TRUE, scenario = "rcp26", type = "dbh"){
  #-----------------------------------------------------------------------
  # Partitioning uncertainty
  #-----------------------------------------------------------------------
  # Create a function for our process model (linear state space model)
  
  iterate_statespace.dbh <- function( x = x.mat[,"x[1,45]"], m = m, betas.all, alpha, SDdbh, SDinc = 0, covariates) {
    
    #j <- 1
    
    
    # pseudocode for now
    tree.growth <- x +  alpha + betas.all$b0 + 
      betas.all$bSDI*covariates$SDI + 
      betas.all$bSDI_ppt*covariates$SDI*covariates$ppt + 
      betas.all$bX*(x-30) + 
      betas.all$bX2*(x-30)*(x-30) + 
      betas.all$bX_SDI*(x-30)*covariates$SDI + 
      betas.all$bX_ppt*covariates$ppt*(x-30) + 
      betas.all$bppt*covariates$ppt + 
      betas.all$btmax*covariates$tmax + 
      betas.all$bX_tmax*(x-30)*covariates$tmax + 
      betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
      betas.all$btmax_ppt*covariates$tmax*covariates$ppt 
    #tree.growth
    
    
    # Stochastic process model
    xpred <- rnorm(length(tree.growth), tree.growth, SDdbh) 
    
   
      xpred
    
  }
  alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
  
  alpha <- quantile(alphas[, alphaplotid],0.5)
  bSDI <- quantile(betas[,"betaSDI"],0.5)
  bSDI_ppt <- quantile(betas[,"betaSDI_wintP.wateryr"],0.5)
  bX <-  quantile(betas[,"betaX"],0.5)
  bX2 <- quantile(betas[,"betaX2"],0.5)
  bX_SDI <- quantile(betas[,"betaX_SDI"],0.5)
  bX_ppt <- quantile(betas[,"betaX_wintP.wateryr"],0.5)
  bppt <- quantile(betas[,"betawintP.wateryr"],0.5)
  btmax <- quantile(betas[,"betatmax.fallspr"] ,0.5)
  bX_tmax <- quantile(betas[,"betaX_tmax.fallspr"],0.5)
  bSDI_tmax <- quantile(betas[,"betaSDI_tmax.fallspr"],0.5)
  btmax_ppt <- quantile(betas[,"betatmax.fallspr_wintP.wateryr"],0.5)
  b0 <- quantile(B0, 0.5)
  
  betas.point <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                            bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  
  #scenario <- "rcp26"
  # code toe filter out future climate data for the tree id and the rcp:
  # filter out for the means
  future.proj <- ens.means[ens.means$id == m & ens.means$rcp == scenario, ]
  proj.ordered <- future.proj[order(future.proj$year),]
  
  # now filter for the full ensemble
  future.ens <- clim.ts.df.full[clim.ts.df.full$id == m & clim.ts.df.full$rcp == scenario,]
  ens.proj.ordered <-  future.ens[order( future.ens$year),]
  #ggplot(proj.ordered, aes(as.numeric(year), mean.ppt))+geom_point()+stat_smooth()
  # just use the ensemble means (w.out driver uncertainty)
  
  
  #---------------------------------------------------------------------------
  ##  breaking down total Driver uncertainty
  #---------------------------------------------------------------------------
  
  # use all of the parameter MCMCS:
  
  # 
  # just use the ensemble means (w.out driver uncertainty)
  ppt <- tmax <- SDI <- matrix(NA, nrow =length(57001:60300), ncol = 82 )
  for(i in 1:82){
    
    ppt[,i]<- rnorm(n = length(57001:60300) , mean = proj.ordered[i,]$mean.ppt, sd = proj.ordered[i,]$SD.ppt)
    tmax[,i]<- rnorm(n = length(57001:60300) , mean = proj.ordered[i,]$mean.tmax.fs, sd = proj.ordered[i,]$SD.tmax)
    SDI[,i]<- rep(cov.data[m, ]$SDI, length(57001:60300))
    
  }
  
 
  covariates <- list()
  covariates$SDI <- SDI
  covariates$ppt <- ppt
  covariates$tmax <- tmax
  
  #covariates <- list(SDI, ppt, tmax)
  
  time_steps <- 82
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = mean(x.mat[,paste0("x[", m,",", 45,"]")]), m = m, betas.all = betas.point, alpha = alpha, SDdbh = 0, covariates = data.frame(SDI = covariates$SDI[,t], 
                                                                                                                                                               ppt = covariates$ppt[,t], 
                                                                                                                                                               tmax = covariates$tmax[,t]))
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - mean(x.mat[,paste0("x[", m,",", 45,"]")])
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.point, alpha = alpha, SDdbh = 0, covariates = data.frame(SDI = covariates$SDI[,t], 
                                                                                                                                          ppt = covariates$ppt[,t], 
                                                                                                                                          tmax = covariates$tmax[,t]))
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - forecast[,t-1]
    }  }
  variancetotal.driver <- apply(forecast,2,var)
  forecast.all.driver <- apply(forecast, 2, quantile)
  
  variancetotal.driver.inc <- apply(inc,2,var)
  forecast.all.driver.inc <- apply(inc, 2, quantile)
  
  #---------------------------------------------------------------------------
  ##  breaking down Precipitation Driver uncertainty
  #---------------------------------------------------------------------------
  
  # use all of the parameter MCMCS:
  
  # 
  # just use the ensemble means (w.out driver uncertainty)
  ppt <- tmax <- SDI <- matrix(NA, nrow =length(57001:60300), ncol = 82 )
  for(i in 1:82){
    
    ppt[,i]<- rnorm(n = length(57001:60300) , mean = proj.ordered[i,]$mean.ppt, sd = proj.ordered[i,]$SD.ppt)
    tmax[,i]<- rep( proj.ordered[i,]$mean.tmax.fs, length(57001:60300))
    SDI[,i]<- rep(cov.data[m, ]$SDI, length(57001:60300))
    
  }
  
  
  covariates <- list()
  covariates$SDI <- SDI
  covariates$ppt <- ppt
  covariates$tmax <- tmax
  
  #covariates <- list(SDI, ppt, tmax)
  
  time_steps <- 82
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = mean(x.mat[,paste0("x[", m,",", 45,"]")]), m = m, betas.all = betas.point, alpha = alpha, SDdbh = 0, covariates = data.frame(SDI = covariates$SDI[,t], 
                                                                                                                                                                 ppt = covariates$ppt[,t], 
                                                                                                                                                                 tmax = covariates$tmax[,t]))
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - mean(x.mat[,paste0("x[", m,",", 45,"]")])
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.point, alpha = alpha, SDdbh = 0, covariates = data.frame(SDI = covariates$SDI[,t], 
                                                                                                                                            ppt = covariates$ppt[,t], 
                                                                                                                                            tmax = covariates$tmax[,t]))
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - forecast[,t-1]
      
    }  }
  variance.ppt.driver <- apply(forecast,2,var)
  forecast.ppt.driver <- apply(forecast, 2, quantile)
  
  
  variance.ppt.driver.inc <- apply(inc,2,var)
  forecast.ppt.driver.inc <- apply(inc, 2, quantile)
  
  
  # combine variances:
  if(type == "dbh"){
  V.pred.sim     <- rbind(variance.ppt.driver, variancetotal.driver)
  V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
  
  pred.sims     <- data.frame(Precipitation.0 =forecast.ppt.driver[1,],
                              Temperature.0 = forecast.all.driver[1,],
                              Precipitation.100 =forecast.ppt.driver[5,],
                              Temperature.100 =forecast.all.driver[5,],
                             year = 2018:2099)
                              
  
  axis.name <- "Diameter"
  }else{
    V.pred.sim     <- rbind(variance.ppt.driver.inc, variancetotal.driver.inc)
    V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
    pred.sims     <- data.frame(Precipitation.0 =forecast.ppt.driver.inc[1,],
                                Temperature.0 = forecast.all.driver.inc[1,],
                                Precipitation.100 =forecast.ppt.driver.inc[5,],
                                Temperature.100 =forecast.all.driver.inc[5,],
                                year = 2018:2099)
    
    axis.name <- "Increment"
  }
  

  
  pred.sims.m <- reshape2::melt(pred.sims, id.vars = "year")
  pred.sims.class <- pred.sims.m %>% separate(col = variable, sep = "[.]", into = c("unc","lo")) %>%
    spread(key = lo, value = value)
  colnames(pred.sims.class) <- c("year", "uncertainty", "Low", "High")
 
  
  pred.sims.class$uncertainty <- factor(pred.sims.class$uncertainty, levels = c("Temperature", "Precipitation"))
  driver_cols <- c("#b2182b","#2166ac")
  
  predY_plot <- ggplot(data = pred.sims.class, aes(x=year, fill = uncertainty))+
    geom_ribbon(aes(ymin=Low, ymax=High), color = "grey")+
    ylab(axis.name)+
    xlab("Year")+theme_bw()+
    scale_fill_manual(values = driver_cols, name = NULL)+ theme(legend.position = "none", panel.grid = element_blank())
  
  # # combine forecasts:
  # pred.sims     <- data.frame(IPP.0 = forecast.ic.param.process.driver[1,],
  #                             
  #                             IPD.0 = forecast.ic.param.d[1,],
  #                             IP.0 =forecast.ic.param[1,],
  #                             I.0 =forecast.ic[1,],
  #                             IPP.100= forecast.ic.param.process.driver[5,],
  #                             IPD.100 = forecast.ic.param.d[5,],
  #                             IP.100=forecast.ic.param[5,],
  #                             I.100=forecast.ic[5,],
  #                             year = 2018:2099)
  # 
  # pred.sims.m <- reshape2::melt(pred.sims, id.vars = "year")
  # #pred.sims.m %>% separate(variable, sep = ".", into = c(" hilo"))
  # my_cols <- c("#1b9e77",# green
  #              "#d95f02",# orange
  #              "#7570b3", # purple
  #              "grey")
  # #pred.sims.m <- pred.sims
  # predY_plot <- ggplot(data=pred.sims, aes(x=year))+
  #   geom_ribbon(aes(ymin=IPP.0, ymax=IPP.100), fill=my_cols[1])+
  #   geom_ribbon(aes(ymin=IPD.0, ymax=IPD.100), fill=my_cols[2])+
  #   
  #   geom_ribbon(aes(ymin=IP.0, ymax=IP.100), fill=my_cols[3])+
  #   geom_ribbon(aes(ymin=I.0, ymax=I.100+0.001), fill=my_cols[4])+
  #   
  #   
  #   ylab("Predicted DBH")+
  #   xlab("Year")+
  #   #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
  #   #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
  #   #scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
  #   theme_bw()
  # predY_plot
  
  ####
  ####  PLOT THE FORECASTING UNCERTAINTY PARTITION -------------------------------
  ####
  var_rel_preds <- as.data.frame(t(V.pred.sim.rel*100))
  var_rel_preds$x <- 1:nrow(var_rel_preds)
  #my_cols <- c("black", "grey55", "grey70")
  my_cols <- c("#1b9e77",
               "#d95f02",
               "#7570b3")
  
  # pretty colored plot:
  
  my_cols <- c("#1b9e77",
               "#d95f02",
               "black",
               "#7570b3", 
               "grey")
  tmpvar <- var_rel_preds
  tmpvar$year <- 2018:2099
  colnames(tmpvar) <- c( "Precipitation Uncertainty", "Temperature Uncertainty", "x", "year")
  variance.df <- tmpvar %>%
    gather(simtype, variance, -x, -year)
  
  variance.df$simtype <- factor(variance.df$simtype, levels = c("Temperature Uncertainty", "Precipitation Uncertainty"))
  
  driver_cols <- c("#b2182b","#2166ac")
  
  prop.var <- ggplot(variance.df, aes(x=year, fill = simtype))+
    geom_ribbon(aes(ymin=0, ymax=variance), color = "grey")+
    ylab(paste("% of driver uncertainty \n for", axis.name))+
    xlab("Year")+
    scale_fill_manual(values = driver_cols, name = NULL)+#, 
    #labels = c("Process error", "Driver uncertainty",  "Parameter uncertainty","alpha uncertainty", "Initial conditions"))+
    scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                       expand = c(0, 0))+
    theme_bw()+theme(panel.grid=element_blank())
  
  if(prop== TRUE){
    prop.var
  }else{
    predY_plot
  }
  
}
tree.n <- 70
d.26 <- plot.prop.variance.driver(m = tree.n, prop =TRUE,  scenario = "rcp26", type = "dbh")#+ggtitle(paste0("Contribution to  Driver Uncertainty in DBH for Tree",i))
r.26 <- plot.prop.variance.driver(m = tree.n,  prop =TRUE,scenario = "rcp26", type = "ringwidth")#+ggtitle(paste0("Contribution to  Driver Uncertainty in DBH for Tree",i))


plot.prop.variance.driver(m = tree.n, prop =FALSE,  scenario = "rcp26", type = "dbh")
plot.prop.variance.driver(m = tree.n, prop =FALSE,  scenario = "rcp26", type = "ringwidth")

plot.prop.variance.driver(m = 10, prop =FALSE,  scenario = "rcp26", type = "dbh")
plot.prop.variance.driver(m = 10, prop =FALSE,  scenario = "rcp26", type = "ringwidth")
plot.prop.variance.driver(m = 10, prop =FALSE,  scenario = "rcp85", type = "dbh")
plot.prop.variance.driver(m = 10, prop =FALSE,  scenario = "rcp85", type = "ringwidth")


d.45<- plot.prop.variance.driver(m = tree.n, prop =TRUE, scenario = "rcp45", type = "dbh")#+ggtitle(paste0("Contribution to  Driver Uncertainty in DBH for Tree",i))
r.45<- plot.prop.variance.driver(m = tree.n,  prop =TRUE,scenario = "rcp45", type = "ringwidth")#+ggtitle(paste0("Contribution to  Driver Uncertainty in DBH for Tree",i))

d.60<- plot.prop.variance.driver(m = tree.n, prop =TRUE, scenario = "rcp60", type = "dbh")#+ggtitle(paste0("Contribution to  Driver Uncertainty in DBH for Tree",i))
r.60<-plot.prop.variance.driver(m = tree.n, prop =TRUE, scenario = "rcp60", type = "ringwidth")#+ggtitle(paste0("Contribution to  Driver Uncertainty in DBH for Tree",i))

d.85<- plot.prop.variance.driver(m = tree.n,  prop =TRUE,scenario = "rcp85", type = "dbh")#+ggtitle(paste0("Contribution to  Driver Uncertainty in DBH for Tree",i))
r.85<- plot.prop.variance.driver(m = tree.n,  prop =TRUE, scenario = "rcp85", type = "ringwidth")#+ggtitle(paste0("Contribution to  Driver Uncertainty in DBH for Tree",i))

driver.legend <- get_legend(d.85)
png(height = 5, width = 10, units = "in", res = 300, paste0("Driver_uncertainty_scenarios_tree_",tree.n,".png"))
cowplot::plot_grid(
cowplot::plot_grid(d.26+theme(legend.position = "none"), d.45+theme(legend.position = "none"), d.60+theme(legend.position = "none"), d.85+theme(legend.position = "none"), 
                   r.26+theme(legend.position = "none"), r.45+theme(legend.position = "none"), r.60+theme(legend.position = "none"), r.85+theme(legend.position = "none"), 
                   ncol = 4, align = "hv"), 
driver.legend,
nrow = 2, rel_heights = c(1, 0.25))
dev.off()


# plot out a pdf with the driver uncertainty close ups:

multiplot.list.param <- list()
# make a pdf with the first ten trees:
multiplot.driver.unc <- function(i){
  
  prop.inc<- plot.prop.variance.driver(m = i, prop = TRUE, scenario = "rcp26", type = "ringwidth")
  total.inc <- plot.prop.variance.driver(m = i, prop = FALSE, scenario = "rcp26", type = "ringwidth")
  prop.dbh <- plot.prop.variance.driver(m = i, prop = TRUE, scenario = "rcp26", type = "dbh")
  total.dbh <- plot.prop.variance.driver(m = i, prop = FALSE, scenario = "rcp26", type = "dbh")
  legend.colors <- get_legend(prop.inc)
  
  # now add the title
  title <-ggdraw() + 
    draw_label(
      paste("Driver Uncertanity Partitioning for Tree ", i),
      fontface = 'bold',
      x = 0.25,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  
  
  multiplot <- cowplot::plot_grid(title,
                                  cowplot::plot_grid(
                                    cowplot::plot_grid(prop.inc+theme(legend.position = "none"), total.inc, 
                                                       prop.dbh+theme(legend.position = "none"), total.dbh, ncol = 2, align = "hv"),
                                    legend.colors, ncol = 2, rel_widths = c(1, 0.25)),
                                  ncol = 1, rel_heights = c(0.1, 1))
  multiplot
  
}
multiplot.list.driver <- list()
system.time(multiplot.list.param <- lapply(1:100, multiplot.driver.unc))

# for 10 trees:
# user  system elapsed 
# 53.038   0.956  54.027 


# use ggarrange to make a list of what should be on each page (you can mix cowplot and ggarrange, because ggarrange is a wrapper for cowplot)
multi.page.param <- ggarrange( plotlist = multiplot.list.param,
                                nrow = 1, ncol = 1)
#returns a list of two pages with two plots per page. You can visualize each page as follow:

multi.page[[1]] # Visualize page 1
multi.page[[2]] # Visualize page 2

ggexport(plotlist = multi.page.param, filename = "Driver_Uncertainty_partition_10_trees.pdf",width = 10, height = 6)



#---------------------------------------------------------------------------------------
# breaking down the parameter uncertainty 
#---------------------------------------------------------------------------------------
plot.prop.parameter.variance <- function(m, prop =TRUE, scenario = "rcp26", type = "dbh"){
  #-----------------------------------------------------------------------
  # Partitioning uncertainty
  #-----------------------------------------------------------------------
  # Create a function for our process model (linear state space model)
  
  iterate_statespace.dbh <- function( x = x.mat[,"x[1,45]"], m = m, betas.all, alpha, SDdbh, SDinc = 0, covariates) {

    # growth model
    tree.growth <- x +  alpha + betas.all$b0 + 
      betas.all$bSDI*covariates$SDI + 
      betas.all$bSDI_ppt*covariates$SDI*covariates$ppt + 
      betas.all$bX*(x-30) + 
      betas.all$bX2*(x-30)*(x-30) + 
      betas.all$bX_SDI*(x-30)*covariates$SDI + 
      betas.all$bX_ppt*covariates$ppt*(x-30) + 
      betas.all$bppt*covariates$ppt + 
      betas.all$btmax*covariates$tmax + 
      betas.all$bX_tmax*(x-30)*covariates$tmax + 
      betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
      betas.all$btmax_ppt*covariates$tmax*covariates$ppt 
    #tree.growth
    
    
    # Stochastic process model
    xpred <- rnorm(length(tree.growth), tree.growth, SDdbh) 
    
   
      xpred
  
  }
 
  # use all of the parameter MCMCS:
  
  # 
  # just use the ensemble means (w.out driver uncertainty)
  #scenario <- "rcp26"
  # code toe filter out future climate data for the tree id and the rcp:
  # filter out for the means
  future.proj <- ens.means[ens.means$id == m & ens.means$rcp == scenario, ]
  proj.ordered <- future.proj[order(future.proj$year),]
  
  # now filter for the full ensemble
  future.ens <- clim.ts.df.full[clim.ts.df.full$id == m & clim.ts.df.full$rcp == scenario,]
  ens.proj.ordered <-  future.ens[order( future.ens$year),]
  #ggplot(proj.ordered, aes(as.numeric(year), mean.ppt))+geom_point()+stat_smooth()
  # just use the ensemble means (w.out driver uncertainty)
  ppt <- proj.ordered$mean.ppt
  tmax <- proj.ordered$mean.tmax.fs
  #ppt <- time_data$wintP.wateryr[m,]
  #tmax <- time_data$tmax.fallspr[m,]
  SDI <- rep(cov.data[m, ]$SDI, length(ppt))
  #x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  time_steps <- 82
  nMCMC <- length(x.mat[,"x[1,45]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  #---------------------------------------------------------------------------
  # Uncertainty from betaSDI
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
    alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
  
  alpha <- rep(quantile(alphas[, alphaplotid],0.5), length(57001:60300))
  bSDI <- rep(quantile(betas[,"betaSDI"],0.5), length(57001:60300))
  bSDI_ppt <- rep(quantile(betas[,"betaSDI_wintP.wateryr"],0.5), length(57001:60300))
  bX <-  rep(quantile(betas[,"betaX"],0.5), length(57001:60300))
  bX2 <- rep(quantile(betas[,"betaX2"],0.5), length(57001:60300))
  bX_SDI <- rep(quantile(betas[,"betaX_SDI"],0.5), length(57001:60300))
  bX_ppt <- rep(quantile(betas[,"betaX_wintP.wateryr"],0.5), length(57001:60300))
  bppt <- rep(quantile(betas[,"betawintP.wateryr"],0.5), length(57001:60300))
  btmax <- rep(quantile(betas[,"betatmax.fallspr"] ,0.5), length(57001:60300))
  bX_tmax <- rep(quantile(betas[,"betaX_tmax.fallspr"],0.5), length(57001:60300))
  bSDI_tmax <- rep(quantile(betas[,"betaSDI_tmax.fallspr"],0.5), length(57001:60300))
  btmax_ppt <- rep(quantile(betas[,"betatmax.fallspr_wintP.wateryr"],0.5), length(57001:60300))
  b0 <- rep(quantile(B0, 0.5), length(57001:60300))
  
  
  bSDI <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betaSDI"]), sd = sd(betas[57001:60300,"betaSDI"]))
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  ppt <- proj.ordered$mean.ppt
  tmax <- proj.ordered$mean.tmax.fs
  #ppt <- time_data$wintP.wateryr[m,]
  #tmax <- time_data$tmax.fallspr[m,]
  SDI <- rep(cov.data[m, ]$SDI, length(ppt))
  #x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = mean(x.mat[,paste0("x[", m,",", 45,"]")]), m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- dbh.pred - mean(x.mat[,paste0("x[", m,",", 45,"]")])
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t]-forecast[,t-1]
    }  }
  
  varianceSDI <- apply(forecast,2,var)
  forecast.SDI <- apply(forecast, 2, quantile)
  
  varianceSDI.inc <- apply(inc,2,var)
  forecast.SDI.inc <- apply(inc, 2, quantile)
  
  #---------------------------------------------------------------------------
  # Uncertainty from betaX and X2
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:

  bX <-  rnorm(length(57001:60300), mean = betas[57001:60300,"betaX"], sd = sd(betas[57001:60300,"betaX"]))
  bX2 <- rnorm(length(57001:60300), mean =betas[57001:60300,"betaX2"], sd = sd(betas[57001:60300,"betaX2"]))
  #bSDI <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betaSDI"]), sd = sd(betas[57001:60300,"betaSDI"]))
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  ppt <- proj.ordered$mean.ppt
  tmax <- proj.ordered$mean.tmax.fs
  #ppt <- time_data$wintP.wateryr[m,]
  #tmax <- time_data$tmax.fallspr[m,]
  SDI <- rep(cov.data[m, ]$SDI, length(ppt))
  #x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = mean(x.mat[,paste0("x[", m,",", 45,"]")]), m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t] - mean(x.mat[,paste0("x[", m,",", 45,"]")])
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t]<-forecast[,t] - forecast[,t-1] 
      
    }  }
  
  varianceSDI.X <- apply(forecast,2,var)
  forecast.SDI.X <- apply(forecast, 2, quantile)
  
  varianceSDI.X.inc <- apply(inc,2,var)
  forecast.SDI.X.inc <- apply(inc, 2, quantile)
  #---------------------------------------------------------------------------
  # Uncertainty from betaPrecip 
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  
  bppt <-  rnorm(length(57001:60300), mean = betas[57001:60300,"betawintP.wateryr"], sd = sd(betas[57001:60300,"betawintP.wateryr"]))
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  ppt <- proj.ordered$mean.ppt
  tmax <- proj.ordered$mean.tmax.fs
  #ppt <- time_data$wintP.wateryr[m,]
  #tmax <- time_data$tmax.fallspr[m,]
  SDI <- rep(cov.data[m, ]$SDI, length(ppt))
  #x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = mean(x.mat[,paste0("x[", m,",", 45,"]")]), m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - mean(x.mat[,paste0("x[", m,",", 45,"]")])
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - forecast[,t-1]
    }  }
  
  varianceSDI.X.precip <- apply(forecast,2,var)
  forecast.SDI.X.precip <- apply(forecast, 2, quantile)
  
  varianceSDI.X.precip.inc <- apply(inc,2,var)
  forecast.SDI.X.precip.inc <- apply(inc, 2, quantile)
  #---------------------------------------------------------------------------
  # Uncertainty from betaTmax
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  
  btmax <-  rnorm(length(57001:60300), mean = betas[57001:60300,"betatmax.fallspr"], sd = sd(betas[57001:60300,"betatmax.fallspr"]))
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  ppt <- proj.ordered$mean.ppt
  tmax <- proj.ordered$mean.tmax.fs
  #ppt <- time_data$wintP.wateryr[m,]
  #tmax <- time_data$tmax.fallspr[m,]
  SDI <- rep(cov.data[m, ]$SDI, length(ppt))
  #x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = mean(x.mat[,paste0("x[", m,",", 45,"]")]), m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - mean(x.mat[,paste0("x[", m,",", 45,"]")])
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - forecast[,t-1]
    }  }
  
  varianceSDI.X.precip.tmax <- apply(forecast,2,var)
  forecast.SDI.X.precip.tmax <- apply(forecast, 2, quantile)
  
  
  varianceSDI.X.precip.tmax.inc <- apply(inc,2,var)
  forecast.SDI.X.precip.tmax.inc <- apply(inc, 2, quantile)
  
  
  #---------------------------------------------------------------------------
  # Uncertainty from T and P interactions
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  
  btmax_ppt <-  rnorm(length(57001:60300), mean = betas[57001:60300,"betatmax.fallspr_wintP.wateryr"], sd = sd(betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]))
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  ppt <- proj.ordered$mean.ppt
  tmax <- proj.ordered$mean.tmax.fs
  #ppt <- time_data$wintP.wateryr[m,]
  #tmax <- time_data$tmax.fallspr[m,]
  SDI <- rep(cov.data[m, ]$SDI, length(ppt))
  #x <- x.mat[,"x[1,45]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = mean(x.mat[,paste0("x[", m,",", 45,"]")]), m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - mean(x.mat[,paste0("x[", m,",", 45,"]")])
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - forecast[,t-1]
    }  }
  
  
  varianceSDI.X.precip.tmax.int <- apply(forecast,2,var)
  forecast.SDI.X.precip.tmax.int <- apply(forecast, 2, quantile)
  
  varianceSDI.X.precip.tmax.int.inc <- apply(inc,2,var)
  forecast.SDI.X.precip.tmax.int.inc <- apply(inc, 2, quantile)

  
  #---------------------------------------------------------------------------
  # Uncertainty from all other Two way interactions
  #---------------------------------------------------------------------------
  alpha = quantile(alphas[, alphaplotid],0.5)
  #alpha <- rnorm(length(57001:60300), mean = mean(alphas[57001:60300, alphaplotid]), sd = sd(alphas[57001:60300, alphaplotid]))
 # bSDI <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betaSDI"]), sd = sd(betas[57001:60300,"betaSDI"]))
  bSDI_ppt <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betaSDI_wintP.wateryr"]), sd = sd(betas[57001:60300,"betaSDI_wintP.wateryr"]))
 # bX <-  rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betaX"]), sd = sd(betas[57001:60300,"betaX"]))
  #bX2 <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betaX2"]), sd = sd(betas[57001:60300,"betaX2"]))
  bX_SDI <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betaX_SDI"]), sd = sd(betas[57001:60300,"betaX_SDI"]))
  bX_ppt <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betaX_wintP.wateryr"]), sd = sd(betas[57001:60300,"betaX_wintP.wateryr"]))
#  bppt <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betawintP.wateryr"]), sd = sd(betas[57001:60300,"betawintP.wateryr"]))
 # btmax <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betatmax.fallspr"]), sd = sd(betas[57001:60300,"betatmax.fallspr"]))
  bX_tmax <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betaX_tmax.fallspr"]), sd = sd(betas[57001:60300,"betaX_tmax.fallspr"]))
  bSDI_tmax <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betaSDI_tmax.fallspr"]), sd = sd(betas[57001:60300,"betaSDI_tmax.fallspr"]))
  #btmax_ppt <- rnorm(length(57001:60300), mean = mean(betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]), sd = sd(betas[57001:60300,"betaSDI_tmax.fallspr"]))
  b0 <- rnorm(length(57001:60300), mean = mean(B0[57001:60300]), sd = sd(B0[57001:60300]))
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = mean(x.mat[,paste0("x[", m,",", 45,"]")]), m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - mean(x.mat[,paste0("x[", m,",", 45,"]")])
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - forecast[,t-1]
    }  }
  
  varianceSDI.X.precip.tmax.int.two.way <- apply(forecast,2,var)
  forecast.SDI.X.precip.tmax.int.two.way <- apply(forecast, 2, quantile)
  
  varianceSDI.X.precip.tmax.int.two.way.inc <- apply(inc,2,var)
  forecast.SDI.X.precip.tmax.int.two.way.inc <- apply(inc, 2, quantile)
  #---------------------------------------------------------------------------
  # Uncertainty from all paramters (including alpha plot)
  #---------------------------------------------------------------------------
  alpha <- rnorm(length(57001:60300), mean = mean(alphas[57001:60300, alphaplotid]), sd = sd(alphas[57001:60300, alphaplotid]))
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = mean(x.mat[,paste0("x[", m,",", 45,"]")]), m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - mean(x.mat[,paste0("x[", m,",", 45,"]")])
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - forecast[,t-1]
    }  }
  
  
  varianceall.params <- apply(forecast,2,var)
 forecastall.params <- apply(forecast, 2, quantile)
 
 
 varianceall.params.inc<- apply(inc,2,var)
 forecastall.params.inc <- apply(inc, 2, quantile)
 #---------------------------------------------------------------------------------- 
  # combine variances:
 
 if(type == "dbh"){
  V.pred.sim     <- rbind( varianceSDI, varianceSDI.X, varianceSDI.X.precip, varianceSDI.X.precip.tmax, 
                          varianceSDI.X.precip.tmax.int, varianceSDI.X.precip.tmax.int.two.way, varianceall.params)
  V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
  
  pred.sims     <- data.frame(SDI.0 =forecast.SDI[1,],
                              X.0 = forecast.SDI.X[1,],
                              Precip.0 =forecast.SDI.X.precip[1,],
                             tmax.0 =forecast.SDI.X.precip.tmax[1,],
                              PTint.0= forecast.SDI.X.precip.tmax.int[1,],
                              twoway.0 = forecast.SDI.X.precip.tmax.int.two.way[1,],
                              all.0=forecastall.params[1,],
                             
                             SDI.100 =forecast.SDI[5,],
                             X.100 = forecast.SDI.X[5,],
                             Precip.100 =forecast.SDI.X.precip[5,],
                             tmax.100 =forecast.SDI.X.precip.tmax[5,],
                             PTint.100= forecast.SDI.X.precip.tmax.int[5,],
                             twoway.100 = forecast.SDI.X.precip.tmax.int.two.way[5,],
                             all.100=forecastall.params[5,],
                              year = 2018:2099)
  axis.name <- "diameter"
  
 }else{
   V.pred.sim     <- rbind( varianceSDI.inc, varianceSDI.X.inc, varianceSDI.X.precip.inc, varianceSDI.X.precip.tmax.inc, 
                            varianceSDI.X.precip.tmax.int.inc, varianceSDI.X.precip.tmax.int.two.way.inc, varianceall.params.inc)
   V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
   
   pred.sims     <- data.frame(SDI.0 =forecast.SDI.inc[1,],
                               X.0 = forecast.SDI.X.inc[1,],
                               Precip.0 =forecast.SDI.X.precip.inc[1,],
                               tmax.0 =forecast.SDI.X.precip.tmax.inc[1,],
                               PTint.0= forecast.SDI.X.precip.tmax.int.inc[1,],
                               twoway.0 = forecast.SDI.X.precip.tmax.int.two.way.inc[1,],
                               all.0=forecastall.params.inc[1,],
                               
                               SDI.100 =forecast.SDI.inc[5,],
                               X.100 = forecast.SDI.X.inc[5,],
                               Precip.100 =forecast.SDI.X.precip.inc[5,],
                               tmax.100 =forecast.SDI.X.precip.tmax.inc[5,],
                               PTint.100= forecast.SDI.X.precip.tmax.int.inc[5,],
                               twoway.100 = forecast.SDI.X.precip.tmax.int.two.way.inc[5,],
                               all.100=forecastall.params.inc[5,],
                               year = 2018:2099)
   axis.name <- "Increment"
   
 }
 
 pred.sims.m <- reshape2::melt(pred.sims, id.vars = "year")
 pred.sims.class <- pred.sims.m %>% separate(col = variable, sep = "[.]", into = c("unc","lo")) %>%
   spread(key = lo, value = value)
 colnames(pred.sims.class) <- c("year", "uncertainty", "Low", "High")
 my_cols <- c("#1b9e77",
              "#d95f02",
              "#7570b3",
              "#e7298a",
              "#66a61e",
              "#e6ab02",
              "#a6761d")
 
 pred.sims.class$uncertainty <- factor(pred.sims.class$uncertainty, levels = c("all","twoway","PTint", "tmax", "Precip", "X", "SDI"))
 
 
 predY_plot <- ggplot(data = pred.sims.class, aes(x=year, fill = uncertainty))+
   geom_ribbon(aes(ymin=Low, ymax=High), color = "grey")+
   ylab(axis.name)+
   xlab("Year")+theme_bw()+
   scale_fill_manual(values = my_cols, name = NULL)+ theme(legend.position = "none", panel.grid = element_blank())
 
 
 
 
  # 
  ####
  ####  PLOT THE FORECASTING UNCERTAINTY PARTITION -------------------------------
  ####
  var_rel_preds <- as.data.frame(t(V.pred.sim.rel*100))
  var_rel_preds$x <- 1:nrow(var_rel_preds)
  #my_cols <- c("black", "grey55", "grey70")
  my_cols <- c("#1b9e77",
    "#d95f02",
    "#7570b3",
    "#e7298a",
    "#66a61e",
    "#e6ab02",
    "#a6761d")
  
  # pretty colored plot:
  
  
  tmpvar <- var_rel_preds
  tmpvar$year <- 2018:2099
  colnames(tmpvar) <- c( "SDI","DBH","Precip",
                         "Tmax", "PrecipxTmax", "all other 2-way interactions", "plot random effect","x", "year")
  variance.df <- tmpvar %>%
    gather(simtype, variance, -x, -year)
  
  variance.df$simtype <- factor(variance.df$simtype, levels = rev(c( "SDI","DBH", "Precip","Tmax",
                                                                 "PrecipxTmax", "all other 2-way interactions", "plot random effect"))
  )
  
  prop.var <- ggplot(variance.df, aes(x=year, fill = simtype))+
    geom_ribbon(aes(ymin=0, ymax=variance), color = "black")+
    ylab(paste("% of parameter variance \n for", axis.name))+
    xlab("Year")+
    scale_fill_manual(values = my_cols, name = NULL)+#, 
    #labels = c("Process error", "Driver uncertainty",  "Parameter uncertainty","alpha uncertainty", "Initial conditions"))+
    scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                       expand = c(0, 0))+
    theme_bw()+theme(panel.grid = element_blank())
  
  if(prop == TRUE){
    prop.var 
  }else{
    predY_plot
  }

}
plot.prop.parameter.variance(m = 8, prop = TRUE, scenario = "rcp60", type = "dbh")#+ggtitle(paste0("Contribution to  Driver Uncertainty in DBH for Tree",i))
plot.prop.parameter.variance(m = 8, prop = FALSE, scenario = "rcp60", type = "dbh")
plot.prop.parameter.variance(m = 8, prop = TRUE, scenario = "rcp60", type = "ringwidth")
plot.prop.parameter.variance(m = 8, prop = FALSE, scenario = "rcp60", type = "ringwidth")


plot.prop.parameter.variance(m = 30,  scenario = "rcp60", type = "dbh")+ggtitle(paste0("Contribution to  Driver Uncertainty in DBH for Tree",i))
plot.prop.parameter.variance(m = 30,  scenario = "rcp60", type = "ringwidth")+ggtitle(paste0("Contribution to  Driver Uncertainty in DBH for Tree",i))

multiplot.list.param <- list()
# make a pdf with the first ten trees:
multiplot.param.unc <- function(i){
  
  prop.inc<- plot.prop.parameter.variance(m = i, prop = TRUE, scenario = "rcp26", type = "ringwidth")
  total.inc <- plot.prop.parameter.variance(m = i, prop = FALSE, scenario = "rcp26", type = "ringwidth")
  prop.dbh <- plot.prop.parameter.variance(m = i, prop = TRUE, scenario = "rcp26", type = "dbh")
  total.dbh <- plot.prop.parameter.variance(m = i, prop = FALSE, scenario = "rcp26", type = "dbh")
  legend.colors <- get_legend(prop.inc)
  
  # now add the title
  title <-ggdraw() + 
    draw_label(
      paste("Parameter Uncertanity Partitioning for Tree ", i),
      fontface = 'bold',
      x = 0.25,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  
  
  multiplot <- cowplot::plot_grid(title,
                                            cowplot::plot_grid(
                                              cowplot::plot_grid(prop.inc+theme(legend.position = "none"), total.inc, 
                                                                 prop.dbh+theme(legend.position = "none"), total.dbh, ncol = 2, align = "hv"),
                                              legend.colors, ncol = 2, rel_widths = c(1, 0.25)),
                                            ncol = 1, rel_heights = c(0.1, 1))
  multiplot
  
}
system.time(multiplot.list.param <- lapply(1:10, multiplot.param.unc))

# for 10 trees:
# user  system elapsed 
# 53.038   0.956  54.027 


# use ggarrange to make a list of what should be on each page (you can mix cowplot and ggarrange, because ggarrange is a wrapper for cowplot)
multi.page.param2 <- ggarrange( plotlist = multiplot.list.param,
                         nrow = 1, ncol = 1)
#returns a list of two pages with two plots per page. You can visualize each page as follow:

multi.page[[1]] # Visualize page 1
multi.page[[2]] # Visualize page 2

ggexport(plotlist = multi.page.param2, filename = "Parameter_Uncertainty_partition_10_trees.pdf",width = 10, height = 6)
