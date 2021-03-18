# script to partition out uncertainty in future forecasts:

# basic idea: 
# Use posteriors to forecast:
# 1. Initial Condition Uncertainty
# 2. Parameter Uncertainty
# 3. Process Uncertainty
# 4. Driver uncertainty (?)

stage2 = FALSE
output.base.name <- "SDI_SI.norand.X.resampled"
setwd("/home/rstudio")

# assuming that we already ran the conditional_effects_mcmc_plots script, and that output.base.name is in our env
#jags.comb.params <- readRDS(file=paste0("data/IGF",output.base.name,".rds"))
#jags.comb.params <- readRDS(url("https://de.cyverse.org/dl/d/FCC8972B-4F82-4BF5-9689-905B0B63E50F/IGFSDI_noSI.rand.X.nadapt.5000.rds"))
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
xval.ests<- readRDS(paste0("data/IGF_xvals_", output.base.name,".rds"))
xval.ests<- readRDS(paste0("data/IGF_xvals_", "SDI_SI.norand.X.nadapt.5000",".rds"))

x.mat <- as.matrix(xval.ests)
x.ci      <- apply(x.mat , 2, quantile, c(0.025, 0.5, 0.975))
x.ci[, "x[1,24]"]
hist(x.mat[,"x[4,53]"])

# make sure we have the average climate conditions for each plot/tree:
# We have plot MAP and MAT standarized in cov.data:
jags.new <- readRDS(paste0("jags.new.", output.base.name, ".rds"))
cov.data <- jags.new$cov.data


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
# # Create a function for our process model (linear state space model)
# 
# 
# 
# 
# 
# plot.prop.variance <- function(m){
#   #-----------------------------------------------------------------------
#   # Partitioning uncertainty
#   #-----------------------------------------------------------------------
#   # Create a function for our process model (linear state space model)
#   
#   iterate_statespace <- function( x = x.mat[,"x[1,45]"], m = 1, betas.all, alpha, SDinc, covariates) {
#     
#     j <- 1
#     
#     # alpha <- alphas[j, alphaplotid] 
#     # bSDI <- betas[j,"betaSDI"]
#     # bSDI_ppt <- betas[j,"betaSDI_wintP.wateryr"]
#     # bX <-  betas[j,"betaX"]
#     # bX2 <- betas[j,"betaX2"]
#     # bX_SDI <- betas[j,"betaX_SDI"]
#     # bX_ppt <- betas[j,"betaX_wintP.wateryr"]
#     # bppt <- betas[j,"betawintP.wateryr"]
#     # btmax <- betas[j,"betatmax.fallspr"] 
#     # bX_tmax <- betas[j,"betaX_tmax.fallspr"]
#     # bSDI_tmax <- betas[j,"betaSDI_tmax.fallspr"]
#     # btmax_ppt <- betas[j,"betatmax.fallspr_wintP.wateryr"]
#     # 
#     
#     # pseudocode for now
#     tree.growth <- alpha + betas.all$b0 + 
#       betas.all$bSDI*covariates$SDI + 
#       betas.all$bSDI_ppt*covariates$SDI*covariates$ppt + 
#       betas.all$bX*(x-30) + 
#       betas.all$bX2*(x-30)*(x-30) + 
#       betas.all$bX_SDI*(x-30)*covariates$SDI + 
#       betas.all$bX_ppt*covariates$ppt*(x-30) + 
#       betas.all$bppt*covariates$ppt + 
#       betas.all$btmax*covariates$tmax + 
#       betas.all$bX_tmax*(x-30)*covariates$tmax + 
#       betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
#       betas.all$btmax_ppt*covariates$tmax*covariates$ppt 
#     #tree.growth
#     
#     
#     # Stochastic process model
#     ypred <- rnorm(length(tree.growth), tree.growth, SDinc) 
#     
#     ypred
#   }
#   alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
#   
#   alpha <- quantile(alphas[, alphaplotid],0.5)
#   bSDI <- quantile(betas[,"betaSDI"],0.5)
#   bSDI_ppt <- quantile(betas[,"betaSDI_wintP.wateryr"],0.5)
#   bX <-  quantile(betas[,"betaX"],0.5)
#   bX2 <- quantile(betas[,"betaX2"],0.5)
#   bX_SDI <- quantile(betas[,"betaX_SDI"],0.5)
#   bX_ppt <- quantile(betas[,"betaX_wintP.wateryr"],0.5)
#   bppt <- quantile(betas[,"betawintP.wateryr"],0.5)
#   btmax <- quantile(betas[,"betatmax.fallspr"] ,0.5)
#   bX_tmax <- quantile(betas[,"betaX_tmax.fallspr"],0.5)
#   bSDI_tmax <- quantile(betas[,"betaSDI_tmax.fallspr"],0.5)
#   btmax_ppt <- quantile(betas[,"betatmax.fallspr_wintP.wateryr"],0.5)
#   b0 <- quantile(B0, 0.5)
#   
#   betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
#                           bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
#   
#   SDI <- rep(cov.data[m, ]$SDI, 45)
#   ppt <- time_data$wintP.wateryr[m,]
#   tmax <- time_data$tmax.fallspr[m,]
#   #x <- x.mat[,"x[1,45]"]
#   covariates <- data.frame(SDI, ppt, tmax)
#   
#   
#   
#   
#   
#   test <- iterate_statespace(x = x.mat[,"x[1,45]"], m = 1, betas.all = betas.all, alpha, SDinc = 0, covariates[1,])
#   #---------------------------------------------------------------------------
#   ##  Initial condition uncertainty: make forecasts from all MCMC iterations of
#   ##    the final year, but use mean parameter values and no process error.
#   time_steps <- 45
#   nMCMC <- length(x.mat[,"x[1,45]"])
#   forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
#   
#   
#   # just for 1 tree
#   for(t in 1:time_steps){
#     inc.pred <- iterate_statespace(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDinc = 0, covariates = covariates[t,])
#     forecast[,t] <- inc.pred
#   }
#   varianceIC <- apply(forecast,2,var) # get variance from IC
#   forecast.ic <- apply(forecast, 2, quantile)
#   
#   #---------------------------------------------------------------------------
#   ##  Uncertainty from Initial conditions AND parameters uncertainty
#   #---------------------------------------------------------------------------
#   # use all of the parameter MCMCS:
#   alpha <- alphas[57001:60300, alphaplotid]
#   bSDI <- betas[57001:60300,"betaSDI"]
#   bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
#   bX <-  betas[57001:60300,"betaX"]
#   bX2 <- betas[57001:60300,"betaX2"]
#   bX_SDI <- betas[57001:60300,"betaX_SDI"]
#   bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
#   bppt <- betas[57001:60300,"betawintP.wateryr"]
#   btmax <- betas[57001:60300,"betatmax.fallspr"]
#   bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
#   bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
#   btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
#   b0 <- B0[57001:60300]
#   
#   betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
#                           bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
#   
#   SDI <- rep(cov.data[m, ]$SDI, 45)
#   ppt <- time_data$wintP.wateryr[m,]
#   tmax <- time_data$tmax.fallspr[m,]
#   x <- x.mat[,"x[1,45]"]
#   covariates <- data.frame(SDI, ppt, tmax)
#   
#   
#   
#   
#   time_steps <- 45
#   nMCMC <- length(x.mat[,"x[1,45]"])
#   forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
#   
#   for(t in 1:time_steps){
#     inc.pred <- iterate_statespace(x = x.mat[,paste0("x[", m,",", t,"]")], m = 1, betas.all = betas.all, alpha = alpha, SDinc = 0, covariates = covariates[t,])
#     forecast[,t] <- inc.pred
#   }
#   varianceIC_Parameters <- apply(forecast,2,var)
#   forecast.ic.param <- apply(forecast, 2, quantile)
#   
#   #---------------------------------------------------------------------------
#   ##  Uncertainty from IC AND parameters uncertainty AND process error
#   #---------------------------------------------------------------------------
#   # use all of the parameter MCMCS:
#   alpha <- alphas[57001:60300, alphaplotid]
#   bSDI <- betas[57001:60300,"betaSDI"]
#   bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
#   bX <-  betas[57001:60300,"betaX"]
#   bX2 <- betas[57001:60300,"betaX2"]
#   bX_SDI <- betas[57001:60300,"betaX_SDI"]
#   bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
#   bppt <- betas[57001:60300,"betawintP.wateryr"]
#   btmax <- betas[57001:60300,"betatmax.fallspr"]
#   bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
#   bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
#   btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
#   b0 <- B0[57001:60300]
#   
#   betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
#                           bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
#   
#   SDI <- rep(cov.data[m, ]$SDI, 45)
#   ppt <- time_data$wintP.wateryr[m,]
#   tmax <- time_data$tmax.fallspr[m,]
#   x <- x.mat[,"x[1,45]"]
#   covariates <- data.frame(SDI, ppt, tmax)
#   
#   Sdev <- 1/quantile(out[,"tau_inc"], 0.5)
#   
#   
#   for(t in 1:time_steps){
#     inc.pred <- iterate_statespace(x = x.mat[,paste0("x[",m,",", t,"]")], m = m, betas.all = betas.all, alpha, SDinc = Sdev, covariates[t,])
#     forecast[,t] <- inc.pred
#   }
#   varianceIC_Parameters_process <- apply(forecast,2,var)
#   forecast.ic.param.process <- apply(forecast, 2, quantile)
#   
#   
#   # combine variances:
#   V.pred.sim     <- rbind(varianceIC_Parameters_process,varianceIC_Parameters,varianceIC)
#   V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
#   
#   # combine forecasts:
#   pred.sims     <- data.frame(IPP.0 = forecast.ic.param.process[1,],
#                               IP.0 =forecast.ic.param[1,],
#                               I.0 =forecast.ic[1,],
#                               IPP.100= forecast.ic.param.process[5,],
#                               IP.100=forecast.ic.param[5,],
#                               I.100=forecast.ic[5,],
#                               year = 1:45)
#   my_cols <- c("#1b9e77",
#                "#d95f02",
#                "#7570b3")
#   
#   predY_plot <- ggplot(data=pred.sims, aes(x=year))+
#     geom_ribbon(aes(ymin=IPP.0, ymax=IPP.100), fill=my_cols[3])+
#     geom_ribbon(aes(ymin=IP.0, ymax=IP.100), fill=my_cols[2])+
#     geom_ribbon(aes(ymin=I.0, ymax=I.100), fill=my_cols[1])+
#     ylab("Predicted Tree Growth")+
#     xlab("Year")+
#     #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
#     #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
#     #scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
#     theme_bw()
#   
#   # pred.sim.0rel <- apply(pred.sim.0,2,function(x) {x/max(x)})
#   # 
#   # pred.sim.75     <- rbind(forecast.ic.param.process[5,],
#   #                          forecast.ic.param[5,],
#   #                          forecast.ic[5,])
#   # pred.sim.75rel <- apply(pred.sim.75,2,function(x) {x/max(x)})
#   # 
#   # plot(1:45, pred.sim.0[1,], type = "l")
#   # lines(1:45, pred.sim.75[1,], col= "red")
#   ####
#   ####  PLOT THE FORECASTING UNCERTAINTY PARTITION -------------------------------
#   ####
#   var_rel_preds <- as.data.frame(t(V.pred.sim.rel*100))
#   var_rel_preds$x <- 1:nrow(var_rel_preds)
#   #my_cols <- c("black", "grey55", "grey70")
#   my_cols <- c("#1b9e77",
#                "#d95f02",
#                "#7570b3")
#   variance_plot <- ggplot(data=var_rel_preds, aes(x=x))+
#     geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters_process), fill=my_cols[3])+
#     geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters), fill=my_cols[2])+
#     geom_ribbon(aes(ymin=0, ymax=varianceIC), fill=my_cols[1])+
#     ylab("Percent of uncertainty")+
#     xlab("Year")+
#     #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
#     #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
#     scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
#     theme_bw()
#   
#   variance_plot 
#   
#   # now do this for all of the trees:
#   
#   
#   
#   
#   ##  For presentations
#   
#   my_cols <- c("#1b9e77",
#                "#d95f02",
#                "#7570b3")
#   tmpvar <- var_rel_preds
#   colnames(tmpvar) <- c( "BvarIPD", "CvarIP", "DvarI", "x")
#   var2 <- tmpvar %>%
#     gather(simtype, variance, -x)
#   
#   ggplot(var2, aes(x=x, fill = simtype))+
#     geom_ribbon(aes(ymin=0, ymax=variance), color = "black")+
#     ylab("Percentage of total variance (%)")+
#     xlab("Year")+
#     scale_fill_manual(values = my_cols, name = NULL, 
#                       labels = c("Process error", "Parameter uncertainty", "Initial conditions"))+
#     scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
#                        expand = c(0, 0))+
#     theme_bw()
#   
#   
# }
# plot.prop.variance (m=515)
# 
# 
# plot.increment.variance <- function(m){
#   #-----------------------------------------------------------------------
#   # Partitioning uncertainty
#   #-----------------------------------------------------------------------
#   # Create a function for our process model (linear state space model)
#   
#   iterate_statespace <- function( x = x.mat[,"x[1,45]"], m = m, betas.all, alpha, SDinc, covariates) {
#     
#     j <- 1
#     
#     # alpha <- alphas[j, alphaplotid] 
#     # bSDI <- betas[j,"betaSDI"]
#     # bSDI_ppt <- betas[j,"betaSDI_wintP.wateryr"]
#     # bX <-  betas[j,"betaX"]
#     # bX2 <- betas[j,"betaX2"]
#     # bX_SDI <- betas[j,"betaX_SDI"]
#     # bX_ppt <- betas[j,"betaX_wintP.wateryr"]
#     # bppt <- betas[j,"betawintP.wateryr"]
#     # btmax <- betas[j,"betatmax.fallspr"] 
#     # bX_tmax <- betas[j,"betaX_tmax.fallspr"]
#     # bSDI_tmax <- betas[j,"betaSDI_tmax.fallspr"]
#     # btmax_ppt <- betas[j,"betatmax.fallspr_wintP.wateryr"]
#     # 
#     
#     # pseudocode for now
#     tree.growth <- alpha + betas.all$b0 + 
#       betas.all$bSDI*covariates$SDI + 
#       betas.all$bSDI_ppt*covariates$SDI*covariates$ppt + 
#       betas.all$bX*(x-30) + 
#       betas.all$bX2*(x-30)*(x-30) + 
#       betas.all$bX_SDI*(x-30)*covariates$SDI + 
#       betas.all$bX_ppt*covariates$ppt*(x-30) + 
#       betas.all$bppt*covariates$ppt + 
#       betas.all$btmax*covariates$tmax + 
#       betas.all$bX_tmax*(x-30)*covariates$tmax + 
#       betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
#       betas.all$btmax_ppt*covariates$tmax*covariates$ppt 
#     #tree.growth
#     
#     
#     # Stochastic process model
#     ypred <- rnorm(length(tree.growth), tree.growth, SDinc) 
#     
#     ypred
#   }
#   alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
#   
#   alpha <- quantile(alphas[, alphaplotid],0.5)
#   bSDI <- quantile(betas[,"betaSDI"],0.5)
#   bSDI_ppt <- quantile(betas[,"betaSDI_wintP.wateryr"],0.5)
#   bX <-  quantile(betas[,"betaX"],0.5)
#   bX2 <- quantile(betas[,"betaX2"],0.5)
#   bX_SDI <- quantile(betas[,"betaX_SDI"],0.5)
#   bX_ppt <- quantile(betas[,"betaX_wintP.wateryr"],0.5)
#   bppt <- quantile(betas[,"betawintP.wateryr"],0.5)
#   btmax <- quantile(betas[,"betatmax.fallspr"] ,0.5)
#   bX_tmax <- quantile(betas[,"betaX_tmax.fallspr"],0.5)
#   bSDI_tmax <- quantile(betas[,"betaSDI_tmax.fallspr"],0.5)
#   btmax_ppt <- quantile(betas[,"betatmax.fallspr_wintP.wateryr"],0.5)
#   b0 <- quantile(B0, 0.5)
#   
#   betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
#                           bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
#   
#   SDI <- rep(cov.data[m, ]$SDI, 45)
#   ppt <- time_data$wintP.wateryr[m,]
#   tmax <- time_data$tmax.fallspr[m,]
#   #x <- x.mat[,"x[1,45]"]
#   covariates <- data.frame(SDI, ppt, tmax)
# 
#   
#   
#   
#   
#  # test <- iterate_statespace(x = x.mat[,"x[1,45]"], m = 1, betas.all = betas.all, alpha, SDinc = 0, covariates[1,])
#   #---------------------------------------------------------------------------
#   ##  Initial condition uncertainty: make forecasts from all MCMC iterations of
#   ##    the final year, but use mean parameter values and no process error.
#   time_steps <- 45
#   nMCMC <- length(x.mat[,"x[1,45]"])
#   forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
#   
#   
#   # just for 1 tree
#   for(t in 1:time_steps){
#     inc.pred <- iterate_statespace(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDinc = 0, covariates = covariates[t,])
#     forecast[,t] <- inc.pred
#   }
#   varianceIC <- apply(forecast,2,var) # get variance from IC
#   forecast.ic <- apply(forecast, 2, quantile)
#   
#   #---------------------------------------------------------------------------
#   ##  Uncertainty from Initial conditions AND parameters uncertainty
#   #---------------------------------------------------------------------------
#   # use all of the parameter MCMCS:
#   alpha <- alphas[57001:60300, alphaplotid]
#   bSDI <- betas[57001:60300,"betaSDI"]
#   bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
#   bX <-  betas[57001:60300,"betaX"]
#   bX2 <- betas[57001:60300,"betaX2"]
#   bX_SDI <- betas[57001:60300,"betaX_SDI"]
#   bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
#   bppt <- betas[57001:60300,"betawintP.wateryr"]
#   btmax <- betas[57001:60300,"betatmax.fallspr"]
#   bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
#   bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
#   btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
#   b0 <- B0[57001:60300]
#   
#   betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
#                           bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
#   
#   SDI <- rep(cov.data[m, ]$SDI, 45)
#   ppt <- time_data$wintP.wateryr[m,]
#   tmax <- time_data$tmax.fallspr[m,]
#   x <- x.mat[,"x[1,45]"]
#   covariates <- data.frame(SDI, ppt, tmax)
#   
#   
#   
#   
#   time_steps <- 45
#   nMCMC <- length(x.mat[,"x[1,45]"])
#   forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
#   
#   for(t in 1:time_steps){
#     inc.pred <- iterate_statespace(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDinc = 0, covariates = covariates[t,])
#     forecast[,t] <- inc.pred
#   }
#   varianceIC_Parameters <- apply(forecast,2,var)
#   forecast.ic.param <- apply(forecast, 2, quantile)
#   
#   #---------------------------------------------------------------------------
#   ##  Uncertainty from IC AND parameters uncertainty AND process error
#   #---------------------------------------------------------------------------
#   # use all of the parameter MCMCS:
#   alpha <- alphas[57001:60300, alphaplotid]
#   bSDI <- betas[57001:60300,"betaSDI"]
#   bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
#   bX <-  betas[57001:60300,"betaX"]
#   bX2 <- betas[57001:60300,"betaX2"]
#   bX_SDI <- betas[57001:60300,"betaX_SDI"]
#   bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
#   bppt <- betas[57001:60300,"betawintP.wateryr"]
#   btmax <- betas[57001:60300,"betatmax.fallspr"]
#   bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
#   bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
#   btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
#   b0 <- B0[57001:60300]
#   
#   betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
#                           bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
#   
#   SDI <- rep(cov.data[m, ]$SDI, 45)
#   ppt <- time_data$wintP.wateryr[m,]
#   tmax <- time_data$tmax.fallspr[m,]
#   x <- x.mat[,"x[1,45]"]
#   covariates <- data.frame(SDI, ppt, tmax)
#   
#   Sdev <- 1/quantile(out[,"tau_inc"], 0.5)
#   
#   
#   for(t in 1:time_steps){
#     inc.pred <- iterate_statespace(x = x.mat[,paste0("x[",m,",", t,"]")], m = m, betas.all = betas.all, alpha, SDinc = Sdev, covariates[t,])
#     forecast[,t] <- inc.pred
#   }
#   varianceIC_Parameters_process <- apply(forecast,2,var)
#   forecast.ic.param.process <- apply(forecast, 2, quantile)
#   
#   
#   # combine variances:
#   V.pred.sim     <- rbind(varianceIC_Parameters_process,varianceIC_Parameters,varianceIC)
#   V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
#   
#   # combine forecasts:
#   pred.sims     <- data.frame(IPP.0 = forecast.ic.param.process[1,],
#                               IP.0 =forecast.ic.param[1,],
#                               I.0 =forecast.ic[1,],
#                               IPP.100= forecast.ic.param.process[5,],
#                               IP.100=forecast.ic.param[5,],
#                               I.100=forecast.ic[5,],
#                               year = 1:45)
#   pred.sims.m <- reshape2::melt(pred.sims, id.vars = "year")
#   pred.sims.m$simtype <- ifelse(pred.sims.m$variable %in% c("IPP.0", "IPP.100"), "IPP", 
#                                 ifelse(pred.sims.m$variable %in% c("IP.0", "IP.100"), "IP", "I"))
#                                       
#   pred.sims.m$cat <- ifelse(pred.sims.m$variable %in% c("IPP.0", "IP.0", "I.0"), "lo", "hi")
#   pred.sims.by <- pred.sims.m %>% select(-variable) %>% group_by(year, simtype) %>% spread(key = cat, value = value)
#   
#   my_cols <- c("#1b9e77",
#                "#d95f02",
#                "#7570b3")
#   pred.sims.by$simtype <- factor(pred.sims.by$simtype, levels = c(  "IPP","IP", "I") )
#   
#   predY_plot <- ggplot(data=pred.sims.by, aes(x=year))+
#     geom_ribbon(data=pred.sims.by, aes(ymin=lo, ymax=hi, fill=simtype))+
#        scale_fill_manual(values = my_cols, name = NULL, 
#                          labels = c("Process error", "Parameter uncertainty", "Initial conditions"))+
#     ylab("Predicted Tree Growth")+
#     xlab("Year")+
#     #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
#     #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
#     #scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
#     theme_bw()
#   predY_plot
#   
#   # pred.sim.0rel <- apply(pred.sim.0,2,function(x) {x/max(x)})
#   # 
#   # pred.sim.75     <- rbind(forecast.ic.param.process[5,],
#   #                          forecast.ic.param[5,],
#   #                          forecast.ic[5,])
#   # pred.sim.75rel <- apply(pred.sim.75,2,function(x) {x/max(x)})
#   # 
#   # plot(1:45, pred.sim.0[1,], type = "l")
#   # lines(1:45, pred.sim.75[1,], col= "red")
#   ####
#   # ####  PLOT THE FORECASTING UNCERTAINTY PARTITION -------------------------------
#   # ####
#   # var_rel_preds <- as.data.frame(t(V.pred.sim.rel*100))
#   # var_rel_preds$x <- 1:nrow(var_rel_preds)
#   # #my_cols <- c("black", "grey55", "grey70")
#   # my_cols <- c("#1b9e77",
#   #              "#d95f02",
#   #              "#7570b3")
#   # variance_plot <- ggplot(data=var_rel_preds, aes(x=x))+
#   #   geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters_process), fill=my_cols[3])+
#   #   geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters), fill=my_cols[2])+
#   #   geom_ribbon(aes(ymin=0, ymax=varianceIC), fill=my_cols[1])+
#   #   ylab("Percent of uncertainty")+
#   #   xlab("Year")+
#   #   #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
#   #   #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
#   #   scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
#   #   theme_bw()
#   # 
#   # variance_plot 
#   
#   # now do this for all of the trees:
#   
#   
#   
#   
#   # ##  For presentations
#   # 
#   # my_cols <- c("#1b9e77",
#   #              "#d95f02",
#   #              "#7570b3")
#   # tmpvar <- var_rel_preds
#   # colnames(tmpvar) <- c( "BvarIPD", "CvarIP", "DvarI", "x")
#   # var2 <- tmpvar %>%
#   #   gather(simtype, variance, -x)
#   
#   # ggplot(var2, aes(x=x, fill = simtype))+
#   #   geom_ribbon(aes(ymin=0, ymax=variance), color = "black")+
#   #   ylab("Percentage of total variance (%)")+
#   #   xlab("Year")+
#   #   scale_fill_manual(values = my_cols, name = NULL, 
#   #                     labels = c("Process error", "Parameter uncertainty", "Initial conditions"))+
#   #   scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
#   #                      expand = c(0, 0))+
#   #   theme_bw()
#   # 
#   
# }
# plot.increment.variance (m=300)
# 
# for(i in 1:10){
# cowplot::plot_grid(plot.increment.variance (m=100) + theme(legend.position = "none"), plot.prop.variance(m = 100)+ theme(legend.position = "none"), ncol = 1)
# }
# 
# 
# #------------------------------------------------
# # same but for DBH:
# #------------------------------------------------
# 
# plot.prop.variance.dbh <- function(m){
#   #-----------------------------------------------------------------------
#   # Partitioning uncertainty
#   #-----------------------------------------------------------------------
#   # Create a function for our process model (linear state space model)
#   
#   iterate_statespace.dbh <- function( x = x.mat[,"x[1,45]"], m = m, betas.all, alpha, SDdbh, covariates) {
#     
#     j <- 1
#     
#     # alpha <- alphas[j, alphaplotid] 
#     # bSDI <- betas[j,"betaSDI"]
#     # bSDI_ppt <- betas[j,"betaSDI_wintP.wateryr"]
#     # bX <-  betas[j,"betaX"]
#     # bX2 <- betas[j,"betaX2"]
#     # bX_SDI <- betas[j,"betaX_SDI"]
#     # bX_ppt <- betas[j,"betaX_wintP.wateryr"]
#     # bppt <- betas[j,"betawintP.wateryr"]
#     # btmax <- betas[j,"betatmax.fallspr"] 
#     # bX_tmax <- betas[j,"betaX_tmax.fallspr"]
#     # bSDI_tmax <- betas[j,"betaSDI_tmax.fallspr"]
#     # btmax_ppt <- betas[j,"betatmax.fallspr_wintP.wateryr"]
#     # 
#     
#     # pseudocode for now
#     dbh.new <-x + alpha + betas.all$b0 + 
#       betas.all$bSDI*covariates$SDI + 
#       betas.all$bSDI_ppt*covariates$SDI*covariates$ppt + 
#       betas.all$bX*(x-30) + 
#       betas.all$bX2*(x-30)*(x-30) + 
#       betas.all$bX_SDI*(x-30)*covariates$SDI + 
#       betas.all$bX_ppt*covariates$ppt*(x-30) + 
#       betas.all$bppt*covariates$ppt + 
#       betas.all$btmax*covariates$tmax + 
#       betas.all$bX_tmax*(x-30)*covariates$tmax + 
#       betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
#       betas.all$btmax_ppt*covariates$tmax*covariates$ppt 
#     #tree.growth
#     
#     
#     # Stochastic process model
#     xpred <- rnorm(length(dbh.new), dbh.new, SDdbh) 
#     
#     xpred
#   }
#   alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
#   
#   alpha <- quantile(alphas[, alphaplotid],0.5)
#   bSDI <- quantile(betas[,"betaSDI"],0.5)
#   bSDI_ppt <- quantile(betas[,"betaSDI_wintP.wateryr"],0.5)
#   bX <-  quantile(betas[,"betaX"],0.5)
#   bX2 <- quantile(betas[,"betaX2"],0.5)
#   bX_SDI <- quantile(betas[,"betaX_SDI"],0.5)
#   bX_ppt <- quantile(betas[,"betaX_wintP.wateryr"],0.5)
#   bppt <- quantile(betas[,"betawintP.wateryr"],0.5)
#   btmax <- quantile(betas[,"betatmax.fallspr"] ,0.5)
#   bX_tmax <- quantile(betas[,"betaX_tmax.fallspr"],0.5)
#   bSDI_tmax <- quantile(betas[,"betaSDI_tmax.fallspr"],0.5)
#   btmax_ppt <- quantile(betas[,"betatmax.fallspr_wintP.wateryr"],0.5)
#   b0 <- quantile(B0, 0.5)
#   
#   betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
#                           bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
#   
#   SDI <- rep(cov.data[m, ]$SDI, 45)
#   ppt <- time_data$wintP.wateryr[m,]
#   tmax <- time_data$tmax.fallspr[m,]
#   #x <- x.mat[,"x[1,45]"]
#   covariates <- data.frame(SDI, ppt, tmax)
#   
#   
#   
#   
#   
#   test <- iterate_statespace.dbh(x = x.mat[,"x[1,45]"], m = m, betas.all = betas.all, alpha, SDdbh = 0, covariates[1,])
#   #---------------------------------------------------------------------------
#   ##  Initial condition uncertainty: make forecasts from all MCMC iterations of
#   ##    the final year, but use mean parameter values and no process error.
#   time_steps <- 45
#   nMCMC <- length(x.mat[,"x[1,45]"])
#   forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
#   
#   
#   # just for 1 tree
#   for(t in 1:time_steps){
#     dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
#     forecast[,t] <- dbh.pred
#   }
#   varianceIC <- apply(forecast,2,var) # get variance from IC
#   forecast.ic <- apply(forecast, 2, quantile)
#   
#   #---------------------------------------------------------------------------
#   ##  Uncertainty from Initial conditions AND parameters uncertainty
#   #---------------------------------------------------------------------------
#   # use all of the parameter MCMCS:
#   alpha <- alphas[57001:60300, alphaplotid]
#   bSDI <- betas[57001:60300,"betaSDI"]
#   bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
#   bX <-  betas[57001:60300,"betaX"]
#   bX2 <- betas[57001:60300,"betaX2"]
#   bX_SDI <- betas[57001:60300,"betaX_SDI"]
#   bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
#   bppt <- betas[57001:60300,"betawintP.wateryr"]
#   btmax <- betas[57001:60300,"betatmax.fallspr"]
#   bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
#   bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
#   btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
#   b0 <- B0[57001:60300]
#   
#   betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
#                           bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
#   
#   SDI <- rep(cov.data[m, ]$SDI, 45)
#   ppt <- time_data$wintP.wateryr[m,]
#   tmax <- time_data$tmax.fallspr[m,]
#   x <- x.mat[,"x[1,45]"]
#   covariates <- data.frame(SDI, ppt, tmax)
#   
#   
#   
#   
#   time_steps <- 45
#   nMCMC <- length(x.mat[,"x[1,45]"])
#   forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
#   
#   for(t in 1:time_steps){
#     inc.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
#     forecast[,t] <- inc.pred
#   }
#   varianceIC_Parameters <- apply(forecast,2,var)
#   forecast.ic.param <- apply(forecast, 2, quantile)
#   
#   #---------------------------------------------------------------------------
#   ##  Uncertainty from IC AND parameters uncertainty AND process error
#   #---------------------------------------------------------------------------
#   # use all of the parameter MCMCS:
#   alpha <- alphas[57001:60300, alphaplotid]
#   bSDI <- betas[57001:60300,"betaSDI"]
#   bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
#   bX <-  betas[57001:60300,"betaX"]
#   bX2 <- betas[57001:60300,"betaX2"]
#   bX_SDI <- betas[57001:60300,"betaX_SDI"]
#   bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
#   bppt <- betas[57001:60300,"betawintP.wateryr"]
#   btmax <- betas[57001:60300,"betatmax.fallspr"]
#   bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
#   bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
#   btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
#   b0 <- B0[57001:60300]
#   
#   betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
#                           bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
#   
#   SDI <- rep(cov.data[m, ]$SDI, 45)
#   ppt <- time_data$wintP.wateryr[m,]
#   tmax <- time_data$tmax.fallspr[m,]
#   x <- x.mat[,"x[1,45]"]
#   covariates <- data.frame(SDI, ppt, tmax)
#   
#   Sdev <- 1/quantile(out[,"tau_dbh"], 0.5)
#   
#   
#   for(t in 1:time_steps){
#     inc.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[",m,",", t,"]")], m = m, betas.all = betas.all, alpha, SDdbh = Sdev, covariates[t,])
#     forecast[,t] <- inc.pred
#   }
#   varianceIC_Parameters_process <- apply(forecast,2,var)
#   forecast.ic.param.process <- apply(forecast, 2, quantile)
#   
#   
#   # combine variances:
#   V.pred.sim     <- rbind(varianceIC_Parameters_process,varianceIC_Parameters,varianceIC)
#   V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
#   
#   # combine forecasts:
#   pred.sims     <- data.frame(IPP.0 = forecast.ic.param.process[1,],
#                               IP.0 =forecast.ic.param[1,],
#                               I.0 =forecast.ic[1,],
#                               IPP.100= forecast.ic.param.process[5,],
#                               IP.100=forecast.ic.param[5,],
#                               I.100=forecast.ic[5,],
#                               year = 1:45)
#   my_cols <- c("#1b9e77",
#                "#d95f02",
#                "#7570b3")
#   
#   predY_plot <- ggplot(data=pred.sims, aes(x=year))+
#     geom_ribbon(aes(ymin=IPP.0, ymax=IPP.100), fill=my_cols[3])+
#     geom_ribbon(aes(ymin=I.0, ymax=I.100), fill=my_cols[1])+
#     geom_ribbon(aes(ymin=IP.0, ymax=IP.100), fill=my_cols[2])+
#     
#     ylab("Predicted DBH")+
#     xlab("Year")+
#     #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
#     #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
#     #scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
#     theme_bw()
#   
#   # pred.sim.0rel <- apply(pred.sim.0,2,function(x) {x/max(x)})
#   # 
#   # pred.sim.75     <- rbind(forecast.ic.param.process[5,],
#   #                          forecast.ic.param[5,],
#   #                          forecast.ic[5,])
#   # pred.sim.75rel <- apply(pred.sim.75,2,function(x) {x/max(x)})
#   # 
#   # plot(1:45, pred.sim.0[1,], type = "l")
#   # lines(1:45, pred.sim.75[1,], col= "red")
#   ####
#   ####  PLOT THE FORECASTING UNCERTAINTY PARTITION -------------------------------
#   ####
#   var_rel_preds <- as.data.frame(t(V.pred.sim.rel*100))
#   var_rel_preds$x <- 1:nrow(var_rel_preds)
#   #my_cols <- c("black", "grey55", "grey70")
#   my_cols <- c("#1b9e77",
#                "#d95f02",
#                "#7570b3")
#   variance_plot <- ggplot(data=var_rel_preds, aes(x=x))+
#     geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters_process), fill=my_cols[3])+
#     geom_ribbon(aes(ymin=0, ymax=varianceIC), fill=my_cols[1])+
#     geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters), fill=my_cols[2])+
#     
#     ylab("Percent of uncertainty")+
#     xlab("Year")+
#     #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
#     #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
#     scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
#     theme_bw()
#   
#   variance_plot 
#   
#   # now do this for all of the trees:
#   
#   
#   
#   
#   ##  For presentations
#   
#   my_cols <- c("#1b9e77",
#                "#d95f02",
#                "#7570b3")
#   tmpvar <- var_rel_preds
#   colnames(tmpvar) <- c( "BvarIPD", "CvarIP", "DvarI", "x")
#   var2 <- tmpvar %>%
#     gather(simtype, variance, -x)
#   
#   var2$simtype <- factor(var2$simtype, levels = c("BvarIPD", "DvarI", "CvarIP"))
#   ggplot(var2, aes(x=x, fill = simtype))+
#     geom_ribbon(aes(ymin=0, ymax=variance), color = "black")+
#     ylab("Percentage of total variance (%)")+
#     xlab("Year")+
#     scale_fill_manual(values = my_cols, name = NULL, 
#                       labels = c("Process error", "Parameter uncertainty", "Initial conditions"))+
#     scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
#                        expand = c(0, 0))+
#     theme_bw()
#   
#   
# }
# plot.prop.variance.dbh (m=400)
# 
# plot.dbh.variance <- function(m){
#   #-----------------------------------------------------------------------
#   # Partitioning uncertainty
#   #-----------------------------------------------------------------------
#   # Create a function for our process model (linear state space model)
#   
#   iterate_statespace <- function( x = x.mat[,"x[1,45]"], m = m, betas.all, alpha, SDdbh, covariates) {
#     
#     j <- 1
#     
#     
#     
#     # pseudocode for now
#     dbh.new <- x + alpha + betas.all$b0 + 
#       betas.all$bSDI*covariates$SDI + 
#       betas.all$bSDI_ppt*covariates$SDI*covariates$ppt + 
#       betas.all$bX*(x-30) + 
#       betas.all$bX2*(x-30)*(x-30) + 
#       betas.all$bX_SDI*(x-30)*covariates$SDI + 
#       betas.all$bX_ppt*covariates$ppt*(x-30) + 
#       betas.all$bppt*covariates$ppt + 
#       betas.all$btmax*covariates$tmax + 
#       betas.all$bX_tmax*(x-30)*covariates$tmax + 
#       betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
#       betas.all$btmax_ppt*covariates$tmax*covariates$ppt 
#     #tree.growth
#     
#     
#     # Stochastic process model
#     xpred <- rnorm(length(dbh.new), dbh.new, SDdbh) 
#     
#     xpred
#   }
#   
#   
#   alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
#   
#   alpha <- quantile(alphas[, alphaplotid],0.5)
#   bSDI <- quantile(betas[,"betaSDI"],0.5)
#   bSDI_ppt <- quantile(betas[,"betaSDI_wintP.wateryr"],0.5)
#   bX <-  quantile(betas[,"betaX"],0.5)
#   bX2 <- quantile(betas[,"betaX2"],0.5)
#   bX_SDI <- quantile(betas[,"betaX_SDI"],0.5)
#   bX_ppt <- quantile(betas[,"betaX_wintP.wateryr"],0.5)
#   bppt <- quantile(betas[,"betawintP.wateryr"],0.5)
#   btmax <- quantile(betas[,"betatmax.fallspr"] ,0.5)
#   bX_tmax <- quantile(betas[,"betaX_tmax.fallspr"],0.5)
#   bSDI_tmax <- quantile(betas[,"betaSDI_tmax.fallspr"],0.5)
#   btmax_ppt <- quantile(betas[,"betatmax.fallspr_wintP.wateryr"],0.5)
#   b0 <- quantile(B0, 0.5)
#   
#   betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
#                           bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
#   
#   SDI <- rep(cov.data[m, ]$SDI, 45)
#   ppt <- time_data$wintP.wateryr[m,]
#   tmax <- time_data$tmax.fallspr[m,]
#   #x <- x.mat[,"x[1,45]"]
#   covariates <- data.frame(SDI, ppt, tmax)
#   
#   
#   
#   
#   
#   # test <- iterate_statespace(x = x.mat[,"x[1,45]"], m = 1, betas.all = betas.all, alpha, SDinc = 0, covariates[1,])
#   #---------------------------------------------------------------------------
#   ##  Initial condition uncertainty: make forecasts from all MCMC iterations of
#   ##    the final year, but use mean parameter values and no process error.
#   time_steps <- 45
#   nMCMC <- length(x.mat[,"x[1,45]"])
#   forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
#   
#   
#   # just for 1 tree
#   for(t in 1:time_steps){
#     inc.pred <- iterate_statespace(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
#     forecast[,t] <- inc.pred
#   }
#   varianceIC <- apply(forecast,2,var) # get variance from IC
#   forecast.ic <- apply(forecast, 2, quantile)
#   
#   #---------------------------------------------------------------------------
#   ##  Uncertainty from Initial conditions AND parameters uncertainty
#   #---------------------------------------------------------------------------
#   # use all of the parameter MCMCS:
#   alpha <- alphas[57001:60300, alphaplotid]
#   bSDI <- betas[57001:60300,"betaSDI"]
#   bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
#   bX <-  betas[57001:60300,"betaX"]
#   bX2 <- betas[57001:60300,"betaX2"]
#   bX_SDI <- betas[57001:60300,"betaX_SDI"]
#   bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
#   bppt <- betas[57001:60300,"betawintP.wateryr"]
#   btmax <- betas[57001:60300,"betatmax.fallspr"]
#   bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
#   bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
#   btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
#   b0 <- B0[57001:60300]
#   
#   betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
#                           bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
#   
#   SDI <- rep(cov.data[m, ]$SDI, 45)
#   ppt <- time_data$wintP.wateryr[m,]
#   tmax <- time_data$tmax.fallspr[m,]
#   x <- x.mat[,"x[1,45]"]
#   covariates <- data.frame(SDI, ppt, tmax)
#   
#   
#   
#   
#   time_steps <- 45
#   nMCMC <- length(x.mat[,"x[1,45]"])
#   forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
#   
#   for(t in 1:time_steps){
#     inc.pred <- iterate_statespace(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
#     forecast[,t] <- inc.pred
#   }
#   varianceIC_Parameters <- apply(forecast,2,var)
#   forecast.ic.param <- apply(forecast, 2, quantile)
#   
#   #---------------------------------------------------------------------------
#   ##  Uncertainty from IC AND parameters uncertainty AND process error
#   #---------------------------------------------------------------------------
#   # use all of the parameter MCMCS:
#   alpha <- alphas[57001:60300, alphaplotid]
#   bSDI <- betas[57001:60300,"betaSDI"]
#   bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
#   bX <-  betas[57001:60300,"betaX"]
#   bX2 <- betas[57001:60300,"betaX2"]
#   bX_SDI <- betas[57001:60300,"betaX_SDI"]
#   bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
#   bppt <- betas[57001:60300,"betawintP.wateryr"]
#   btmax <- betas[57001:60300,"betatmax.fallspr"]
#   bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
#   bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
#   btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
#   b0 <- B0[57001:60300]
#   
#   betas.all.mcmc <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
#                           bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
#   
#   SDI <- rep(cov.data[m, ]$SDI, 45)
#   ppt <- time_data$wintP.wateryr[m,]
#   tmax <- time_data$tmax.fallspr[m,]
#   x <- x.mat[,"x[1,45]"]
#   covariates <- data.frame(SDI, ppt, tmax)
#   
#   Sdev <- 1/quantile(out[,"tau_dbh"], 0.5)
#   
#   
#   for(t in 1:time_steps){
#     inc.pred <- iterate_statespace(x = x.mat[,paste0("x[",m,",", t,"]")], m = m, betas.all = betas.all.mcmc, alpha, SDdbh = Sdev, covariates[t,])
#     forecast[,t] <- inc.pred
#   }
#   varianceIC_Parameters_process <- apply(forecast,2,var)
#   forecast.ic.param.process <- apply(forecast, 2, quantile)
#   
#   
#   # combine variances:
#   V.pred.sim     <- rbind(varianceIC_Parameters_process,varianceIC_Parameters,varianceIC)
#   V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
#   
#   # combine forecasts:
#   pred.sims     <- data.frame(IPP.0 = forecast.ic.param.process[1,],
#                               IP.0 =forecast.ic.param[1,],
#                               I.0 =forecast.ic[1,],
#                               IPP.100= forecast.ic.param.process[5,],
#                               IP.100=forecast.ic.param[5,],
#                               I.100=forecast.ic[5,],
#                               year = 1:45)
#   pred.sims.m <- reshape2::melt(pred.sims, id.vars = "year")
#   pred.sims.m$simtype <- ifelse(pred.sims.m$variable %in% c("IPP.0", "IPP.100"), "IPP", 
#                                 ifelse(pred.sims.m$variable %in% c("IP.0", "IP.100"), "IP", "I"))
#   
#   pred.sims.m$cat <- ifelse(pred.sims.m$variable %in% c("IPP.0", "IP.0", "I.0"), "lo", "hi")
#   pred.sims.by <- pred.sims.m %>% select(-variable) %>% group_by(year, simtype) %>% spread(key = cat, value = value)
#   
#   my_cols <- c("#1b9e77",
#                "#d95f02",
#                "#7570b3")
#   pred.sims.by$simtype <- factor(pred.sims.by$simtype, levels = c(  "IPP","IP", "I") )
#   
#   predY_plot <- ggplot(data=pred.sims.by, aes(x=year))+
#     geom_ribbon(data=pred.sims.by, aes(ymin=lo, ymax=hi, fill=simtype))+
#     scale_fill_manual(values = my_cols, name = NULL, 
#                       labels = c("Process error", "Parameter uncertainty", "Initial conditions"))+
#     ylab("Predicted Tree Growth")+
#     xlab("Year")+
#     #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
#     #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
#     #scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
#     theme_bw()
#   predY_plot
#   
#   # pred.sim.0rel <- apply(pred.sim.0,2,function(x) {x/max(x)})
#   # 
#   # pred.sim.75     <- rbind(forecast.ic.param.process[5,],
#   #                          forecast.ic.param[5,],
#   #                          forecast.ic[5,])
#   # pred.sim.75rel <- apply(pred.sim.75,2,function(x) {x/max(x)})
#   # 
#   # plot(1:45, pred.sim.0[1,], type = "l")
#   # lines(1:45, pred.sim.75[1,], col= "red")
#   ####
#   # ####  PLOT THE FORECASTING UNCERTAINTY PARTITION -------------------------------
#   # ####
#   # var_rel_preds <- as.data.frame(t(V.pred.sim.rel*100))
#   # var_rel_preds$x <- 1:nrow(var_rel_preds)
#   # #my_cols <- c("black", "grey55", "grey70")
#   # my_cols <- c("#1b9e77",
#   #              "#d95f02",
#   #              "#7570b3")
#   # variance_plot <- ggplot(data=var_rel_preds, aes(x=x))+
#   #   geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters_process), fill=my_cols[3])+
#   #   geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters), fill=my_cols[2])+
#   #   geom_ribbon(aes(ymin=0, ymax=varianceIC), fill=my_cols[1])+
#   #   ylab("Percent of uncertainty")+
#   #   xlab("Year")+
#   #   #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
#   #   #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
#   #   scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
#   #   theme_bw()
#   # 
#   # variance_plot 
#   
#   # now do this for all of the trees:
#   
#   
#   
#   
#   # ##  For presentations
#   # 
#   # my_cols <- c("#1b9e77",
#   #              "#d95f02",
#   #              "#7570b3")
#   # tmpvar <- var_rel_preds
#   # colnames(tmpvar) <- c( "BvarIPD", "CvarIP", "DvarI", "x")
#   # var2 <- tmpvar %>%
#   #   gather(simtype, variance, -x)
#   
#   # ggplot(var2, aes(x=x, fill = simtype))+
#   #   geom_ribbon(aes(ymin=0, ymax=variance), color = "black")+
#   #   ylab("Percentage of total variance (%)")+
#   #   xlab("Year")+
#   #   scale_fill_manual(values = my_cols, name = NULL, 
#   #                     labels = c("Process error", "Parameter uncertainty", "Initial conditions"))+
#   #   scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
#   #                      expand = c(0, 0))+
#   #   theme_bw()
#   # 
#   
# }
# plot.dbh.variance (m=400)
# cowplot::plot_grid(plot.dbh.variance (m=100) + theme(legend.position = "none"), plot.prop.variance.dbh(m = 100)+ theme(legend.position = "none"), ncol = 1)
# 
# 
# 
# #------------------------------------------------
# # set up to forecast through time from just the X initial condiation
# #------------------------------------------------
# # this is important for when we start to forecast with climate drivers
# plot.prop.variance.dbh.forecast <- function(m, prop =TRUE){
#   #-----------------------------------------------------------------------
#   # Partitioning uncertainty
#   #-----------------------------------------------------------------------
#   # Create a function for our process model (linear state space model)
#   
#   iterate_statespace.dbh <- function( x = x.mat[,"x[1,45]"], m = m, betas.all, alpha, SDdbh, covariates) {
#     
#     j <- 1
#     
#     # alpha <- alphas[j, alphaplotid] 
#     # bSDI <- betas[j,"betaSDI"]
#     # bSDI_ppt <- betas[j,"betaSDI_wintP.wateryr"]
#     # bX <-  betas[j,"betaX"]
#     # bX2 <- betas[j,"betaX2"]
#     # bX_SDI <- betas[j,"betaX_SDI"]
#     # bX_ppt <- betas[j,"betaX_wintP.wateryr"]
#     # bppt <- betas[j,"betawintP.wateryr"]
#     # btmax <- betas[j,"betatmax.fallspr"] 
#     # bX_tmax <- betas[j,"betaX_tmax.fallspr"]
#     # bSDI_tmax <- betas[j,"betaSDI_tmax.fallspr"]
#     # btmax_ppt <- betas[j,"betatmax.fallspr_wintP.wateryr"]
#     # 
#     
#     # pseudocode for now
#     dbh.new <-x + alpha + betas.all$b0 + 
#       betas.all$bSDI*covariates$SDI + 
#       betas.all$bSDI_ppt*covariates$SDI*covariates$ppt + 
#       betas.all$bX*(x-30) + 
#       betas.all$bX2*(x-30)*(x-30) + 
#       betas.all$bX_SDI*(x-30)*covariates$SDI + 
#       betas.all$bX_ppt*covariates$ppt*(x-30) + 
#       betas.all$bppt*covariates$ppt + 
#       betas.all$btmax*covariates$tmax + 
#       betas.all$bX_tmax*(x-30)*covariates$tmax + 
#       betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
#       betas.all$btmax_ppt*covariates$tmax*covariates$ppt 
#     #tree.growth
#     
#     
#     # Stochastic process model
#     xpred <- rnorm(length(dbh.new), dbh.new, SDdbh) 
#     
#     xpred
#   }
#   alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
#   
#   alpha <- quantile(alphas[, alphaplotid],0.5)
#   bSDI <- quantile(betas[,"betaSDI"],0.5)
#   bSDI_ppt <- quantile(betas[,"betaSDI_wintP.wateryr"],0.5)
#   bX <-  quantile(betas[,"betaX"],0.5)
#   bX2 <- quantile(betas[,"betaX2"],0.5)
#   bX_SDI <- quantile(betas[,"betaX_SDI"],0.5)
#   bX_ppt <- quantile(betas[,"betaX_wintP.wateryr"],0.5)
#   bppt <- quantile(betas[,"betawintP.wateryr"],0.5)
#   btmax <- quantile(betas[,"betatmax.fallspr"] ,0.5)
#   bX_tmax <- quantile(betas[,"betaX_tmax.fallspr"],0.5)
#   bSDI_tmax <- quantile(betas[,"betaSDI_tmax.fallspr"],0.5)
#   btmax_ppt <- quantile(betas[,"betatmax.fallspr_wintP.wateryr"],0.5)
#   b0 <- quantile(B0, 0.5)
#   
#   betas.point <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
#                           bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
#   
#   SDI <- rep(cov.data[m, ]$SDI, 45)
#   ppt <- time_data$wintP.wateryr[m,]
#   tmax <- time_data$tmax.fallspr[m,]
#   #x <- x.mat[,"x[1,45]"]
#   covariates <- data.frame(SDI, ppt, tmax)
#   
#   
#   
#   
#   
#   test <- iterate_statespace.dbh(x = x.mat[,"x[1,45]"], m = m, betas.all = betas.point, alpha, SDdbh = 0, covariates[1,])
#   #---------------------------------------------------------------------------
#   ##  Initial condition uncertainty: make forecasts from all MCMC iterations of
#   ##    the final year, but use mean parameter values and no process error.
#   time_steps <- 45
#   nMCMC <- length(x.mat[,"x[1,45]"])
#   forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
#   
#   
#   # just for 1 tree
#   for(t in 1:time_steps){
#     if(t == 1){
#     dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.point, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
#     forecast[,t] <- dbh.pred
#     }else{
#       dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.point, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
#       forecast[,t] <- dbh.pred
#       
#     }
#   }
#   varianceIC <- apply(forecast,2,var) # get variance from IC
#   forecast.ic <- apply(forecast, 2, quantile)
#   
#   #---------------------------------------------------------------------------
#   ##  Uncertainty from Initial conditions AND parameters uncertainty
#   #---------------------------------------------------------------------------
#   # use all of the parameter MCMCS:
#   alpha <- alphas[57001:60300, alphaplotid]
#   bSDI <- betas[57001:60300,"betaSDI"]
#   bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
#   bX <-  betas[57001:60300,"betaX"]
#   bX2 <- betas[57001:60300,"betaX2"]
#   bX_SDI <- betas[57001:60300,"betaX_SDI"]
#   bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
#   bppt <- betas[57001:60300,"betawintP.wateryr"]
#   btmax <- betas[57001:60300,"betatmax.fallspr"]
#   bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
#   bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
#   btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
#   b0 <- B0[57001:60300]
#   
#   betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
#                           bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
#   
#   SDI <- rep(cov.data[m, ]$SDI, 45)
#   ppt <- time_data$wintP.wateryr[m,]
#   tmax <- time_data$tmax.fallspr[m,]
#   x <- x.mat[,"x[1,45]"]
#   covariates <- data.frame(SDI, ppt, tmax)
#   
#   
#   
#   
#   time_steps <- 45
#   nMCMC <- length(x.mat[,"x[1,45]"])
#   forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
#   
#   for(t in 1:time_steps){
#     if(t == 1){
#       dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
#       forecast[,t] <- dbh.pred
#     }else{
#       dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
#       forecast[,t] <- dbh.pred
#       
#     }  }
#   varianceIC_Parameters <- apply(forecast,2,var)
#   forecast.ic.param <- apply(forecast, 2, quantile)
#   
#   #---------------------------------------------------------------------------
#   ##  Uncertainty from IC AND parameters uncertainty AND process error
#   #---------------------------------------------------------------------------
#   # use all of the parameter MCMCS:
#   alpha <- alphas[57001:60300, alphaplotid]
#   bSDI <- betas[57001:60300,"betaSDI"]
#   bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
#   bX <-  betas[57001:60300,"betaX"]
#   bX2 <- betas[57001:60300,"betaX2"]
#   bX_SDI <- betas[57001:60300,"betaX_SDI"]
#   bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
#   bppt <- betas[57001:60300,"betawintP.wateryr"]
#   btmax <- betas[57001:60300,"betatmax.fallspr"]
#   bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
#   bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
#   btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
#   b0 <- B0[57001:60300]
#   
#   betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
#                           bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
#   
#   SDI <- rep(cov.data[m, ]$SDI, 45)
#   ppt <- time_data$wintP.wateryr[m,]
#   tmax <- time_data$tmax.fallspr[m,]
#   x <- x.mat[,"x[1,45]"]
#   covariates <- data.frame(SDI, ppt, tmax)
#   
#   Sdev <- 1/quantile(out[,"tau_dbh"], 0.5)
#   
#   
#   for(t in 1:time_steps){
#     if(t == 1){
#       dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = Sdev, covariates = covariates[t,])
#       forecast[,t] <- dbh.pred
#     }else{
#       dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = Sdev, covariates = covariates[t,])
#       forecast[,t] <- dbh.pred
#       
#     }  }
#   varianceIC_Parameters_process <- apply(forecast,2,var)
#   forecast.ic.param.process <- apply(forecast, 2, quantile)
#   
#   
#   # combine variances:
#   V.pred.sim     <- rbind(varianceIC_Parameters_process,varianceIC_Parameters,varianceIC)
#   V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
#   
#   # combine forecasts:
#   pred.sims     <- data.frame(IPP.0 = forecast.ic.param.process[1,],
#                               IP.0 =forecast.ic.param[1,],
#                               I.0 =forecast.ic[1,],
#                               IPP.100= forecast.ic.param.process[5,],
#                               IP.100=forecast.ic.param[5,],
#                               I.100=forecast.ic[5,],
#                               year = 1:45)
#   my_cols <- c("#1b9e77",
#                "#d95f02",
#                "#7570b3")
#   
#   predY_plot <- ggplot(data=pred.sims, aes(x=year))+
#     geom_ribbon(aes(ymin=IPP.0, ymax=IPP.100), fill=my_cols[3])+
#     geom_ribbon(aes(ymin=IP.0, ymax=IP.100), fill=my_cols[2])+
#     geom_ribbon(aes(ymin=I.0, ymax=I.100), fill=my_cols[1])+
#     
#     
#     ylab("Predicted DBH")+
#     xlab("Year")+
#     #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
#     #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
#     #scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
#     theme_bw()
#   
#   # pred.sim.0rel <- apply(pred.sim.0,2,function(x) {x/max(x)})
#   # 
#   # pred.sim.75     <- rbind(forecast.ic.param.process[5,],
#   #                          forecast.ic.param[5,],
#   #                          forecast.ic[5,])
#   # pred.sim.75rel <- apply(pred.sim.75,2,function(x) {x/max(x)})
#   # 
#   # plot(1:45, pred.sim.0[1,], type = "l")
#   # lines(1:45, pred.sim.75[1,], col= "red")
#   ####
#   ####  PLOT THE FORECASTING UNCERTAINTY PARTITION -------------------------------
#   ####
#   var_rel_preds <- as.data.frame(t(V.pred.sim.rel*100))
#   var_rel_preds$x <- 1:nrow(var_rel_preds)
#   #my_cols <- c("black", "grey55", "grey70")
#   my_cols <- c("#1b9e77",
#                "#d95f02",
#                "#7570b3")
#   variance_plot <- ggplot(data=var_rel_preds, aes(x=x))+
#     geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters_process), fill=my_cols[3])+
#     geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters),fill=my_cols[2])+
#     geom_ribbon(aes(ymin=0, ymax=varianceIC), fill=my_cols[1])+
#      
#     
#     ylab("Percent of uncertainty")+
#     xlab("Year")+
#     #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
#     #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
#     scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
#     theme_bw()
#   
#   variance_plot 
#   
#   # now do this for all of the trees:
#   
#   
#   
#   
#   ##  For presentations
#   
#   my_cols <- c("#1b9e77",
#                "#d95f02",
#                "#7570b3")
#   tmpvar <- var_rel_preds
#   colnames(tmpvar) <- c( "BvarIPD", "CvarIP", "DvarI", "x")
#   var2 <- tmpvar %>%
#     gather(simtype, variance, -x)
#   
#   #var2$simtype <- factor(var2$simtype, levels = c("BvarIPD", "DvarI", "CvarIP"))
#   prop.var <- ggplot(var2, aes(x=x, fill = simtype))+
#     geom_ribbon(aes(ymin=0, ymax=variance), color = "black")+
#     ylab("Percentage of total variance (%)")+
#     xlab("Year")+
#     scale_fill_manual(values = my_cols, name = NULL, 
#                       labels = c("Process error", "Parameter uncertainty", "Initial conditions"))+
#     scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
#                        expand = c(0, 0))+
#     theme_bw()
#   
#   if(prop == TRUE){
#    prop.var
#   }else{
#     predY_plot
#   }
# }
# 
# plot.prop.variance.dbh.forecast(2, prop =TRUE)
# plot.prop.variance.dbh.forecast(2, prop =FALSE)
# 
# plot.prop.variance.inc.forecast <- function(m, prop =TRUE){
#   #-----------------------------------------------------------------------
#   # Partitioning uncertainty
#   #-----------------------------------------------------------------------
#   # Create a function for our process model (linear state space model)
#   
#   iterate_statespace.dbh <- function( x = x.mat[,"x[1,45]"], m = m, betas.all, alpha, SDdbh, covariates) {
#     
#     j <- 1
#     
#     # alpha <- alphas[j, alphaplotid] 
#     # bSDI <- betas[j,"betaSDI"]
#     # bSDI_ppt <- betas[j,"betaSDI_wintP.wateryr"]
#     # bX <-  betas[j,"betaX"]
#     # bX2 <- betas[j,"betaX2"]
#     # bX_SDI <- betas[j,"betaX_SDI"]
#     # bX_ppt <- betas[j,"betaX_wintP.wateryr"]
#     # bppt <- betas[j,"betawintP.wateryr"]
#     # btmax <- betas[j,"betatmax.fallspr"] 
#     # bX_tmax <- betas[j,"betaX_tmax.fallspr"]
#     # bSDI_tmax <- betas[j,"betaSDI_tmax.fallspr"]
#     # btmax_ppt <- betas[j,"betatmax.fallspr_wintP.wateryr"]
#     # 
#     
#     # pseudocode for now
#     tree.growth <- alpha + betas.all$b0 + 
#       betas.all$bSDI*covariates$SDI + 
#       betas.all$bSDI_ppt*covariates$SDI*covariates$ppt + 
#       betas.all$bX*(x-30) + 
#       betas.all$bX2*(x-30)*(x-30) + 
#       betas.all$bX_SDI*(x-30)*covariates$SDI + 
#       betas.all$bX_ppt*covariates$ppt*(x-30) + 
#       betas.all$bppt*covariates$ppt + 
#       betas.all$btmax*covariates$tmax + 
#       betas.all$bX_tmax*(x-30)*covariates$tmax + 
#       betas.all$bSDI_tmax*covariates$SDI*covariates$tmax +
#       betas.all$btmax_ppt*covariates$tmax*covariates$ppt 
#     #tree.growth
#     
#     
#     # Stochastic process model
#     xpred <- rnorm(length(tree.growth), tree.growth, SDdbh) 
#     
#     xpred
#   }
#   alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
#   
#   alpha <- quantile(alphas[, alphaplotid],0.5)
#   bSDI <- quantile(betas[,"betaSDI"],0.5)
#   bSDI_ppt <- quantile(betas[,"betaSDI_wintP.wateryr"],0.5)
#   bX <-  quantile(betas[,"betaX"],0.5)
#   bX2 <- quantile(betas[,"betaX2"],0.5)
#   bX_SDI <- quantile(betas[,"betaX_SDI"],0.5)
#   bX_ppt <- quantile(betas[,"betaX_wintP.wateryr"],0.5)
#   bppt <- quantile(betas[,"betawintP.wateryr"],0.5)
#   btmax <- quantile(betas[,"betatmax.fallspr"] ,0.5)
#   bX_tmax <- quantile(betas[,"betaX_tmax.fallspr"],0.5)
#   bSDI_tmax <- quantile(betas[,"betaSDI_tmax.fallspr"],0.5)
#   btmax_ppt <- quantile(betas[,"betatmax.fallspr_wintP.wateryr"],0.5)
#   b0 <- quantile(B0, 0.5)
#   
#   betas.point <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
#                             bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
#   
#   SDI <- rep(cov.data[m, ]$SDI, 45)
#   ppt <- time_data$wintP.wateryr[m,]
#   tmax <- time_data$tmax.fallspr[m,]
#   #x <- x.mat[,"x[1,45]"]
#   covariates <- data.frame(SDI, ppt, tmax)
#   
#   
#   
#   
#   
#   test <- iterate_statespace.dbh(x = x.mat[,"x[1,45]"], m = m, betas.all = betas.point, alpha, SDdbh = 0, covariates[1,])
#   #---------------------------------------------------------------------------
#   ##  Initial condition uncertainty: make forecasts from all MCMC iterations of
#   ##    the final year, but use mean parameter values and no process error.
#   time_steps <- 45
#   nMCMC <- length(x.mat[,"x[1,45]"])
#   forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
#   
#   
#   # just for 1 tree
#   for(t in 1:time_steps){
#     if(t == 1){
#       dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.point, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
#       forecast[,t] <- dbh.pred
#     }else{
#       dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.point, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
#       forecast[,t] <- dbh.pred
#       
#     }
#   }
#   varianceIC <- apply(forecast,2,var) # get variance from IC
#   forecast.ic <- apply(forecast, 2, quantile)
#   
#   #---------------------------------------------------------------------------
#   ##  Uncertainty from Initial conditions AND parameters uncertainty
#   #---------------------------------------------------------------------------
#   # use all of the parameter MCMCS:
#   alpha <- alphas[57001:60300, alphaplotid]
#   bSDI <- betas[57001:60300,"betaSDI"]
#   bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
#   bX <-  betas[57001:60300,"betaX"]
#   bX2 <- betas[57001:60300,"betaX2"]
#   bX_SDI <- betas[57001:60300,"betaX_SDI"]
#   bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
#   bppt <- betas[57001:60300,"betawintP.wateryr"]
#   btmax <- betas[57001:60300,"betatmax.fallspr"]
#   bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
#   bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
#   btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
#   b0 <- B0[57001:60300]
#   
#   betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
#                           bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
#   
#   SDI <- rep(cov.data[m, ]$SDI, 45)
#   ppt <- time_data$wintP.wateryr[m,]
#   tmax <- time_data$tmax.fallspr[m,]
#   x <- x.mat[,"x[1,45]"]
#   covariates <- data.frame(SDI, ppt, tmax)
#   
#   
#   
#   
#   time_steps <- 45
#   nMCMC <- length(x.mat[,"x[1,45]"])
#   forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
#   
#   for(t in 1:time_steps){
#     if(t == 1){
#       dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
#       forecast[,t] <- dbh.pred
#     }else{
#       dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
#       forecast[,t] <- dbh.pred
#       
#     }  }
#   varianceIC_Parameters <- apply(forecast,2,var)
#   forecast.ic.param <- apply(forecast, 2, quantile)
#   
#   #---------------------------------------------------------------------------
#   ##  Uncertainty from IC AND parameters uncertainty AND process error
#   #---------------------------------------------------------------------------
#   # use all of the parameter MCMCS:
#   alpha <- alphas[57001:60300, alphaplotid]
#   bSDI <- betas[57001:60300,"betaSDI"]
#   bSDI_ppt <- betas[57001:60300,"betaSDI_wintP.wateryr"]
#   bX <-  betas[57001:60300,"betaX"]
#   bX2 <- betas[57001:60300,"betaX2"]
#   bX_SDI <- betas[57001:60300,"betaX_SDI"]
#   bX_ppt <- betas[57001:60300,"betaX_wintP.wateryr"]
#   bppt <- betas[57001:60300,"betawintP.wateryr"]
#   btmax <- betas[57001:60300,"betatmax.fallspr"]
#   bX_tmax <- betas[57001:60300,"betaX_tmax.fallspr"]
#   bSDI_tmax <- betas[57001:60300,"betaSDI_tmax.fallspr"]
#   btmax_ppt <- betas[57001:60300,"betatmax.fallspr_wintP.wateryr"]
#   b0 <- B0[57001:60300]
#   
#   betas.all <- data.frame(b0, bSDI, bSDI_ppt, bX, bX2, bX_SDI, bX_ppt, 
#                           bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
#   
#   SDI <- rep(cov.data[m, ]$SDI, 45)
#   ppt <- time_data$wintP.wateryr[m,]
#   tmax <- time_data$tmax.fallspr[m,]
#   x <- x.mat[,"x[1,45]"]
#   covariates <- data.frame(SDI, ppt, tmax)
#   
#   Sdev <- 1/quantile(out[,"tau_inc"], 0.5)
#   
#   
#   for(t in 1:time_steps){
#     if(t == 1){
#       dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", t,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = Sdev, covariates = covariates[t,])
#       forecast[,t] <- dbh.pred
#     }else{
#       dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = Sdev, covariates = covariates[t,])
#       forecast[,t] <- dbh.pred
#       
#     }  }
#   varianceIC_Parameters_process <- apply(forecast,2,var)
#   forecast.ic.param.process <- apply(forecast, 2, quantile)
#   
#   
#   # combine variances:
#   V.pred.sim     <- rbind(varianceIC_Parameters_process,varianceIC_Parameters,varianceIC)
#   V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
#   
#   # combine forecasts:
#   pred.sims     <- data.frame(IPP.0 = forecast.ic.param.process[1,],
#                               IP.0 =forecast.ic.param[1,],
#                               I.0 =forecast.ic[1,],
#                               IPP.100= forecast.ic.param.process[5,],
#                               IP.100=forecast.ic.param[5,],
#                               I.100=forecast.ic[5,],
#                               year = 1:45)
#   my_cols <- c("#1b9e77",
#                "#d95f02",
#                "#7570b3")
#   
#   predY_plot <- ggplot(data=pred.sims, aes(x=year))+
#     geom_ribbon(aes(ymin=IPP.0, ymax=IPP.100), fill=my_cols[3])+
#     geom_ribbon(aes(ymin=IP.0, ymax=IP.100), fill=my_cols[2])+
#     geom_ribbon(aes(ymin=I.0, ymax=I.100+0.001), fill=my_cols[1])+
#     
#     
#     ylab("Predicted DBH")+
#     xlab("Year")+
#     #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
#     #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
#     #scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
#     theme_bw()
#   predY_plot
#   # pred.sim.0rel <- apply(pred.sim.0,2,function(x) {x/max(x)})
#   # 
#   # pred.sim.75     <- rbind(forecast.ic.param.process[5,],
#   #                          forecast.ic.param[5,],
#   #                          forecast.ic[5,])
#   # pred.sim.75rel <- apply(pred.sim.75,2,function(x) {x/max(x)})
#   # 
#   # plot(1:45, pred.sim.0[1,], type = "l")
#   # lines(1:45, pred.sim.75[1,], col= "red")
#   ####
#   ####  PLOT THE FORECASTING UNCERTAINTY PARTITION -------------------------------
#   ####
#   var_rel_preds <- as.data.frame(t(V.pred.sim.rel*100))
#   var_rel_preds$x <- 1:nrow(var_rel_preds)
#   #my_cols <- c("black", "grey55", "grey70")
#   my_cols <- c("#1b9e77",
#                "#d95f02",
#                "#7570b3")
#   variance_plot <- ggplot(data=var_rel_preds, aes(x=x))+
#     geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters_process), fill=my_cols[3])+
#     geom_ribbon(aes(ymin=0, ymax=varianceIC_Parameters),fill=my_cols[2])+
#     geom_ribbon(aes(ymin=0, ymax=varianceIC), fill=my_cols[1])+
#     
#     
#     ylab("Percent of uncertainty")+
#     xlab("Year")+
#     #scale_x_continuous(breaks=seq(1,time_steps,by=1), 
#     #                  labels=paste(seq(1,time_steps,by=1), "yrs"))+
#     scale_y_continuous(labels=paste0(seq(0,100,25),"%"))+
#     theme_bw()
#   
#   variance_plot 
#   
#   # now do this for all of the trees:
#   
#   
#   
#   
#   ##  For presentations
#   
#   my_cols <- c("#1b9e77",
#                "#d95f02",
#                "#7570b3")
#   tmpvar <- var_rel_preds
#   colnames(tmpvar) <- c( "BvarIPD", "CvarIP", "DvarI", "x")
#   var2 <- tmpvar %>%
#     gather(simtype, variance, -x)
#   
#   #var2$simtype <- factor(var2$simtype, levels = c("BvarIPD", "DvarI", "CvarIP"))
#   prop.var <- ggplot(var2, aes(x=x, fill = simtype))+
#     geom_ribbon(aes(ymin=0, ymax=variance), color = "black")+
#     ylab("Percentage of total variance (%)")+
#     xlab("Year")+
#     scale_fill_manual(values = my_cols, name = NULL, 
#                       labels = c("Process error", "Parameter uncertainty", "Initial conditions"))+
#     scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
#                        expand = c(0, 0))+
#     theme_bw()
#   
#   if(prop == TRUE){
#     prop.var
#   }else{
#     predY_plot
#   }
# }
# plot.prop.variance.inc.forecast(2, prop =TRUE)
# plot.prop.variance.inc.forecast(2, prop =FALSE)

#-------------------------------------------------------------------
# set up to forecast through time from just the last X condiation
# using future climate time series (2018-2099)
# note that this version also includes driver uncertainty
#-------------------------------------------------------------------

# driver uncertainty:
# for each scenario, in each year, for each plot, we sample from the distribution of ppt and tmax
# that has a mean of the ensemble mean, and variance of the ensemble variance

# read the rds with climate scenarios:
clim.data <- readRDS("PRISM_non_scaled.rds")

clim.ts <- readRDS("data/pipo.cores.ds.mean.correct.climate_2018_2099.RDS")
colnames(clim.ts)[6:7] <-c("year.ppt", "tmax.fall.spr") 

clim.ts.df <- clim.ts #$future.climate.ts
clim.ts.df$tmax.fall.spr[is.nan(clim.ts.df$tmax.fall.spr)] <- NA
#tmax.fallspr.df <- tmax.fallspr


# need to scale future climate data on the same scale as the past climate
clim.ts.df$ppt.scale <-(clim.ts.df$year.ppt-mean(as.matrix(clim.data$wintP.wateryr)))/sd(as.matrix(clim.data$wintP.wateryr))
clim.ts.df$tmax.scaled <-(clim.ts.df$tmax.fall.spr-mean(as.matrix(clim.data$tmax.fallspr)))/sd(as.matrix(clim.data$tmax.fallspr))


rm(tmax.fallspr)
climate.ensemble.means <- clim.ts.df %>% group_by(lat, lon, year, rcp) %>% 
  dplyr::summarise(mean.tmax.fs = mean(tmax.scaled, na.rm = TRUE), 
                   SD.tmax = var(tmax.scaled, na.rm = TRUE),
                   mean.ppt = mean(ppt.scale, na.rm = TRUE), 
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



#--------------------------------------------------------------------------------------------------
# function for partitioning uncertinaty into driver, IC, random effects, parameter error, and process error
#--------------------------------------------------------------------------------------------------

plot.prop.variance.future.forecast.driver <- function(m, prop = TRUE, scenario = "rcp26", type = "dbh", print =TRUE, Xplot = FALSE){
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
  alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
  
  alpha <- quantile(alphas[, alphaplotid],0.5)
  bSDI <- quantile(betas[,"betaSDI"],0.5)
  bSDI_ppt <- quantile(betas[,"betaSDI_wintP.wateryr"],0.5)
  
  if(Xplot == TRUE){
    BXid <- paste0("betaX_PLOT[", cov.data[m,]$plotid, "]")
    bX <-  quantile(betaXplots[,BXid],0.5)
  }else{
    bX <- quantile(betas[,"betaX"], 0.5)
  }
  bX2 <- quantile(betas[,"betaX2"],0.5)
  
  if("betaX_SDI" %in% colnames(betas) == FALSE ){
    bX_SDI <- 0
  }else{
    bX_SDI <- quantile(betas[,"betaX_SDI"],0.5)
  }
  
  if("betaSICOND" %in% colnames(betas) == FALSE ){
    bSI <- 0
    bSI_X <- 0
    bSI_wintP.wateryr <- 0
    bSI_tmax.fallspr <- 0
  }else{
    bSI <- quantile(betas[,"betaSICOND"],0.5)
    bSI_X <- quantile(betas[,"betaX_SICOND"],0.5)
    bSI_wintP.wateryr <- quantile(betas[,"betaSICOND_wintP.wateryr"],0.5)
    bSI_tmax.fallspr <- quantile(betas[,"betaSICOND_tmax.fallspr"],0.5)
  }
  
  bX_ppt <- quantile(betas[,"betaX_wintP.wateryr"],0.5)
  bppt <- quantile(betas[,"betawintP.wateryr"],0.5)
  btmax <- quantile(betas[,"betatmax.fallspr"] ,0.5)
  bX_tmax <- quantile(betas[,"betaX_tmax.fallspr"],0.5)
  bSDI_tmax <- quantile(betas[,"betaSDI_tmax.fallspr"],0.5)
  btmax_ppt <- quantile(betas[,"betatmax.fallspr_wintP.wateryr"],0.5)
  b0 <- quantile(B0, 0.5)
  
  betas.point <- data.frame(b0, bSDI, bSDI_ppt, bSI, bSI_tmax.fallspr, bSI_wintP.wateryr,
                            bSI_X, bX, bX2, bX_SDI, bX_ppt, 
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
  SICOND <- rep(cov.data[m, ]$SICOND, length(ppt))
  #x <- x.mat[,"x[1,53]"]
  covariates <- data.frame(SDI,SICOND, ppt, tmax)
  
  
  
  
  
  #test <- iterate_statespace.dbh(x = x.mat[,"x[1,53]"], m = m, betas.all = betas.point, alpha, SDdbh = 0, covariates[1,], rw = type)
  #---------------------------------------------------------------------------
  ##  Initial condition uncertainty: make forecasts from all MCMC iterations of
  ##    the final year, but use mean parameter values and no process error.
  time_steps <- length(ppt)
  nMCMC <- length(x.mat[,"x[1,53]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  # just for 1 tree # note that we start at x = 53
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", 53,"]")], m = m, betas.all = betas.point, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t]-x.mat[,paste0("x[", m,",", 53,"]")]
      
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.point, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t]-forecast[,t-1]
      
    }
    
    
  }
  varianceIC <- apply(forecast,2,var) # get variance from IC
  forecast.ic <- apply(forecast, 2, function(x){quantile(x, c(0.025, 0.25, 0.5, 0.975), na.rm = TRUE)})
  
  var.inc.ic <- apply(inc, 2, var)
  inc.ic <- apply(inc, 2, function(x){quantile(x, c(0.025, 0.25, 0.5, 0.975), na.rm = TRUE)})
  
  set.seed(22)
  #---------------------------------------------------------------------------
  ##  Uncertainty from Initial conditions AND beta parameters uncertainty
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  alpha = quantile(alphas[, alphaplotid],0.5)
  #alpha <- rnorm(length(7501:15300), mean = mean(alphas[7501:15300, alphaplotid]), sd = sd(alphas[7501:15300, alphaplotid]))
  bSDI <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betaSDI"]), sd = sd(betas[7501:15300,"betaSDI"]))
  bSDI_ppt <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betaSDI_wintP.wateryr"]), sd = sd(betas[7501:15300,"betaSDI_wintP.wateryr"]))
  
  # if there are random effects on X
  if(Xplot == TRUE){
    BXid <- paste0("betaX_PLOT[", cov.data[m,]$plotid, "]")
    
    # bX <-  rnorm(length(7501:15300), mean = mean(betaXplots[,BXid]), sd = sd(betaXplots[,BXid]))
    bX <- quantile(betaXplots[,BXid], 0.5)
  }else{
    bX <- quantile(betas[, "betaX"],0.5) 
  }
  bX2 <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betaX2"]), sd = sd(betas[7501:15300,"betaX2"]))
  
  if("betaX_SDI" %in% colnames(betas) == FALSE ){
    bX_SDI <- rep(0,7800 )
  }else{
    
    bX_SDI <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betaX_SDI"]), sd = sd(betas[7501:15300,"betaX_SDI"]))
    
  }
  
  if("betaSICOND" %in% colnames(betas) == FALSE ){
    bSI <- rep(0,7800 )
    bSI_X <- rep(0,7800 )
    bSI_wintP.wateryr <- rep(0,7800 )
    bSI_tmax.fallspr <- rep(0,7800 )
  }else{
    bSI <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betaSICOND"]), sd = sd(betas[7501:15300,"betaSICOND"]))
    bSI_X <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betaX_SICOND"]), sd = sd(betas[7501:15300,"betaX_SICOND"]))
    bSI_wintP.wateryr <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betaSICOND_wintP.wateryr"]), sd = sd(betas[7501:15300,"betaSICOND_wintP.wateryr"]))
    
    bSI_tmax.fallspr <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betaSICOND_tmax.fallspr"]), sd = sd(betas[7501:15300,"betaSICOND_tmax.fallspr"]))
    
  }
  
  bX_ppt <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betaX_wintP.wateryr"]), sd = sd(betas[7501:15300,"betaX_wintP.wateryr"]))
  bppt <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betawintP.wateryr"]), sd = sd(betas[7501:15300,"betawintP.wateryr"]))
  btmax <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betatmax.fallspr"]), sd = sd(betas[7501:15300,"betatmax.fallspr"]))
  bX_tmax <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betaX_tmax.fallspr"]), sd = sd(betas[7501:15300,"betaX_tmax.fallspr"]))
  bSDI_tmax <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betaSDI_tmax.fallspr"]), sd = sd(betas[7501:15300,"betaSDI_tmax.fallspr"]))
  btmax_ppt <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betatmax.fallspr_wintP.wateryr"]), sd = sd(betas[7501:15300,"betaSDI_tmax.fallspr"]))
  b0 <- rnorm(length(7501:15300), mean = mean(B0[7501:15300]), sd = sd(B0[7501:15300]))
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bSI, bSI_tmax.fallspr, bSI_wintP.wateryr,
                          bSI_X, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  SDI <- rep(cov.data[m, ]$SDI, 53)
  SICOND <- rep(cov.data[m, ]$SICOND, 53)
  ppt <- time_data$wintP.wateryr[m,]
  tmax <- time_data$tmax.fallspr[m,]
  #x <- x.mat[,"x[1,53]"]
  
  # code toe filter out future climate data for the tree id and the rcp:
  
  #future.proj <- ens.means[ens.means$id == m & ens.means$rcp == scenario, ]
  #proj.ordered <- future.proj[order(future.proj$year),]
  
  # just use the ensemble means (w.out driver uncertainty)
  ppt <- proj.ordered$mean.ppt
  tmax <- proj.ordered$mean.tmax.fs
  #ppt <- time_data$wintP.wateryr[m,]
  #tmax <- time_data$tmax.fallspr[m,]
  SDI <- rep(cov.data[m, ]$SDI, length(ppt))
  SICOND <- rep(cov.data[m, ]$SICOND, length(ppt))
  #x <- x.mat[,"x[1,53]"]
  covariates <- data.frame(SDI, ppt, tmax, SICOND)
  
  time_steps <- length(ppt)
  nMCMC <- length(x.mat[,"x[1,53]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", 53,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t]-x.mat[,paste0("x[", m,",", 53,"]")]
      
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t]-forecast[,t-1]
      
      
    }  }
  varianceIC_Parameters <- apply(forecast,2,var)
  forecast.ic.param <- apply(forecast, 2, function(x){quantile(x, c(0.025, 0.25, 0.5, 0.975), na.rm = TRUE)})
  
  var.inc.IC_Parameters <- apply(inc,2,var)
  inc.ic.param <- apply(inc, 2, function(x){quantile(x, c(0.025, 0.25, 0.5, 0.975), na.rm = TRUE)})
  #---------------------------------------------------------------------------
  ##  Uncertainty from Initial conditions AND parameters uncertainty AND plot random effects
  #---------------------------------------------------------------------------
  
  
  alpha <- rnorm(length(7501:15300), mean = mean(alphas[7501:15300, alphaplotid]), sd = sd(alphas[7501:15300, alphaplotid]))
  
  if(Xplot == TRUE){
    BXid <- paste0("betaX_PLOT[", cov.data[m,]$plotid, "]")
    
    bX <-  rnorm(length(7501:15300), mean = mean(betaXplots[7501:15300,BXid]), sd = sd(betaXplots[7501:15300,BXid]))
  }
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bSI, bSI_tmax.fallspr, bSI_wintP.wateryr,
                          bSI_X, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  
  #ppt <- time_data$wintP.wateryr[m,]
  #tmax <- time_data$tmax.fallspr[m,]
  #x <- x.mat[,"x[1,53]"]
  
  # code toe filter out future climate data for the tree id and the rcp:
  
  #future.proj <- ens.means[ens.means$id == m & ens.means$rcp == scenario, ]
  #proj.ordered <- future.proj[order(future.proj$year),]
  
  # just use the ensemble means (w.out driver uncertainty)
  
  
  ppt <- proj.ordered$mean.ppt
  tmax <- proj.ordered$mean.tmax.fs
  #ppt <- time_data$wintP.wateryr[m,]
  #tmax <- time_data$tmax.fallspr[m,]
  
  #x <- x.mat[,"x[1,53]"]
  covariates <- data.frame(SDI, ppt, tmax, SICOND)
  
  time_steps <- length(ppt)
  nMCMC <- length(x.mat[,"x[1,53]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", 53,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t]-x.mat[,paste0("x[", m,",", 53,"]")]
      
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t]-forecast[,t-1]
      
    }  }
  varianceIC_Parameters.alpha <- apply(forecast,2,var)
  forecast.ic.param.alpha <- apply(forecast, 2, function(x){quantile(x, c(0.025, 0.25, 0.5, 0.975), na.rm = TRUE)})
  
  var.inc.IC_Parameters.alpha <- apply(inc,2,var)
  inc.ic.param.alpha <- apply(inc, 2, function(x){quantile(x, c(0.025, 0.25, 0.5, 0.975), na.rm = TRUE)})
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from Initial conditions AND parameters uncertainty AND future Driver uncertainty
  #---------------------------------------------------------------------------
  
  # use all of the parameter MCMCS:
  
  # 
  # just use the ensemble means (w.out driver uncertainty)
  ppt <- tmax <- SDI <-SICOND <- matrix(NA, nrow =1000, ncol = 82 )
  yeardf<- 2018:2099
  
  # want to sample 1 of the 21-31 models (not all models have all RCP scenarios) for each new sample:
  models <- unique(ens.proj.ordered$modelrun)
  sample.model <- sample(models, size = length(models), replace= FALSE)
  
  
  # for(i in 1:1000){
  #   
  #   ens.proj.yr <- ens.proj.ordered %>% filter(modelrun %in% sample.model[i])
  #   ens.proj.yr <- ens.proj.yr [!duplicated(ens.proj.yr),]
  #   
  #   # just sample from the distribution of climate:
  #   ppt[i,] <- ens.proj.yr$ppt.scale
  #   tmax[i,] <- ens.proj.yr$tmax.scaled
  #   
  #   #ppt[,i]<- rnorm(n = length(7501:15300) , mean = proj.ordered[i,]$mean.ppt, sd = proj.ordered[i,]$SD.ppt)
  #   #tmax[,i]<- rnorm(n = length(7501:15300) , mean = proj.ordered[i,]$mean.tmax.fs, sd = proj.ordered[i,]$SD.tmax)
  #   SDI[i,]<- rep(cov.data[m, ]$SDI, 82)
  #   SICOND[i,]<- rep(cov.data[m, ]$SICOND, 82)
  #   df <- data.frame(ppt, tmax, SDI, SICOND)
  #   df
  # }
  get.ens.df <- function(i){
    
    ens.proj.yr <- ens.proj.ordered %>% filter(modelrun %in% sample.model[i])
    ens.proj.yr <- ens.proj.yr [!duplicated(ens.proj.yr),]
    
    # # just sample from the distribution of climate:
    # ppt <- ens.proj.yr$ppt.scale
    # tmax <- ens.proj.yr$tmax.scaled
    # 
    # #ppt[,i]<- rnorm(n = length(7501:15300) , mean = proj.ordered[i,]$mean.ppt, sd = proj.ordered[i,]$SD.ppt)
    # #tmax[,i]<- rnorm(n = length(7501:15300) , mean = proj.ordered[i,]$mean.tmax.fs, sd = proj.ordered[i,]$SD.tmax)
    # SDI<- rep(cov.data[m, ]$SDI, 82)
    # SICOND<- rep(cov.data[m, ]$SICOND, 82)
    df <- data.frame(ppt = ens.proj.yr$ppt.scale, 
                     tmax = ens.proj.yr$tmax.scaled, 
                     SDI =  rep(cov.data[m, ]$SDI, 82), 
                     SICOND =  rep(cov.data[m, ]$SDI, 82), 
                     i = i, 
                     year = ens.proj.yr$year)
    df
  }
  
  ens.samps <- lapply(1:length(models), get.ens.df)
  ens.samps.df <- do.call(rbind, ens.samps)
  # for(i in 2018:2099){
  # 
  #   
  #   ppt[,i]<- sample(x =  ens.proj.ordered[ens.proj.ordered$year == i,]$ppt.scale, replace = TRUE, size = length(7501:15300))
  #   tmax[,i]<- sample(n = length(7501:15300) , ens.proj.ordered[ens.proj.ordered$year == i,]$tmax.scale)
  #   SDI[,i]<- rep(cov.data[m, ]$SDI, length(7501:15300))
  #   
  # }
  #tmax <- proj.ordered$mean.tmax.fs
  #ppt <- time_data$wintP.wateryr[m,]
  #tmax <- time_data$tmax.fallspr[m,]
  #SDI <- rep(cov.data[m, ]$SDI, length(ppt))
  #x <- x.mat[,"x[1,53]"]
  # covariates <- list()
  # covariates$SDI <- SDI
  # covariates$ppt <- ppt
  # covariates$tmax <- tmax
  # covariates$SICOND <- SICOND
  #covariates <- list(SDI, ppt, tmax)
  
  
  
  time_steps <- 82
  nMCMC <- length(x.mat[,"x[1,53]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", 53,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = data.frame(SDI = ens.samps.df %>% filter(year == t + 2017) %>% select(SDI), 
                                                                                                                                                                  ppt = ens.samps.df %>% filter(year == t + 2017) %>% select(ppt), 
                                                                                                                                                                  tmax = ens.samps.df %>% filter(year == t + 2017) %>% select(tmax), 
                                                                                                                                                                  SICOND = ens.samps.df %>% filter(year == t + 2017) %>% select(SICOND)))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-x.mat[,paste0("x[", m,",", 53,"]")]
      
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = data.frame(SDI = ens.samps.df %>% filter(year == t + 2017) %>% select(SDI), 
                                                                                                                                             ppt = ens.samps.df %>% filter(year == t + 2017) %>% select(ppt), 
                                                                                                                                             tmax = ens.samps.df %>% filter(year == t + 2017) %>% select(tmax), 
                                                                                                                                             SICOND = ens.samps.df %>% filter(year == t + 2017) %>% select(SICOND)))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-forecast[,t-1]
      
    }  }
  varianceIC_Parameters_driver <- apply(forecast,2,var)
  forecast.ic.param.d <- apply(forecast, 2,function(x){quantile(x, c(0.025, 0.25, 0.5, 0.975), na.rm = TRUE)})
  
  var.inc.IC_Parameters_driver <- apply(inc,2,var)
  inc.ic.param.d <- apply(inc, 2,function(x){quantile(x, c(0.025, 0.25, 0.5, 0.975), na.rm = TRUE)})
  
  #---------------------------------------------------------------------------
  ##  Uncertainty from IC AND parameters uncertainty AND process error
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  
  
  time_steps <- 82
  nMCMC <- length(x.mat[,"x[1,53]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  
  Sdev <- sqrt(1/quantile(out[,"tau_add"], 0.5))
  Sdevinc <- 1/quantile(out[,"tau_inc"], 0.5)
  
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = x.mat[,paste0("x[", m,",", 53,"]")], m = m, betas.all = betas.all, alpha = alpha, SDdbh = Sdev,SDinc = Sdevinc,  covariates = data.frame(SDI = ens.samps.df %>% filter(year == t + 2017) %>% select(SDI), 
                                                                                                                                                                                      ppt = ens.samps.df %>% filter(year == t + 2017) %>% select(ppt), 
                                                                                                                                                                                      tmax = ens.samps.df %>% filter(year == t + 2017) %>% select(tmax), 
                                                                                                                                                                                      SICOND = ens.samps.df %>% filter(year == t + 2017) %>% select(SICOND)))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-x.mat[,paste0("x[", m,",", 53,"]")]
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = Sdev, SDinc = Sdevinc,covariates = data.frame(SDI = ens.samps.df %>% filter(year == t + 2017) %>% select(SDI), 
                                                                                                                                                                ppt = ens.samps.df %>% filter(year == t + 2017) %>% select(ppt), 
                                                                                                                                                                tmax = ens.samps.df %>% filter(year == t + 2017) %>% select(tmax), 
                                                                                                                                                                SICOND = ens.samps.df %>% filter(year == t + 2017) %>% select(SICOND)))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-forecast[,t-1]
      
    }  }
  varianceIC_Parameters_process.driver <- apply(forecast,2,var)
  forecast.ic.param.process.driver <- apply(forecast, 2, function(x){quantile(x, c(0.025, 0.25, 0.5, 0.975), na.rm = TRUE)})
  
  var.inc.IC_Parameters_process.driver <- apply(inc,2,var)
  inc.ic.param.process.driver <- apply(inc, 2, function(x){quantile(x, c(0.025, 0.25, 0.5, 0.975),na.rm = TRUE)})
  
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
                                IPP.100= forecast.ic.param.process.driver[4,],
                                IPD.100 = forecast.ic.param.d[4,],
                                IPA.100 =forecast.ic.param.alpha[4,],
                                IP.100=forecast.ic.param[4,],
                                I.100=forecast.ic[4,],
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
                                IPP.100= inc.ic.param.process.driver[4,],
                                IPD.100 = inc.ic.param.d[4,],
                                IPA.100 = inc.ic.param.alpha[4,],
                                IP.100=inc.ic.param[4,],
                                I.100=inc.ic[4,],
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
    geom_ribbon(aes(ymin=Low, ymax=High))+
    ylab(axis.name)+
    xlab("Year")+theme_bw()+
    scale_fill_manual(values = my_cols, name = NULL)+ theme(legend.position = "none", panel.grid = element_blank())#+ylim(-10, 1)
  
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
  
  write.csv(variance.df, paste0("variance_partitioning/tree_", m,"_", axis.name, "_", scenario,"_",output.base.name,"_proportion", ".csv"))
  write.csv(pred.sims.class, paste0("variance_partitioning/tree_", m,"_", axis.name, "_", scenario,"_",output.base.name,"_totalunc", ".csv"))
  
  #write.csv(variance.df, paste0("variance_partitioning/tree_", m,"_", "Diameter", "_", scenario,"_",output.base.name,"_proportion", ".csv"))
  #write.csv(pred.sims.class, paste0("variance_partitioning/tree_", m,"_", "Diameter", "_", scenario,"_",output.base.name,"_totalunc", ".csv"))
  
  if(print == TRUE){
    if(prop == TRUE){
      prop.var
      
    }else{
      predY_plot
    }
  }else{
    cat(m)
  }
}
system.time(tree.1 <- plot.prop.variance.future.forecast.driver(m = 1, prop = TRUE, scenario = "rcp26", type = "ringwidth", print = TRUE))

tree1.dbh <- plot.prop.variance.future.forecast.driver(m = 12, prop = FALSE, scenario = "rcp26", type = "dbh", print = TRUE)

tree1.dbh.tot <- plot.prop.variance.future.forecast.driver(m = 1, prop = FALSE, scenario = "rcp26", type = "dbh", print = TRUE)
tree1.inc.tot <- plot.prop.variance.future.forecast.driver(m = 1, prop = FALSE, scenario = "rcp26", type = "ringwidth", print = TRUE)
tree1.inc.tot <- plot.prop.variance.future.forecast.driver(m = 1, prop = FALSE, scenario = "rcp85", type = "ringwidth", print = TRUE)
tree1.inc.tot <- plot.prop.variance.future.forecast.driver(m = 1, prop = TRUE, scenario = "rcp85", type = "dbh", print = TRUE)
tree12.inc.tot <- plot.prop.variance.future.forecast.driver(m = 12, prop = FALSE, scenario = "rcp85", type = "dbh", print = TRUE)

#treeds <- c(1:38, 43:54)
treeds <- c(1:515)
# proportions for rcp 2.6
for(k in treeds){
  plot.prop.variance.future.forecast.driver(m = k, prop = TRUE, scenario = "rcp26", type = "ringwidth", print = FALSE, Xplot = FALSE)
}

for(k in treeds){
  plot.prop.variance.future.forecast.driver(m = k, prop = TRUE, scenario = "rcp26", type = "dbh", print = FALSE, Xplot = FALSE)
}

# rcp 4.5
for(k in treeds){
  plot.prop.variance.future.forecast.driver(m = k, prop = TRUE, scenario = "rcp45", type = "ringwidth", print = FALSE, Xplot = FALSE)
}

for(k in treeds){
  plot.prop.variance.future.forecast.driver(m = k, prop = TRUE, scenario = "rcp45", type = "dbh", print = FALSE, Xplot = FALSE)
}

# rcp 6.0
for(k in treeds){
  plot.prop.variance.future.forecast.driver(m = k, prop = TRUE, scenario = "rcp60", type = "ringwidth", print = FALSE, Xplot = FALSE)
}

for(k in treeds){
  plot.prop.variance.future.forecast.driver(m = k, prop = TRUE, scenario = "rcp60", type = "dbh", print = FALSE, Xplot = FALSE)
}

# rcp 8.5
for(k in treeds){
  plot.prop.variance.future.forecast.driver(m = k, prop = TRUE, scenario = "rcp85", type = "ringwidth", print = FALSE, Xplot = FALSE)
}

for(k in treeds){
  plot.prop.variance.future.forecast.driver(m = k, prop = TRUE, scenario = "rcp85", type = "dbh", print = FALSE, Xplot = FALSE)
}
#------------------------------------------------------------------------------------------
#  Summarise the tree ring increment proportion of uncertainty
#------------------------------------------------------------------------------------------
inc.prop.list <- list()
treeds <- c(1:515)
df.2 <- list()
for(xy in treeds){
  df <- read.csv(paste0("variance_partitioning/tree_", xy,"_Increment_rcp45_", output.base.name, "_proportion.csv"))
  df$treeid <- xy
  df$rcp <- "rcp4.5"
  df.2[[xy]]<- df
}
inc.prop.list.2.6 <- lapply(treeds, function(xy){df <- read.csv(paste0("variance_partitioning/tree_", xy,"_Increment_rcp26_", output.base.name, "_proportion.csv"))
df$treeid <- xy
df$rcp <- "rcp2.6"
df})
inc.prop <- do.call(rbind, inc.prop.list.2.6)


inc.prop.list.45 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning/tree_", x,"_Increment_rcp45_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp4.5"
df})
inc.prop.45 <- do.call(rbind, inc.prop.list.45)

inc.prop.list.60 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning/tree_", x,"_Increment_rcp60_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp6.0"
df})
inc.prop.60 <- do.call(rbind, inc.prop.list.60)



inc.prop.list.85 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning/tree_", x,"_Increment_rcp85_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp8.5"
df})
inc.prop.85 <- do.call(rbind, inc.prop.list.85)

inc.prop.all <- rbind(inc.prop, inc.prop.45, inc.prop.60, inc.prop.85)
saveRDS(inc.prop.all, paste0("inc.prop.all_",output.base.name, ".RDS"))
#inc.prop <- read.csv("variance_partitioning/tree_100_Increment_rcp26_proportion.csv")


inc.prop.wide <- inc.prop.all %>% select(-X,) %>% group_by(x,year, rcp) %>% spread(simtype, variance)
inc.prop.wide$InitialCondition <-  inc.prop.wide$`Initial Conditions`
inc.prop.wide$ProcessError <-  inc.prop.wide$`Process Error` - inc.prop.wide$`Driver Uncertainty`
inc.prop.wide$ParameterUnc <-  inc.prop.wide$`Parameter Uncertainty` - inc.prop.wide$`Initial Conditions`
inc.prop.wide$PlotrandUnc <-  inc.prop.wide$`Plot random effect` - inc.prop.wide$`Parameter Uncertainty`
inc.prop.wide$DriverUnc <-  inc.prop.wide$`Driver Uncertainty` - inc.prop.wide$`Plot random effect`

inc.prop.wide$period <- ifelse(inc.prop.wide$year >= 2075, "2075 - 2099", 
                               ifelse(inc.prop.wide$year >= 2050 & inc.prop.wide$year < 2075, "2050 - 2074", 
                                      ifelse(inc.prop.wide$year < 2050, "2019 - 2049", NA)))

inc.prop.long <- inc.prop.wide %>% group_by(period, year, treeid, x, rcp) %>% select(-`Driver Uncertainty` , -`Initial Conditions`, -`Parameter Uncertainty`,
                                                                                     -`Plot random effect`, -`Process Error`) %>%
  gather(Uncertainty, value ,-x, -year, -treeid, -period, -rcp)
#inc.prop.wide <- reshape2::melt(inc.prop.wide)
rcp.inc.prop <- inc.prop.long %>% group_by(period, Uncertainty, rcp) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                                                   ci.lo = quantile(value, 0.025, na.rm=TRUE), 
                                                                                   ci.hi = quantile(value, 0.975, na.rm =TRUE)) 
rcp.inc.prop$Uncertainty <- factor(rcp.inc.prop$Uncertainty , levels = c("ProcessError", 
                                                                         "DriverUnc", 
                                                                         "PlotrandUnc", 
                                                                         
                                                                         "ParameterUnc", 
                                                                         "InitialCondition"))

Uncertainty.table <- data.frame(Uncertainty = c("ProcessError", 
                                                "DriverUnc", 
                                                "PlotrandUnc", 
                                                "ParameterUnc", 
                                                "InitialCondition"),
                                Unc = c("Process", 
                                        "Driver", 
                                        "Random Effects", 
                                        "Parameter", 
                                        "Initial Conditions"))


rcp.inc.proportion.summary <- merge(rcp.inc.prop, Uncertainty.table, by = "Uncertainty")

rcp.inc.proportion.summary$Unc <- factor(rcp.inc.proportion.summary$Unc , levels = c("Process", 
                                                                                     "Driver", 
                                                                                     "Random Effects", 
                                                                                     "Parameter", 
                                                                                     "Initial Conditions"))


my_cols <- c("#1b9e77",
             "#d95f02",
             "black",
             "#7570b3", 
             "grey")
#ggplot( inc.prop.long, aes(x = rcp , y = value, fill = Uncertainty))+geom_boxplot()+facet_wrap(~period)
increment.unc.summary.prop <- ggplot(rcp.inc.proportion.summary, aes(x = rcp , y = mean, fill = Unc))+geom_bar(stat = "identity")+facet_wrap(~period)+
  ylab("Proportion of Uncertainty\n in Increment")+xlab("RCP Scenario")+scale_fill_manual(values = my_cols, name = NULL)+
  theme_bw(base_size = 12)+theme(axis.text.x = element_text(hjust = 1, angle = 45), panel.grid = element_blank())
increment.unc.summary.prop

png(height = 4, width = 6, units = "in", res = 300, paste0(output.base.name,  paste0("_Proportion_increment_unc_proportion_all_rcp_all_time",output.base.name,".png")))
increment.unc.summary.prop
dev.off()


#------------------------------------------------------------------------------------------
#  Summarise the tree ring Diameter proportion of uncertainty
#------------------------------------------------------------------------------------------
dbh.prop.list <- list()
treeds <- c(1:515)
dbh.prop.list.2.6 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning/tree_", x,"_Diameter_rcp26_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp2.6"
df})
dbh.prop <- do.call(rbind, dbh.prop.list.2.6)


dbh.prop.list.45 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning/tree_", x,"_Diameter_rcp45_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp4.5"
df})
dbh.prop.45 <- do.call(rbind, dbh.prop.list.45)

dbh.prop.list.60 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning/tree_", x,"_Diameter_rcp60_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp6.0"
df})
dbh.prop.60 <- do.call(rbind, dbh.prop.list.60)



dbh.prop.list.85 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning/tree_", x,"_Diameter_rcp85_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp8.5"
df})
dbh.prop.85 <- do.call(rbind, dbh.prop.list.85)

dbh.prop.all <- rbind(dbh.prop, dbh.prop.45, dbh.prop.60, dbh.prop.85)

saveRDS(dbh.prop.all, paste0("dbh.prop.all",output.base.name,".RDS"))
#dbh.prop <- read.csv("variance_partitioning/tree_100_Diameter_rcp26_proportion.csv")


dbh.prop.wide <- dbh.prop.all %>% select(-X,) %>% group_by(x,year, rcp) %>% spread(simtype, variance)
dbh.prop.wide$InitialCondition <-  dbh.prop.wide$`Initial Conditions`
dbh.prop.wide$ProcessError <-  dbh.prop.wide$`Process Error` - dbh.prop.wide$`Driver Uncertainty`
dbh.prop.wide$ParameterUnc <-  dbh.prop.wide$`Parameter Uncertainty` - dbh.prop.wide$`Initial Conditions`
dbh.prop.wide$PlotrandUnc <-  dbh.prop.wide$`Plot random effect` - dbh.prop.wide$`Parameter Uncertainty`
dbh.prop.wide$DriverUnc <-  dbh.prop.wide$`Driver Uncertainty` - dbh.prop.wide$`Plot random effect`

summary(dbh.prop.wide)

dbh.prop.wide$period <- ifelse(dbh.prop.wide$year >= 2075, "2075 - 2099", 
                               ifelse(dbh.prop.wide$year >= 2050 & dbh.prop.wide$year < 2075, "2050 - 2074", 
                                      ifelse(dbh.prop.wide$year < 2050, "2019 - 2049", NA)))

dbh.prop.long <- dbh.prop.wide %>% group_by(period, year, treeid, x, rcp) %>% select(-`Driver Uncertainty` , -`Initial Conditions`, -`Parameter Uncertainty`,
                                                                                     -`Plot random effect`, -`Process Error`) %>%
  gather(Uncertainty, value ,-x, -year, -treeid, -period, -rcp)
#dbh.prop.wide <- reshape2::melt(dbh.prop.wide)
rcp.dbh.prop <- dbh.prop.long %>% group_by(period, Uncertainty, rcp) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                                                   ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                   ci.hi = quantile(value, 0.975, na.rm = TRUE)) 
rcp.dbh.prop$Uncertainty <- factor(rcp.dbh.prop$Uncertainty , levels = c("ProcessError", 
                                                                         "DriverUnc", 
                                                                         "PlotrandUnc", 
                                                                         
                                                                         "ParameterUnc", 
                                                                         "InitialCondition"))

Uncertainty.table <- data.frame(Uncertainty = c("ProcessError", 
                                                "DriverUnc", 
                                                "PlotrandUnc", 
                                                "ParameterUnc", 
                                                "InitialCondition"),
                                Unc = c("Process", 
                                        "Driver", 
                                        "Random Effects", 
                                        "Parameter", 
                                        "Initial Conditions"))


rcp.dbh.proportion.summary <- merge(rcp.dbh.prop, Uncertainty.table, by = "Uncertainty")

rcp.dbh.proportion.summary$Unc <- factor(rcp.dbh.proportion.summary$Unc , levels = c("Process", 
                                                                                     "Driver", 
                                                                                     "Random Effects", 
                                                                                     "Parameter", 
                                                                                     "Initial Conditions"))
#ggplot( dbh.prop.long, aes(x = rcp , y = value, fill = Uncertainty))+geom_boxplot()+facet_wrap(~period)
Diameter.unc.summary.prop <- ggplot(rcp.dbh.proportion.summary, aes(x = rcp , y = mean, fill = Unc))+geom_bar(stat = "identity")+facet_wrap(~period)+
  ylab("Proportion of Uncertainty\n in Diameter")+xlab("RCP Scenario")+scale_fill_manual(values = my_cols, name = NULL)+
  theme_bw(base_size = 12)+theme(axis.text.x = element_text(hjust = 1, angle = 45), panel.grid = element_blank())


png(height = 4, width = 6, units = "in", res = 300, paste0(output.base.name, "_Proportion_Diameter_unc_proportion_all_rcp_all_time.png"))
Diameter.unc.summary.prop
dev.off()

png(height = 6, width = 7, units = "in", res = 300, paste0("both_uncertainty_prop_summary_",output.base.name,".png"))
#cowplot::plot_grid(
cowplot::plot_grid(increment.unc.summary.prop, Diameter.unc.summary.prop, ncol = 1, align = "hv", labels = "AUTO")#, #Uncertainty.legend,ncol = 2, rel_widths = c(1, 0.05))
dev.off()

#--------------------------------------------------------------------------------------
# Plot summaries of the total increment uncertainty summarised across all the trees
#--------------------------------------------------------------------------------------
inc.tot.list <- list()
treeds <- c(1:515)
inc.tot.list.2.6 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning/tree_", x,"_Increment_rcp26_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp2.6"
df})
inc.tot <- do.call(rbind, inc.tot.list.2.6)


inc.tot.list.45 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning/tree_", x,"_Increment_rcp45_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp4.5"
df})
inc.tot.45 <- do.call(rbind, inc.tot.list.45)

inc.tot.list.60 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning/tree_", x,"_Increment_rcp60_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp6.0"
df})
inc.tot.60 <- do.call(rbind, inc.tot.list.60)



inc.tot.list.85 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning/tree_", x,"_Increment_rcp85_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp8.5"
df})
inc.tot.85 <- do.call(rbind, inc.tot.list.85)

inc.tot.all <- rbind(inc.tot, inc.tot.45, inc.tot.60, inc.tot.85)

saveRDS(inc.tot.all, paste0("INC.TOT.ALL",output.base.name,".RDS"))
#inc.tot <- read.csv("variance_partitioning/tree_100_Increment_rcp26_totortion.csv")
inc.tot.all <- inc.tot.all[complete.cases(inc.tot.all),]
inc.tot.all$diff <- ifelse(is.infinite(abs(inc.tot.all$High - inc.tot.all$Low)), 1000, abs(inc.tot.all$High - inc.tot.all$Low))
inc.tot.less <- inc.tot.all %>% filter(diff <= 5)
hist(inc.tot.less$diff )


inc.tot.wide <- inc.tot.less %>% select( -Low, -High, -X)  %>% group_by(year,treeid, rcp) %>% spread(uncertainty, diff)
inc.tot.wide$InitialCondition <-  inc.tot.wide$I
inc.tot.wide$ProcessError <-  inc.tot.wide$IPP 
inc.tot.wide$ParameterUnc <-  inc.tot.wide$IPA - inc.tot.wide$IP
inc.tot.wide$PlotrandUnc <-  inc.tot.wide$IPA - inc.tot.wide$IP
inc.tot.wide$DriverUnc <-  inc.tot.wide$IPD - inc.tot.wide$IPA

inc.tot.wide$period <- ifelse(inc.tot.wide$year >= 2075, "2075 - 2099", 
                              ifelse(inc.tot.wide$year >= 2050 & inc.tot.wide$year < 2075, "2050 - 2074", 
                                     ifelse(inc.tot.wide$year < 2050, "2019 - 2049", NA)))

inc.tot.long <- inc.tot.wide %>% group_by(period, year, treeid,  rcp) %>% select(-I , -IP, -IPA,
                                                                                 -IPD, -IPP) %>%
  gather(Uncertainty, value , -year, -treeid, -period, -rcp)
#inc.tot.wide <- reshape2::melt(inc.tot.wide)
rcp.inc.tot <- inc.tot.long %>% group_by(period, Uncertainty, rcp) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                                                 ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                 ci.hi = quantile(value, 0.975, na.rm = TRUE)) 
rcp.inc.tot$Uncertainty <- factor(rcp.inc.tot$Uncertainty , levels = c("ProcessError", 
                                                                       "DriverUnc", 
                                                                       "PlotrandUnc", 
                                                                       
                                                                       "ParameterUnc", 
                                                                       "InitialCondition"))

Uncertainty.table <- data.frame(Uncertainty = c("ProcessError", 
                                                "DriverUnc", 
                                                "PlotrandUnc", 
                                                "ParameterUnc", 
                                                "InitialCondition"),
                                Unc = c("Process", 
                                        "Driver", 
                                        "Random Effects", 
                                        "Parameter", 
                                        "Initial Conditions"))


rcp.inc.totortion.summary <- merge(rcp.inc.tot, Uncertainty.table, by = "Uncertainty")

rcp.inc.totortion.summary$Unc <- factor(rcp.inc.totortion.summary$Unc , levels = c("Process", 
                                                                                   "Driver", 
                                                                                   "Random Effects", 
                                                                                   "Parameter", 
                                                                                   "Initial Conditions"))

#ggplot( inc.tot.long, aes(x = rcp , y = value, fill = Uncertainty))+geom_boxplot()+facet_wrap(~period)
increment.unc.summary.tot <- ggplot(rcp.inc.totortion.summary, aes(x = rcp , y = mean, fill = Unc))+geom_bar(stat = "identity")+facet_wrap(~period)+
  ylab("Mean Total Uncertainty\n in Increment")+xlab("RCP Scenario")+scale_fill_manual(values = my_cols, name = NULL)+
  theme_bw(base_size = 12)+theme(axis.text.x = element_text(hjust = 1, angle = 45), panel.grid = element_blank())


png(height = 4, width = 6, units = "in", res = 300, paste0(output.base.name, "_total_increment_unc_totortion_all_rcp_all_time.png"))
increment.unc.summary.tot
dev.off()


#--------------------------------------------------------------------------------------
# Plot summaries of the total Diameter uncertainty summarised across all the trees
#--------------------------------------------------------------------------------------
dbh.tot.list <- list()
treeds <- c(1:515)
dbh.tot.list.2.6 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning/tree_", x,"_Diameter_rcp26_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp2.6"
df})
dbh.tot <- do.call(rbind, dbh.tot.list.2.6)


dbh.tot.list.45 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning/tree_", x,"_Diameter_rcp45_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp4.5"
df})
dbh.tot.45 <- do.call(rbind, dbh.tot.list.45)

dbh.tot.list.60 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning/tree_", x,"_Diameter_rcp60_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp6.0"
df})
dbh.tot.60 <- do.call(rbind, dbh.tot.list.60)



dbh.tot.list.85 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning/tree_", x,"_Diameter_rcp85_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp8.5"
df})
dbh.tot.85 <- do.call(rbind, dbh.tot.list.85)

dbh.tot.all <- rbind(dbh.tot, dbh.tot.45, dbh.tot.60, dbh.tot.85)

saveRDS(dbh.tot.all, paste0("dbh.TOT.ALL.RDS",output.base.name,".RDS"))



dbh.tot.all$diff <- ifelse(is.infinite(abs(dbh.tot.all$High - dbh.tot.all$Low)), 1000, 
                           ifelse(abs(dbh.tot.all$High - dbh.tot.all$Low) >= 1000,1000,abs(dbh.tot.all$High - dbh.tot.all$Low)))
dbh.tot.all$mean <- dbh.tot.all$High- ((dbh.tot.all$High - dbh.tot.all$Low)/2)

dbh.tot.wide <- dbh.tot.all %>% select( -Low, -High, -X, -mean)  %>% group_by(year,treeid, rcp) %>% spread(uncertainty,diff)
dbh.tot.wide$InitialCondition <-  dbh.tot.wide$I
dbh.tot.wide$ParameterUnc <-  dbh.tot.wide$IP - dbh.tot.wide$I
dbh.tot.wide$PlotrandUnc <-  dbh.tot.wide$IPA - dbh.tot.wide$IP
dbh.tot.wide$DriverUnc <-  dbh.tot.wide$IPD - dbh.tot.wide$IPA
dbh.tot.wide$ProcessError <-  dbh.tot.wide$IPP -dbh.tot.wide$IPD

hist(dbh.tot.wide$InitialCondition)
hist(dbh.tot.wide$ParameterUnc)
hist(dbh.tot.wide$PlotrandUnc)
# plot out the average total uncertainty:

total.unc <- dbh.tot.all %>% filter(uncertainty %in% "IPP") %>% group_by(year, rcp) %>% summarise(average.tot.unc = mean(diff, na.rm = TRUE))


ggplot(data = total.unc, aes(x = year, y = average.tot.unc, color = rcp))+geom_line()+
  theme_bw(base_size = 12)+ylab("Average Diameter Forecast Uncertainty")+xlab("Year")+theme(panel.grid = element_blank())






dbh.tot.wide$period <- ifelse(dbh.tot.wide$year >= 2075, "2075 - 2099", 
                              ifelse(dbh.tot.wide$year >= 2050 & dbh.tot.wide$year < 2075, "2050 - 2074", 
                                     ifelse(dbh.tot.wide$year < 2050, "2019 - 2049", NA)))

dbh.tot.long <- dbh.tot.wide %>% group_by(period, year, treeid,  rcp) %>% select(-I , -IP, -IPA,
                                                                                 -IPD, -IPP) %>%
  gather(Uncertainty, value , -year, -treeid, -period, -rcp)
#dbh.tot.wide <- reshape2::melt(dbh.tot.wide)
rcp.dbh.tot <- dbh.tot.long %>% group_by(period, Uncertainty, rcp) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                                                 ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                 ci.hi = quantile(value, 0.975, na.rm = TRUE)) 
rcp.dbh.tot$Uncertainty <- factor(rcp.dbh.tot$Uncertainty , levels = c("ProcessError", 
                                                                       "DriverUnc", 
                                                                       "PlotrandUnc", 
                                                                       
                                                                       "ParameterUnc", 
                                                                       "InitialCondition"))

Uncertainty.table <- data.frame(Uncertainty = c("ProcessError", 
                                                "DriverUnc", 
                                                "PlotrandUnc", 
                                                "ParameterUnc", 
                                                "InitialCondition"),
                                Unc = c("Process", 
                                        "Driver", 
                                        "Random Effects", 
                                        "Parameter", 
                                        "Initial Conditions"))


rcp.dbh.totortion.summary <- merge(rcp.dbh.tot, Uncertainty.table, by = "Uncertainty")

rcp.dbh.totortion.summary$Unc <- factor(rcp.dbh.totortion.summary$Unc , levels = c("Process", 
                                                                                   "Driver", 
                                                                                   "Random Effects", 
                                                                                   "Parameter", 
                                                                                   "Initial Conditions"))





Diameter.unc.summary.tot <- ggplot(rcp.dbh.totortion.summary, aes(x = rcp , y = mean, fill = Unc))+geom_bar(stat = "identity")+facet_wrap(~period)+
  ylab("Mean Total Uncertainty\n in Diameter (cm)")+xlab("RCP Scenario")+scale_fill_manual(values = my_cols, name = NULL)+
  theme_bw(base_size = 12)+theme(axis.text.x = element_text(hjust = 1, angle = 45), panel.grid = element_blank())

Diameter.unc.summary.tot

png(height = 4, width = 6, units = "in", res = 300, paste0(output.base.name, "_total_Diameter_unc_totortion_all_rcp_all_time.png"))
Diameter.unc.summary.tot
dev.off()

png(height = 6, width = 7, units = "in", res = 300, paste0("both_uncertainty_totals_summary_",output.base.name,".png"))
#cowplot::plot_grid(
cowplot::plot_grid(increment.unc.summary.tot, Diameter.unc.summary.tot, ncol = 1, align = "hv", labels = "AUTO")#, #Uncertainty.legend,ncol = 2, rel_widths = c(1, 0.05))
dev.off()



###############################################################################################
###############################################################################################

prop.inc <- plot.prop.variance.future.forecast.driver(m = 400, prop = TRUE, scenario = "rcp26", type = "ringwidth")
total.inc <- plot.prop.variance.future.forecast.driver(m = 400, prop = FALSE, scenario = "rcp26", type = "ringwidth")
prop.dbh <- plot.prop.variance.future.forecast.driver(m = 400, prop = TRUE, scenario = "rcp26", type = "dbh")
total.dbh <- plot.prop.variance.future.forecast.driver(m = 400, prop = FALSE, scenario = "rcp26", type = "dbh")
legend.colors <- cowplot::get_legend(prop.inc)


png(height = 6, width = 10, units = "in", res = 300, "Uncertainty_partition_tree_400.png")
cowplot::plot_grid(
  cowplot::plot_grid(prop.inc+theme(legend.position = "none"), total.inc, 
                     prop.dbh+theme(legend.position = "none"), total.dbh, ncol = 2, align = "hv"),
  legend.colors, ncol = 2, rel_widths = c(1, 0.25))

dev.off()

# -------------------Plot multipaged pdf-------------------------


#---------------------------------------------------------------------------------------
# breaking down the driver uncertainty into temperature and precipitation
#---------------------------------------------------------------------------------------
plot.prop.variance.driver <- function(m, prop =TRUE, scenario = "rcp26", type = "dbh", print =TRUE, Xplot = FALSE){
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
  alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
  
  alpha <- quantile(alphas[, alphaplotid],0.5)
  bSDI <- quantile(betas[,"betaSDI"],0.5)
  bSDI_ppt <- quantile(betas[,"betaSDI_wintP.wateryr"],0.5)
  
  if(Xplot == TRUE){
    BXid <- paste0("betaX_PLOT[", cov.data[m,]$plotid, "]")
    bX <-  quantile(betaXplots[,BXid],0.5)
  }else{
    bX <- quantile(betas[,"betaX"], 0.5)
  }
  bX2 <- quantile(betas[,"betaX2"],0.5)
  
  if("betaX_SDI" %in% colnames(betas) == FALSE ){
    bX_SDI <- 0
  }else{
    bX_SDI <- quantile(betas[,"betaX_SDI"],0.5)
  }
  
  if("betaSICOND" %in% colnames(betas) == FALSE ){
    bSI <- 0
    bSI_X <- 0
    bSI_wintP.wateryr <- 0
    bSI_tmax.fallspr <- 0
  }else{
    bSI <- quantile(betas[,"betaSICOND"],0.5)
    bSI_X <- quantile(betas[,"betaX_SICOND"],0.5)
    bSI_wintP.wateryr <- quantile(betas[,"betaSICOND_wintP.wateryr"],0.5)
    bSI_tmax.fallspr <- quantile(betas[,"betaSICOND_tmax.fallspr"],0.5)
  }
  
  bX_ppt <- quantile(betas[,"betaX_wintP.wateryr"],0.5)
  bppt <- quantile(betas[,"betawintP.wateryr"],0.5)
  btmax <- quantile(betas[,"betatmax.fallspr"] ,0.5)
  bX_tmax <- quantile(betas[,"betaX_tmax.fallspr"],0.5)
  bSDI_tmax <- quantile(betas[,"betaSDI_tmax.fallspr"],0.5)
  btmax_ppt <- quantile(betas[,"betatmax.fallspr_wintP.wateryr"],0.5)
  b0 <- quantile(B0, 0.5)
  
  betas.point <- data.frame(b0, bSDI, bSDI_ppt, bSI, bSI_tmax.fallspr, bSI_wintP.wateryr,
                            bSI_X, bX, bX2, bX_SDI, bX_ppt, 
                            bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  #scenario <- "rcp26"
  # code toe filter out future climate data for the tree id and the rcp:
  # filter out for the means
  future.proj <- ens.means[ens.means$id == m & ens.means$rcp == scenario, ]
  proj.ordered <- future.proj[order(future.proj$year),]
  
  # now filter for the full ensemble
  future.ens <- clim.ts.df.full[clim.ts.df.full$id == m & clim.ts.df.full$rcp == scenario,]
  ens.proj.ordered <-  future.ens[order( future.ens$year),]
  sample.model <- unique(ens.proj.ordered$modelrun)
  
  #---------------------------------------------------------------------------
  ##  breaking down total Driver uncertainty
  #---------------------------------------------------------------------------
  
  
  # Include all driver uncertainty: Both Temperature and Precipitation uncertainty:
  ppt <- tmax <- SDI <-SICOND <- matrix(NA, nrow =length(7501:15300), ncol = 82 )
  #for(i in 1:82){
  
  #   ppt[,i]<- rnorm(n = length(7501:15300) , mean = proj.ordered[i,]$mean.ppt, sd = proj.ordered[i,]$SD.ppt)
  #   tmax[,i]<- rnorm(n = length(7501:15300) , mean = proj.ordered[i,]$mean.tmax.fs, sd = proj.ordered[i,]$SD.tmax)
  #   SDI[,i]<- rep(cov.data[m, ]$SDI, length(7501:15300))
  #   SICOND[,i]<- rep(cov.data[m, ]$SICOND, length(7501:15300))
  #   
  # }
  # 
  get.ens.df <- function(i){
    
    ens.proj.yr <- ens.proj.ordered %>% filter(modelrun %in% sample.model[i])
    ens.proj.yr <- ens.proj.yr [!duplicated(ens.proj.yr),]
    
    # # just sample from the distribution of climate:
    # ppt <- ens.proj.yr$ppt.scale
    # tmax <- ens.proj.yr$tmax.scaled
    # 
    # #ppt[,i]<- rnorm(n = length(7501:15300) , mean = proj.ordered[i,]$mean.ppt, sd = proj.ordered[i,]$SD.ppt)
    # #tmax[,i]<- rnorm(n = length(7501:15300) , mean = proj.ordered[i,]$mean.tmax.fs, sd = proj.ordered[i,]$SD.tmax)
    # SDI<- rep(cov.data[m, ]$SDI, 82)
    # SICOND<- rep(cov.data[m, ]$SICOND, 82)
    df <- data.frame(ppt = ens.proj.yr$ppt.scale, 
                     tmax = ens.proj.yr$tmax.scaled, 
                     SDI =  rep(cov.data[m, ]$SDI, 82), 
                     SICOND =  rep(cov.data[m, ]$SDI, 82), 
                     i = i, 
                     year = ens.proj.yr$year)
    df
  }
  
  ens.samps <- lapply(1:length(sample.model), get.ens.df)
  ens.samps.df <- do.call(rbind, ens.samps)
  
  
  
  
  #covariates <- list(SDI, ppt, tmax)
  time_steps <- 82
  nMCMC <- length(x.mat[,"x[1,53]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = mean(x.mat[,paste0("x[", m,",", 53,"]")]), m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = data.frame(SDI = ens.samps.df %>% filter(year == t + 2017) %>% select(SDI), 
                                                                                                                                                                        ppt = ens.samps.df %>% filter(year == t + 2017) %>% select(ppt), 
                                                                                                                                                                        tmax = ens.samps.df %>% filter(year == t + 2017) %>% select(tmax), 
                                                                                                                                                                        SICOND = ens.samps.df %>% filter(year == t + 2017) %>% select(SICOND)))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-x.mat[,paste0("x[", m,",", 53,"]")]
      
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = data.frame(SDI = ens.samps.df %>% filter(year == t + 2017) %>% select(SDI), 
                                                                                                                                             ppt = ens.samps.df %>% filter(year == t + 2017) %>% select(ppt), 
                                                                                                                                             tmax = ens.samps.df %>% filter(year == t + 2017) %>% select(tmax), 
                                                                                                                                             SICOND = ens.samps.df %>% filter(year == t + 2017) %>% select(SICOND)))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-forecast[,t-1]
      
    }  }
  variancetotal.driver <- apply(forecast,2,var)
  forecast.all.driver <- apply(forecast, 2, function(b){quantile(b, na.rm = TRUE)})
  
  variancetotal.driver.inc <- apply(inc,2,var)
  forecast.all.driver.inc <- apply(inc, 2, function(b){quantile(b, na.rm = TRUE)})
  
  #---------------------------------------------------------------------------
  ##  breaking down Precipitation Driver uncertainty
  #---------------------------------------------------------------------------
  
  # use all of the parameter MCMCS:
  
  # 
  # including precipitation uncertainty, but no temperature uncertainty
  # tmax <- SDI <- matrix(NA, nrow =length(7501:15300), ncol = 82 )
  # for(i in 1:82){
  #   # use the same value for the previous iterations
  #   #ppt[,i]<- rnorm(n = length(7501:15300) , mean = proj.ordered[i,]$mean.ppt, sd = proj.ordered[i,]$SD.ppt)
  #   tmax[,i]<- rep( proj.ordered[i,]$mean.tmax.fs, length(7501:15300))
  #   SDI[,i]<- rep(cov.data[m, ]$SDI, length(7501:15300))
  #   SICOND[,i]<- rep(cov.data[m, ]$SICOND, length(7501:15300))
  # }
  # 
  get.ens.df <- function(i){
    
    ens.proj.yr <- ens.proj.ordered %>% filter(modelrun %in% sample.model[i])
    ens.proj.yr <- ens.proj.yr [!duplicated(ens.proj.yr),]
    
    # # just sample from the distribution of climate:
    # ppt <- ens.proj.yr$ppt.scale
    # tmax <- ens.proj.yr$tmax.scaled
    # 
    # #ppt[,i]<- rnorm(n = length(7501:15300) , mean = proj.ordered[i,]$mean.ppt, sd = proj.ordered[i,]$SD.ppt)
    # #tmax[,i]<- rnorm(n = length(7501:15300) , mean = proj.ordered[i,]$mean.tmax.fs, sd = proj.ordered[i,]$SD.tmax)
    # SDI<- rep(cov.data[m, ]$SDI, 82)
    # SICOND<- rep(cov.data[m, ]$SICOND, 82)
    df <- data.frame(ppt = ens.proj.yr$ppt.scale,  
                     tmax = proj.ordered$mean.tmax.fs,
                     SDI =  rep(cov.data[m, ]$SDI, 82), 
                     SICOND =  rep(cov.data[m, ]$SDI, 82), 
                     i = i, 
                     year = ens.proj.yr$year)
    df
  }
  
  ens.samps <- lapply(1:length(sample.model), get.ens.df)
  ens.samps.df <- do.call(rbind, ens.samps)
  
  #covariates <- list()
  # covariates$SDI <- SDI
  # covariates$ppt <- ppt
  # covariates$tmax <- tmax
  # covariates$SICOND <- SICOND
  
  #covariates <- list(SDI, ppt, tmax)
  
  
  time_steps <- 82
  nMCMC <- length(x.mat[,"x[1,53]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  inc <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = mean(x.mat[,paste0("x[", m,",", 53,"]")]), m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates =  data.frame(SDI = ens.samps.df %>% filter(year == t + 2017) %>% select(SDI), 
                                                                                                                                                                         ppt = ens.samps.df %>% filter(year == t + 2017) %>% select(ppt), 
                                                                                                                                                                         tmax = ens.samps.df %>% filter(year == t + 2017) %>% select(tmax), 
                                                                                                                                                                         SICOND = ens.samps.df %>% filter(year == t + 2017) %>% select(SICOND)))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-x.mat[,paste0("x[", m,",", 53,"]")]
      
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates =  data.frame(SDI = ens.samps.df %>% filter(year == t + 2017) %>% select(SDI), 
                                                                                                                                              ppt = ens.samps.df %>% filter(year == t + 2017) %>% select(ppt), 
                                                                                                                                              tmax = ens.samps.df %>% filter(year == t + 2017) %>% select(tmax), 
                                                                                                                                              SICOND = ens.samps.df %>% filter(year == t + 2017) %>% select(SICOND)))
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t]-forecast[,t-1]
      
    }  }
  variance.ppt.driver <- apply(forecast,2,var)
  forecast.ppt.driver <- apply(forecast, 2, function(b){quantile(b, na.rm = TRUE)})
  
  
  variance.ppt.driver.inc <- apply(inc,2,var)
  forecast.ppt.driver.inc <- apply(inc, 2, function(b){quantile(b, na.rm = TRUE)})
  
  
  # combine variances:
  if(type == "dbh"){
    V.pred.sim     <- rbind(variance.ppt.driver, variancetotal.driver)
    V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
    
    pred.sims     <- data.frame(Precipitation.0 = forecast.ppt.driver[1,],
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
  
  
  write.csv(variance.df, paste0("variance_partitioning_driver/tree_", m,"_", axis.name, "_", scenario,"_",output.base.name,"_proportion", ".csv"))
  write.csv(pred.sims.class, paste0("variance_partitioning_driver/tree_", m,"_", axis.name, "_", scenario,"_",output.base.name,"_totalunc", ".csv"))
  
  if(print == TRUE){
    if(prop == TRUE){
      prop.var
      
    }else{
      predY_plot
    }
  }else{
    cat(m)
  }
}

plot.prop.variance.driver(m = 222, prop = TRUE, scenario = "rcp26", type = "ringwidth", print = TRUE, Xplot = FALSE)
plot.prop.variance.driver(m = 1, prop = TRUE, scenario = "rcp26", type = "ringwidth", print = TRUE, Xplot = FALSE)

# run this and save outputs for all the trees

treeds <- c(1:515)
# proportions for rcp 2.6
for(k in treeds){
  plot.prop.variance.driver(m = k, prop = TRUE, scenario = "rcp26", type = "ringwidth", print = FALSE, Xplot = FALSE)
}

for(k in treeds){
  plot.prop.variance.driver(m = k, prop = TRUE, scenario = "rcp26", type = "dbh", print = FALSE, Xplot = FALSE)
}

# rcp 4.5
for(k in treeds){
  plot.prop.variance.driver(m = k, prop = TRUE, scenario = "rcp45", type = "ringwidth", print = FALSE, Xplot = FALSE)
}

for(k in treeds){
  plot.prop.variance.driver(m = k, prop = TRUE, scenario = "rcp45", type = "dbh", print = FALSE, Xplot = FALSE)
}


# rcp 8.5
for(k in treeds){
  plot.prop.variance.driver(m = k, prop = TRUE, scenario = "rcp85", type = "ringwidth", print = FALSE, Xplot = FALSE)
}

for(k in treeds){
  plot.prop.variance.driver(m = k, prop = TRUE, scenario = "rcp85", type = "dbh", print = FALSE, Xplot = FALSE)
}

# rcp 6.0
for(k in treeds){
  plot.prop.variance.driver(m = k, prop = TRUE, scenario = "rcp60", type = "ringwidth", print = FALSE, Xplot = FALSE)
}

for(k in treeds){
  plot.prop.variance.driver(m = k, prop = TRUE, scenario = "rcp60", type = "dbh", print = FALSE, Xplot = FALSE)
}

#---------------------------------------------------------------------------------------
# read all of these values in and make an average plot of them
#------------------------------------------------------------------------------------------
#  Summarise the tree ring increment proportion of uncertainty
#------------------------------------------------------------------------------------------
inc.prop.list <- list()
treeds <- c(1:515)
df.2 <- list()
for(xy in treeds){
  df <- read.csv(paste0("variance_partitioning_driver/tree_", xy,"_Increment_rcp45_", output.base.name, "_proportion.csv"))
  df$treeid <- xy
  df$rcp <- "rcp4.5"
  df.2[[xy]]<- df
}
inc.prop.list.2.6 <- lapply(treeds, function(xy){df <- read.csv(paste0("variance_partitioning_driver/tree_", xy,"_Increment_rcp26_", output.base.name, "_proportion.csv"))
df$treeid <- xy
df$rcp <- "rcp2.6"
df})
inc.prop <- do.call(rbind, inc.prop.list.2.6)


inc.prop.list.45 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_driver/tree_", x,"_Increment_rcp45_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp4.5"
df})
inc.prop.45 <- do.call(rbind, inc.prop.list.45)

inc.prop.list.60 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_driver/tree_", x,"_Increment_rcp60_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp6.0"
df})
inc.prop.60 <- do.call(rbind, inc.prop.list.60)



inc.prop.list.85 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_driver/tree_", x,"_Increment_rcp85_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp8.5"
df})
inc.prop.85 <- do.call(rbind, inc.prop.list.85)

inc.prop.all <- rbind(inc.prop, inc.prop.45, inc.prop.60, inc.prop.85)
saveRDS(inc.prop.all, paste0("inc.prop.driver_",output.base.name, ".RDS"))
#inc.prop <- read.csv("variance_partitioning_driver/tree_100_Increment_rcp26_proportion.csv")


inc.prop.wide <- inc.prop.all %>% select(-X,) %>% group_by(x,year, rcp) %>% spread(simtype, variance)
inc.prop.wide$TemperatureUncertainty <- inc.prop.wide$`Temperature Uncertainty` - inc.prop.wide$`Precipitation Uncertainty`

inc.prop.wide$period <- ifelse(inc.prop.wide$year >= 2075, "2075 - 2099", 
                               ifelse(inc.prop.wide$year >= 2050 & inc.prop.wide$year < 2075, "2050 - 2074", 
                                      ifelse(inc.prop.wide$year < 2050, "2019 - 2049", NA)))

inc.prop.long <- inc.prop.wide %>% group_by(period, year, treeid, x, rcp) %>% select(-`Temperature Uncertainty` ) %>%
  gather(Uncertainty, value ,-x, -year, -treeid, -period, -rcp)
#inc.prop.wide <- reshape2::melt(inc.prop.wide)
rcp.inc.prop <- inc.prop.long %>% group_by(period, Uncertainty, rcp) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                                                   ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                   ci.hi = quantile(value, 0.975, na.rm = TRUE)) 
rcp.inc.prop$Uncertainty <- factor(rcp.inc.prop$Uncertainty , levels = c("TemperatureUncertainty", "Precipitation Uncertainty"))

Uncertainty.table <- data.frame(Uncertainty = c("TemperatureUncertainty", 
                                                "Precipitation Uncertainty"),
                                Unc = c("Temperature Uncertainty", 
                                        "Precipitation Uncertainty"))


rcp.inc.proportion.summary <- merge(rcp.inc.prop, Uncertainty.table, by = "Uncertainty")

rcp.inc.proportion.summary$Unc <- factor(rcp.inc.proportion.summary$Unc , levels = c("Temperature Uncertainty", "Precipitation Uncertainty"))
#ggplot( inc.prop.long, aes(x = rcp , y = value, fill = Uncertainty))+geom_boxplot()+facet_wrap(~period)
Increment.unc.summary.prop <- ggplot(rcp.inc.proportion.summary, aes(x = rcp , y = mean, fill = Unc))+geom_bar(stat = "identity")+facet_wrap(~period)+
  ylab("Proportion of Uncertainty\n in Increment")+xlab("RCP Scenario")+scale_fill_manual(values = c("#b2182b", "#2166ac"), name = NULL)+
  theme_bw(base_size = 12)+theme(axis.text.x = element_text(hjust = 1, angle = 45), panel.grid = element_blank())
Increment.unc.summary.prop 

png(height = 4, width = 6, units = "in", res = 300, paste0(output.base.name, "_Proportion_Increment_unc_proportion_driver_unc_rcp_all_time.png"))
Increment.unc.summary.prop
dev.off()



#------------------------------------------------------------------------------------------
#  Summarise the tree ring Diameter proportion of driver uncertainty
#------------------------------------------------------------------------------------------
dbh.prop.list <- list()
treeds <- c(1:515)
dbh.prop.list.2.6 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_driver/tree_", x,"_Diameter_rcp26_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp2.6"
df})
dbh.prop <- do.call(rbind, dbh.prop.list.2.6)


dbh.prop.list.45 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_driver/tree_", x,"_Diameter_rcp45_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp4.5"
df})
dbh.prop.45 <- do.call(rbind, dbh.prop.list.45)

dbh.prop.list.60 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_driver/tree_", x,"_Diameter_rcp60_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp6.0"
df})
dbh.prop.60 <- do.call(rbind, dbh.prop.list.60)



dbh.prop.list.85 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_driver/tree_", x,"_Diameter_rcp85_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp8.5"
df})
dbh.prop.85 <- do.call(rbind, dbh.prop.list.85)

dbh.prop.all <- rbind(dbh.prop, dbh.prop.45, dbh.prop.60, dbh.prop.85)

saveRDS(dbh.prop.all, paste0("dbh.prop.driver",output.base.name,".RDS"))
#dbh.prop <- read.csv("variance_partitioning_driver/tree_100_Diameter_rcp26_proportion.csv")


dbh.prop.wide <- dbh.prop.all %>% select(-X,) %>% group_by(x,year, rcp) %>% spread(simtype, variance)
dbh.prop.wide$TemperatureUncertainty <- dbh.prop.wide$`Temperature Uncertainty` - dbh.prop.wide$`Precipitation Uncertainty`
summary(dbh.prop.wide)

dbh.prop.wide$period <- ifelse(dbh.prop.wide$year >= 2075, "2075 - 2099", 
                               ifelse(dbh.prop.wide$year >= 2050 & dbh.prop.wide$year < 2075, "2050 - 2074", 
                                      ifelse(dbh.prop.wide$year < 2050, "2019 - 2049", NA)))

dbh.prop.long <- dbh.prop.wide %>% group_by(period, year, treeid, x, rcp) %>% select(-`Temperature Uncertainty` ) %>%
  gather(Uncertainty, value ,-x, -year, -treeid, -period, -rcp)
#dbh.prop.wide <- reshape2::melt(dbh.prop.wide)
rcp.dbh.prop <- dbh.prop.long %>% group_by(period, Uncertainty, rcp) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                                                   ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                   ci.hi = quantile(value, 0.975, na.rm = TRUE)) 
rcp.dbh.prop$Uncertainty <- factor(rcp.dbh.prop$Uncertainty , levels = c("TemperatureUncertainty", "Precipitation Uncertainty"))

Uncertainty.table <- data.frame(Uncertainty = c("TemperatureUncertainty", 
                                                "Precipitation Uncertainty"),
                                Unc = c("Temperature Uncertainty", 
                                        "Precipitation Uncertainty"))


rcp.dbh.proportion.summary <- merge(rcp.dbh.prop, Uncertainty.table, by = "Uncertainty")

rcp.dbh.proportion.summary$Unc <- factor(rcp.dbh.proportion.summary$Unc , levels = c("Temperature Uncertainty", "Precipitation Uncertainty"))
#ggplot( dbh.prop.long, aes(x = rcp , y = value, fill = Uncertainty))+geom_boxplot()+facet_wrap(~period)
Diameter.unc.summary.prop <- ggplot(rcp.dbh.proportion.summary, aes(x = rcp , y = mean, fill = Unc))+geom_bar(stat = "identity")+facet_wrap(~period)+
  ylab("Proportion of Uncertainty\n in Diameter")+xlab("RCP Scenario")+scale_fill_manual(values = c("#b2182b", "#2166ac"), name = NULL)+
  theme_bw(base_size = 12)+theme(axis.text.x = element_text(hjust = 1, angle = 45), panel.grid = element_blank())

Diameter.unc.summary.prop
png(height = 4, width = 6, units = "in", res = 300, paste0(output.base.name, "_Proportion_Diameter_unc_proportion_driver_unc_rcp_all_time.png"))
Diameter.unc.summary.prop
dev.off()

png(height = 6, width = 7, units = "in", res = 300, paste0("both_uncertainty_prop_summary_driver",output.base.name,".png"))
#cowplot::plot_grid(
cowplot::plot_grid(Increment.unc.summary.prop, Diameter.unc.summary.prop, ncol = 1, align = "hv", labels = "AUTO")#, #Uncertainty.legend,ncol = 2, rel_widths = c(1, 0.05))
dev.off()

#--------------------------------------------------------------------------------------
# Plot summaries of the total increment uncertainty summarised across all the trees
#--------------------------------------------------------------------------------------
inc.tot.list <- list()
treeds <- c(1:515)
inc.tot.list.2.6 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_driver/tree_", x,"_Increment_rcp26_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp2.6"
df})
inc.tot <- do.call(rbind, inc.tot.list.2.6)


inc.tot.list.45 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_driver/tree_", x,"_Increment_rcp45_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp4.5"
df})
inc.tot.45 <- do.call(rbind, inc.tot.list.45)

inc.tot.list.60 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_driver/tree_", x,"_Increment_rcp60_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp6.0"
df})
inc.tot.60 <- do.call(rbind, inc.tot.list.60)



inc.tot.list.85 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_driver/tree_", x,"_Increment_rcp85_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp8.5"
df})
inc.tot.85 <- do.call(rbind, inc.tot.list.85)

inc.tot.all <- rbind(inc.tot, inc.tot.45, inc.tot.60, inc.tot.85)

saveRDS(inc.tot.all, paste0("INC.TOT.driver",output.base.name,".RDS"))
#inc.tot <- read.csv("variance_partitioning_driver/tree_100_Increment_rcp26_totortion.csv")
inc.tot.all <- inc.tot.all[complete.cases(inc.tot.all),]
inc.tot.all$diff <- ifelse(is.infinite(abs(inc.tot.all$High - inc.tot.all$Low)), 1000, abs(inc.tot.all$High - inc.tot.all$Low))
inc.tot.less <- inc.tot.all %>% filter(diff <= 5)
hist(inc.tot.less$diff )


inc.tot.wide <- inc.tot.less %>% select( -Low, -High, -X)  %>% group_by(year,treeid, rcp) %>% spread(uncertainty, diff)
inc.tot.wide$`Precipitation Uncertainty` <-  inc.tot.wide$Precipitation
inc.tot.wide$`Temperature Uncertainty` <-  inc.tot.wide$Temperature - inc.tot.wide$Precipitation

inc.tot.wide$period <- ifelse(inc.tot.wide$year >= 2075, "2075 - 2099", 
                              ifelse(inc.tot.wide$year >= 2050 & inc.tot.wide$year < 2075, "2050 - 2074", 
                                     ifelse(inc.tot.wide$year < 2050, "2019 - 2049", NA)))

inc.tot.long <- inc.tot.wide %>% group_by(period, year, treeid,  rcp) %>% select(-Precipitation, -Temperature) %>%
  gather(Uncertainty, value , -year, -treeid, -period, -rcp)
#inc.tot.wide <- reshape2::melt(inc.tot.wide)
rcp.inc.tot <- inc.tot.long %>% group_by(period, Uncertainty, rcp) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                                                 ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                 ci.hi = quantile(value, 0.975, na.rm = TRUE)) 
rcp.inc.tot$Uncertainty <- factor(rcp.inc.tot$Uncertainty , levels = c("Temperature Uncertainty","Precipitation Uncertainty" 
))



#ggplot( inc.tot.long, aes(x = rcp , y = value, fill = Uncertainty))+geom_boxplot()+facet_wrap(~period)
increment.unc.summary.tot <- ggplot(rcp.inc.tot, aes(x = rcp , y = mean, fill = Uncertainty))+geom_bar(stat = "identity")+facet_wrap(~period)+
  ylab("Mean Total Uncertainty\n in Increment")+xlab("RCP Scenario")+scale_fill_manual(values = c("#b2182b","#2166ac"), name = NULL)+
  theme_bw(base_size = 12)+theme(axis.text.x = element_text(hjust = 1, angle = 45), panel.grid = element_blank())


png(height = 4, width = 6, units = "in", res = 300, paste0(output.base.name, "_total_increment_unc_total_driver_rcp_all_time.png"))
increment.unc.summary.tot
dev.off()


#--------------------------------------------------------------------------------------
# Plot summaries of the total Diameter uncertainty summarised across all the trees
#--------------------------------------------------------------------------------------
dbh.tot.list <- list()
treeds <- c(1:515)
dbh.tot.list.2.6 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_driver/tree_", x,"_Diameter_rcp26_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp2.6"
df})
dbh.tot <- do.call(rbind, dbh.tot.list.2.6)


dbh.tot.list.45 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_driver/tree_", x,"_Diameter_rcp45_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp4.5"
df})
dbh.tot.45 <- do.call(rbind, dbh.tot.list.45)

dbh.tot.list.60 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_driver/tree_", x,"_Diameter_rcp60_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp6.0"
df})
dbh.tot.60 <- do.call(rbind, dbh.tot.list.60)



dbh.tot.list.85 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_driver/tree_", x,"_Diameter_rcp85_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp8.5"
df})
dbh.tot.85 <- do.call(rbind, dbh.tot.list.85)

dbh.tot.all <- rbind(dbh.tot, dbh.tot.45, dbh.tot.60, dbh.tot.85)

saveRDS(dbh.tot.all, paste0("dbh.TOT.driver",output.base.name,".RDS"))
#dbh.tot <- read.csv("variance_partitioning_driver/tree_100_Diameter_rcp26_totortion.csv")
#dbh.tot.all <- dbh.tot.all[complete.cases(dbh.tot.all),]
dbh.tot.all$diff <- ifelse(is.infinite(abs(dbh.tot.all$High - dbh.tot.all$Low)), 1000,
                           ifelse(abs(dbh.tot.all$High - dbh.tot.all$Low) >= 500, 500, abs(dbh.tot.all$High - dbh.tot.all$Low)))
#dbh.tot.less <- dbh.tot.all %>% filter(diff <= 500)
hist( dbh.tot.all$diff )


dbh.tot.wide <- dbh.tot.all %>% select( -Low, -High, -X)  %>% group_by(year,treeid, rcp) %>% spread(uncertainty, diff)
dbh.tot.wide$`Precipitation Uncertainty` <-  dbh.tot.wide$Precipitation
dbh.tot.wide$`Temperature Uncertainty` <-  dbh.tot.wide$Temperature - dbh.tot.wide$Precipitation

dbh.tot.wide$period <- ifelse(dbh.tot.wide$year >= 2075, "2075 - 2099", 
                              ifelse(dbh.tot.wide$year >= 2050 & dbh.tot.wide$year < 2075, "2050 - 2074", 
                                     ifelse(dbh.tot.wide$year < 2050, "2019 - 2049", NA)))

dbh.tot.long <- dbh.tot.wide %>% group_by(period, year, treeid,  rcp) %>% select(-Precipitation, -Temperature) %>%
  gather(Uncertainty, value , -year, -treeid, -period, -rcp)
#dbh.tot.wide <- reshape2::melt(dbh.tot.wide)
rcp.dbh.tot <- dbh.tot.long %>% group_by(period, Uncertainty, rcp) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                                                 ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                 ci.hi = quantile(value, 0.975, na.rm = TRUE)) 
rcp.dbh.tot$Uncertainty <- factor(rcp.dbh.tot$Uncertainty , levels = c("Temperature Uncertainty","Precipitation Uncertainty" 
))



#ggplot( dbh.tot.long, aes(x = rcp , y = value, fill = Uncertainty))+geom_boxplot()+facet_wrap(~period)
Diameter.unc.summary.tot <- ggplot(rcp.dbh.tot, aes(x = rcp , y = mean, fill = Uncertainty))+geom_bar(stat = "identity")+facet_wrap(~period)+
  ylab("Mean Total Uncertainty\n in Diameter")+xlab("RCP Scenario")+scale_fill_manual(values = c("#b2182b","#2166ac"), name = NULL)+
  theme_bw(base_size = 12)+theme(axis.text.x = element_text(hjust = 1, angle = 45), panel.grid = element_blank())


png(height = 4, width = 6, units = "in", res = 300, paste0(output.base.name, "_total_Diameter_unc_total_driver_rcp_all_time.png"))
Diameter.unc.summary.tot
dev.off()



png(height = 6, width = 7, units = "in", res = 300, paste0("both_uncertainty_totals_summary_",output.base.name,".png"))
#cowplot::plot_grid(
cowplot::plot_grid(increment.unc.summary.tot, Diameter.unc.summary.tot, ncol = 1, align = "hv", labels = "AUTO")#, #Uncertainty.legend,ncol = 2, rel_widths = c(1, 0.05))
dev.off()


#-------------------------------------------------------------------------------------------
tree.n <- 515
d.26 <- plot.prop.variance.driver(m = tree.n, prop =TRUE,  scenario = "rcp26", type = "ring width", print =TRUE, Xplot = FALSE)#+ggtitle(paste0("Contribution to  Driver Uncertainty in DBH for Tree",i))
r.26 <- plot.prop.variance.driver(m = tree.n,  prop =TRUE,scenario = "rcp26", type = "dbh", print =TRUE, Xplot = FALSE)#+ggtitle(paste0("Contribution to  Driver Uncertainty in DBH for Tree",i))


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
system.time(multiplot.list.param <- lapply(1:10, multiplot.driver.unc))

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
# breaking down the parameter uncertainty into components
#---------------------------------------------------------------------------------------
plot.prop.parameter.variance <- function(m, prop =TRUE, scenario = "rcp26", type = "dbh",print = FALSE, Xplot = TRUE){
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
  #x <- x.mat[,"x[1,53]"]
  covariates <- data.frame(SDI, ppt, tmax)
  
  time_steps <- 82
  nMCMC <- length(x.mat[,"x[1,53]"])
  forecast <- matrix(data = NA, nrow = nMCMC, ncol = time_steps)
  
  #---------------------------------------------------------------------------
  # Uncertainty from betaSDI
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  
  alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid, "]")
  
  alpha <- rep(quantile(alphas[, alphaplotid],0.5), length(7501:15300))
  bSDI <- rep(quantile(betas[,"betaSDI"],0.5), length(7501:15300))
  bSDI_ppt <- rep(quantile(betas[,"betaSDI_wintP.wateryr"],0.5), length(7501:15300))
  
  if(Xplot == TRUE){
    BXid <- paste0("betaX_PLOT[", cov.data[m,]$plotid, "]")
    bX <-  rep(quantile(betaXplots[,BXid],0.5), length(7501:15300))
  }else{
    bX <- rep(quantile(betas[,"betaX"], 0.5), length(7501:15300))
  }
  bX2 <- rep(quantile(betas[,"betaX2"],0.5), length(7501:15300))
  
  if("betaX_SDI" %in% colnames(betas) == FALSE ){
    bX_SDI <- rep(0, length(7501:15300))
  }else{
    bX_SDI <- rep(quantile(betas[,"betaX_SDI"],0.5), length(7501:15300))
  }
  
  if("betaSICOND" %in% colnames(betas) == FALSE ){
    bSI <- rep(0, length(7501:15300))
    bSI_X <- rep(0, length(7501:15300))
    bSI_wintP.wateryr <- rep(0, length(7501:15300))
    bSI_tmax.fallspr <- rep(0, length(7501:15300))
  }else{
    bSI <- rep(quantile(betas[,"betaSICOND"],0.5), length(7501:15300))
    bSI_X <- rep(quantile(betas[,"betaX_SICOND"],0.5), length(7501:15300))
    bSI_wintP.wateryr <- rep(quantile(betas[,"betaSICOND_wintP.wateryr"],0.5), length(7501:15300))
    bSI_tmax.fallspr <- rep(quantile(betas[,"betaSICOND_tmax.fallspr"],0.5), length(7501:15300))
  }
  
  bX_ppt <- rep(quantile(betas[,"betaX_wintP.wateryr"],0.5), length(7501:15300))
  bppt <- rep(quantile(betas[,"betawintP.wateryr"],0.5), length(7501:15300))
  btmax <- rep(quantile(betas[,"betatmax.fallspr"] ,0.5), length(7501:15300))
  bX_tmax <- rep(quantile(betas[,"betaX_tmax.fallspr"],0.5), length(7501:15300))
  bSDI_tmax <- rep(quantile(betas[,"betaSDI_tmax.fallspr"],0.5), length(7501:15300))
  btmax_ppt <- rep(quantile(betas[,"betatmax.fallspr_wintP.wateryr"],0.5), length(7501:15300))
  b0 <- rep(quantile(B0, 0.5), length(7501:15300))
  
  
  
  bSDI <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betaSDI"]), sd = sd(betas[7501:15300,"betaSDI"]))
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bSI, bSI_tmax.fallspr, bSI_wintP.wateryr,
                          bSI_X, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  
  ppt <- proj.ordered$mean.ppt
  tmax <- proj.ordered$mean.tmax.fs
  #ppt <- time_data$wintP.wateryr[m,]
  #tmax <- time_data$tmax.fallspr[m,]
  SICOND <- rep(cov.data[m, ]$SICOND, length(ppt))
  SDI <- rep(cov.data[m, ]$SDI, length(ppt))
  #x <- x.mat[,"x[1,53]"]
  covariates <- data.frame(SDI, ppt, tmax, SICOND)
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = mean(x.mat[,paste0("x[", m,",", 53,"]")]), m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- dbh.pred - mean(x.mat[,paste0("x[", m,",", 53,"]")])
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t]-forecast[,t-1]
    }  }
  
  varianceSDI <- apply(forecast,2,var)
  forecast.SDI <- apply(forecast, 2, function(x){quantile(x, na.rm=TRUE)})
  
  varianceSDI.inc <- apply(inc,2,var)
  forecast.SDI.inc <- apply(inc, 2, function(x){quantile(x, na.rm=TRUE)})
  
  #---------------------------------------------------------------------------
  # Uncertainty from betaX and X2
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  if(Xplot == TRUE){
    BXid <- paste0("betaX_PLOT[", cov.data[m,]$plotid, "]")
    
    bX <-  rnorm(length(7501:15300), mean = betaXplots[7501:15300,BXid], sd = sd(betaXplots[7501:15300,BXid]))
    
  }else{
    bX <-  rnorm(length(7501:15300), mean = betas[7501:15300,"betaX"], sd = sd(betas[7501:15300,"betaX"]))
  }
  bX2 <- rnorm(length(7501:15300), mean =betas[7501:15300,"betaX2"], sd = sd(betas[7501:15300,"betaX2"]))
  #bSDI <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betaSDI"]), sd = sd(betas[7501:15300,"betaSDI"]))
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bSI, bSI_tmax.fallspr, bSI_wintP.wateryr,
                          bSI_X, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  
  # ppt <- proj.ordered$mean.ppt
  # tmax <- proj.ordered$mean.tmax.fs
  # #ppt <- time_data$wintP.wateryr[m,]
  # #tmax <- time_data$tmax.fallspr[m,]
  # SDI <- rep(cov.data[m, ]$SDI, length(ppt))
  # #x <- x.mat[,"x[1,53]"]
  # covariates <- data.frame(SDI, ppt, tmax)
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = mean(x.mat[,paste0("x[", m,",", 53,"]")]), m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t]<- forecast[,t] - mean(x.mat[,paste0("x[", m,",", 53,"]")])
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t]<-forecast[,t] - forecast[,t-1] 
      
    }  }
  
  varianceSDI.X <- apply(forecast,2,var)
  forecast.SDI.X <- apply(forecast, 2, function(x){quantile(x, na.rm=TRUE)})
  
  varianceSDI.X.inc <- apply(inc,2,var)
  forecast.SDI.X.inc <- apply(inc, 2, function(x){quantile(x, na.rm=TRUE)})
  #---------------------------------------------------------------------------
  # Uncertainty from betaPrecip 
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  
  bppt <-  rnorm(length(7501:15300), mean = betas[7501:15300,"betawintP.wateryr"], sd = sd(betas[7501:15300,"betawintP.wateryr"]))
  betas.all <-data.frame(b0, bSDI, bSDI_ppt, bSI, bSI_tmax.fallspr, bSI_wintP.wateryr,
                         bSI_X, bX, bX2, bX_SDI, bX_ppt, 
                         bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  # ppt <- proj.ordered$mean.ppt
  # tmax <- proj.ordered$mean.tmax.fs
  # #ppt <- time_data$wintP.wateryr[m,]
  # #tmax <- time_data$tmax.fallspr[m,]
  # SDI <- rep(cov.data[m, ]$SDI, length(ppt))
  # #x <- x.mat[,"x[1,53]"]
  # covariates <- data.frame(SDI, ppt, tmax)
  # 
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = mean(x.mat[,paste0("x[", m,",", 53,"]")]), m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - mean(x.mat[,paste0("x[", m,",", 53,"]")])
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - forecast[,t-1]
    }  }
  
  varianceSDI.X.precip <- apply(forecast,2,var)
  forecast.SDI.X.precip <- apply(forecast, 2, function(x){quantile(x, na.rm=TRUE)})
  
  varianceSDI.X.precip.inc <- apply(inc,2,var)
  forecast.SDI.X.precip.inc <- apply(inc, 2, function(x){quantile(x, na.rm=TRUE)})
  #---------------------------------------------------------------------------
  # Uncertainty from betaTmax
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  
  btmax <-  rnorm(length(7501:15300), mean = betas[7501:15300,"betatmax.fallspr"], sd = sd(betas[7501:15300,"betatmax.fallspr"]))
  betas.all <-data.frame(b0, bSDI, bSDI_ppt, bSI, bSI_tmax.fallspr, bSI_wintP.wateryr,
                         bSI_X, bX, bX2, bX_SDI, bX_ppt, 
                         bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  
  # ppt <- proj.ordered$mean.ppt
  # tmax <- proj.ordered$mean.tmax.fs
  # #ppt <- time_data$wintP.wateryr[m,]
  # #tmax <- time_data$tmax.fallspr[m,]
  # SDI <- rep(cov.data[m, ]$SDI, length(ppt))
  # #x <- x.mat[,"x[1,53]"]
  # covariates <- data.frame(SDI, ppt, tmax)
  # 
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = mean(x.mat[,paste0("x[", m,",", 53,"]")]), m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - mean(x.mat[,paste0("x[", m,",", 53,"]")])
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - forecast[,t-1]
    }  }
  
  varianceSDI.X.precip.tmax <- apply(forecast,2,var)
  forecast.SDI.X.precip.tmax <- apply(forecast, 2, function(x){quantile(x, na.rm=TRUE)})
  
  
  varianceSDI.X.precip.tmax.inc <- apply(inc,2,var)
  forecast.SDI.X.precip.tmax.inc <- apply(inc, 2, function(x){quantile(x, na.rm=TRUE)})
  
  
  #---------------------------------------------------------------------------
  # Uncertainty from T and P interactions
  #---------------------------------------------------------------------------
  # use all of the parameter MCMCS:
  
  btmax_ppt <-  rnorm(length(7501:15300), mean = betas[7501:15300,"betatmax.fallspr_wintP.wateryr"], sd = sd(betas[7501:15300,"betatmax.fallspr_wintP.wateryr"]))
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bSI, bSI_tmax.fallspr, bSI_wintP.wateryr,
                          bSI_X, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  # ppt <- proj.ordered$mean.ppt
  # tmax <- proj.ordered$mean.tmax.fs
  # #ppt <- time_data$wintP.wateryr[m,]
  # #tmax <- time_data$tmax.fallspr[m,]
  # SDI <- rep(cov.data[m, ]$SDI, length(ppt))
  # #x <- x.mat[,"x[1,53]"]
  # covariates <- data.frame(SDI, ppt, tmax)
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = mean(x.mat[,paste0("x[", m,",", 53,"]")]), m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - mean(x.mat[,paste0("x[", m,",", 53,"]")])
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - forecast[,t-1]
    }  }
  
  
  varianceSDI.X.precip.tmax.int <- apply(forecast,2,var)
  forecast.SDI.X.precip.tmax.int <- apply(forecast, 2, function(x){quantile(x, na.rm=TRUE)})
  
  varianceSDI.X.precip.tmax.int.inc <- apply(inc,2,var)
  forecast.SDI.X.precip.tmax.int.inc <- apply(inc, 2, function(x){quantile(x, na.rm=TRUE)})
  
  #-------------------------------------------------------------------------
  #Uncertainty from SICOND
  #-------------------------------------------------------------------------
  if("betaSICOND" %in% colnames(betas) == FALSE ){
    bSI <- rep(0, length(7501:15300))
    
  }else{
    bSI <-  rnorm(length(7501:15300), mean = betas[7501:15300,"betaSICOND"], sd = sd(betas[7501:15300,"betatmax.fallspr_wintP.wateryr"]))
    
  }
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bSI, bSI_tmax.fallspr, bSI_wintP.wateryr,
                          bSI_X, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = mean(x.mat[,paste0("x[", m,",", 53,"]")]), m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - mean(x.mat[,paste0("x[", m,",", 53,"]")])
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - forecast[,t-1]
    }  }
  
  
  varianceSDI.X.precip.tmax.int.SICOND <- apply(forecast,2,var)
  forecast.SDI.X.precip.tmax.int.SICOND  <- apply(forecast, 2, function(x){quantile(x, na.rm=TRUE)})
  
  varianceSDI.X.precip.tmax.int.inc.SICOND  <- apply(inc,2,var)
  forecast.SDI.X.precip.tmax.int.inc.SICOND  <- apply(inc, 2, function(x){quantile(x, na.rm=TRUE)})
  #---------------------------------------------------------------------------
  # Uncertainty from all other Two way interactions
  #---------------------------------------------------------------------------
  alpha = quantile(alphas[, alphaplotid],0.5)
  
  if("betaX_SDI" %in% colnames(betas) == FALSE ){
    bX_SDI <- rep(0, length(7501:15300))
  }else{
    bX_SDI <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betaX_SDI"]), sd = sd(betas[7501:15300,"betaX_SDI"]))
    
  }
  
  if("betaSICOND" %in% colnames(betas) == FALSE ){
    #bSI <- rep(0, length(7501:15300))
    bSI_X <- rep(0, length(7501:15300))
    bSI_wintP.wateryr <- rep(0, length(7501:15300))
    bSI_tmax.fallspr <- rep(0, length(7501:15300))
  }else{
    #bSI <- rnorm(length(7501:15300),mean = mean(betas[7501:15300,"betaSICOND_wintP.wateryr"]), sd = sd(betas[7501:15300,"betaSICOND_wintP.wateryr"]))
    bSI_X <- rnorm(length(7501:15300),mean = mean(betas[7501:15300,"betaX_SICOND"]), sd = sd(betas[7501:15300,"betaX_SICOND"]))
    bSI_wintP.wateryr <- rnorm(length(7501:15300),mean = mean(betas[7501:15300,"betaSICOND_wintP.wateryr"]), sd = sd(betas[7501:15300,"betaSICOND_wintP.wateryr"]))
    bSI_tmax.fallspr <- rnorm(length(7501:15300),mean = mean(betas[7501:15300,"betaSICOND_tmax.fallspr"]), sd = sd(betas[7501:15300,"betaSICOND_tmax.fallspr"]))
  }
  
  
  bSDI_ppt <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betaSDI_wintP.wateryr"]), sd = sd(betas[7501:15300,"betaSDI_wintP.wateryr"]))
  bX_ppt <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betaX_wintP.wateryr"]), sd = sd(betas[7501:15300,"betaX_wintP.wateryr"]))
  bX_tmax <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betaX_tmax.fallspr"]), sd = sd(betas[7501:15300,"betaX_tmax.fallspr"]))
  bSDI_tmax <- rnorm(length(7501:15300), mean = mean(betas[7501:15300,"betaSDI_tmax.fallspr"]), sd = sd(betas[7501:15300,"betaSDI_tmax.fallspr"]))
  b0 <- rnorm(length(7501:15300), mean = mean(B0[7501:15300]), sd = sd(B0[7501:15300]))
  
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bSI, bSI_tmax.fallspr, bSI_wintP.wateryr,
                          bSI_X, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = mean(x.mat[,paste0("x[", m,",", 53,"]")]), m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - mean(x.mat[,paste0("x[", m,",", 53,"]")])
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - forecast[,t-1]
    }  }
  
  varianceSDI.X.precip.tmax.int.two.way <- apply(forecast,2,var)
  forecast.SDI.X.precip.tmax.int.two.way <- apply(forecast, 2, function(x){quantile(x, na.rm=TRUE)})
  
  varianceSDI.X.precip.tmax.int.two.way.inc <- apply(inc,2,var)
  forecast.SDI.X.precip.tmax.int.two.way.inc <- apply(inc, 2, function(x){quantile(x, na.rm=TRUE)})
  #---------------------------------------------------------------------------
  # Uncertainty from all paramters (including alpha plot)
  #---------------------------------------------------------------------------
  alpha <- rnorm(length(7501:15300), mean = mean(alphas[7501:15300, alphaplotid]), sd = sd(alphas[7501:15300, alphaplotid]))
  betas.all <- data.frame(b0, bSDI, bSDI_ppt, bSI, bSI_tmax.fallspr, bSI_wintP.wateryr,
                          bSI_X, bX, bX2, bX_SDI, bX_ppt, 
                          bppt, btmax, bX_tmax, bSDI_tmax, btmax_ppt)
  for(t in 1:time_steps){
    if(t == 1){
      dbh.pred <- iterate_statespace.dbh(x = mean(x.mat[,paste0("x[", m,",", 53,"]")]), m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - mean(x.mat[,paste0("x[", m,",", 53,"]")])
    }else{
      dbh.pred <- iterate_statespace.dbh(x = forecast[,t-1], m = m, betas.all = betas.all, alpha = alpha, SDdbh = 0, covariates = covariates[t,])
      forecast[,t] <- dbh.pred
      inc[,t] <- forecast[,t] - forecast[,t-1]
    }  }
  
  
  varianceall.params <- apply(forecast,2,var)
  forecastall.params <- apply(forecast, 2, function(x){quantile(x, na.rm=TRUE)})
  
  
  varianceall.params.inc<- apply(inc,2,var)
  forecastall.params.inc <- apply(inc, 2, function(x){quantile(x, na.rm=TRUE)})
  #---------------------------------------------------------------------------------- 
  # combine variances:
  
  if(type == "dbh"){
    V.pred.sim     <- rbind( varianceSDI, varianceSDI.X, varianceSDI.X.precip, varianceSDI.X.precip.tmax, 
                             varianceSDI.X.precip.tmax.int, varianceSDI.X.precip.tmax.int.SICOND,varianceSDI.X.precip.tmax.int.two.way, varianceall.params)
    V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
    
    pred.sims     <- data.frame(SDI.0 =forecast.SDI[1,],
                                X.0 = forecast.SDI.X[1,],
                                Precip.0 =forecast.SDI.X.precip[1,],
                                tmax.0 =forecast.SDI.X.precip.tmax[1,],
                                PTint.0= forecast.SDI.X.precip.tmax.int[1,],
                                SI.0 = forecast.SDI.X.precip.tmax.int.SICOND[1,],
                                twoway.0 = forecast.SDI.X.precip.tmax.int.two.way[1,],
                                all.0=forecastall.params[1,],
                                
                                SDI.100 =forecast.SDI[5,],
                                X.100 = forecast.SDI.X[5,],
                                Precip.100 =forecast.SDI.X.precip[5,],
                                tmax.100 =forecast.SDI.X.precip.tmax[5,],
                                PTint.100= forecast.SDI.X.precip.tmax.int[5,],
                                SI.100 = forecast.SDI.X.precip.tmax.int.SICOND[5,],
                                twoway.100 = forecast.SDI.X.precip.tmax.int.two.way[5,],
                                all.100=forecastall.params[5,],
                                year = 2018:2099)
    axis.name <- "diameter"
    
  }else{
    V.pred.sim     <- rbind( varianceSDI.inc, varianceSDI.X.inc, varianceSDI.X.precip.inc, varianceSDI.X.precip.tmax.inc, 
                             varianceSDI.X.precip.tmax.int.inc, varianceSDI.X.precip.tmax.int.inc.SICOND,varianceSDI.X.precip.tmax.int.two.way.inc, varianceall.params.inc)
    V.pred.sim.rel <- apply(V.pred.sim,2,function(x) {x/max(x)})
    
    pred.sims     <- data.frame(SDI.0 =forecast.SDI.inc[1,],
                                X.0 = forecast.SDI.X.inc[1,],
                                Precip.0 =forecast.SDI.X.precip.inc[1,],
                                tmax.0 =forecast.SDI.X.precip.tmax.inc[1,],
                                PTint.0= forecast.SDI.X.precip.tmax.int.inc[1,],
                                SI.0 = forecast.SDI.X.precip.tmax.int.inc.SICOND[1,],
                                twoway.0 = forecast.SDI.X.precip.tmax.int.two.way.inc[1,],
                                all.0=forecastall.params.inc[1,],
                                
                                SDI.100 =forecast.SDI.inc[5,],
                                X.100 = forecast.SDI.X.inc[5,],
                                Precip.100 =forecast.SDI.X.precip.inc[5,],
                                tmax.100 =forecast.SDI.X.precip.tmax.inc[5,],
                                PTint.100= forecast.SDI.X.precip.tmax.int.inc[5,],
                                SI.100 = forecast.SDI.X.precip.tmax.int.inc.SICOND[5,],
                                twoway.100 = forecast.SDI.X.precip.tmax.int.two.way.inc[5,],
                                all.100=forecastall.params.inc[5,],
                                year = 2018:2099)
    axis.name <- "Increment"
    
  }
  
  pred.sims.m <- reshape2::melt(pred.sims, id.vars = "year")
  pred.sims.class <- pred.sims.m %>% separate(col = variable, sep = "[.]", into = c("unc","lo")) %>%
    spread(key = lo, value = value)
  colnames(pred.sims.class) <- c("year", "uncertainty", "Low", "High")
  my_cols <- c("#7fc97f",
               "#beaed4",
               "#fdc086",
               "#ffff99",
               "#386cb0",
               "#f0027f",
               "#bf5b17",
               "#666666")
  
  pred.sims.class$uncertainty <- factor(pred.sims.class$uncertainty, levels = c("all","twoway","SI","PTint", "tmax", "Precip", "X", "SDI"))
  
  
  predY_plot <- ggplot(data = pred.sims.class, aes(x=year, fill = uncertainty))+
    geom_ribbon(aes(ymin=Low, ymax=High), color = "grey")+
    ylab(axis.name)+
    xlab("Year")+theme_bw()+
    scale_fill_manual(values = my_cols, name = NULL)+ theme(legend.position = "none", panel.grid = element_blank())
  
  predY_plot 
  
  
  # 
  ####
  ####  PLOT THE FORECASTING UNCERTAINTY PARTITION -------------------------------
  ####
  var_rel_preds <- as.data.frame(t(V.pred.sim.rel*100))
  var_rel_preds$x <- 1:nrow(var_rel_preds)
  #my_cols <- c("black", "grey55", "grey70")
  
  
  # pretty colored plot:
  
  
  tmpvar <- var_rel_preds
  tmpvar$year <- 2018:2099
  colnames(tmpvar) <- c( "SDI","DBH","Precip",
                         "Tmax", "PrecipxTmax", "SICOND", "all other 2-way interactions", "plot random effect","x", "year")
  variance.df <- tmpvar %>%
    gather(simtype, variance, -x, -year)
  
  variance.df$simtype <- factor(variance.df$simtype, levels = rev(c( "SDI","DBH", "Precip","Tmax",
                                                                     "PrecipxTmax", "SICOND", "all other 2-way interactions", "plot random effect"))
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
  
  
  write.csv(variance.df, paste0("variance_partitioning_parameter/tree_", m,"_", axis.name, "_", scenario,"_",output.base.name,"_proportion", ".csv"))
  write.csv(pred.sims.class, paste0("variance_partitioning_parameter/tree_", m,"_", axis.name, "_", scenario,"_",output.base.name,"_totalunc", ".csv"))
  
  
  if(print == TRUE){
    if(prop == TRUE){
      prop.var
      
    }else{
      predY_plot
    }
  }else{
    cat(m)
  }
}
plot.prop.parameter.variance(m = 100, prop = TRUE, scenario = "rcp26", type = "ringwidth", print = TRUE, Xplot = FALSE)


treeds <- c(1:515)

# proportions for rcp 2.6
for(k in treeds){
  plot.prop.parameter.variance(m = k, prop = TRUE, scenario = "rcp26", type = "ringwidth", print = FALSE, Xplot = FALSE)
}

for(k in treeds){
  plot.prop.parameter.variance(m = k, prop = TRUE, scenario = "rcp26", type = "dbh", print = FALSE, Xplot = FALSE)
}

# rcp 4.5
for(k in treeds){
  plot.prop.parameter.variance(m = k, prop = TRUE, scenario = "rcp45", type = "ringwidth", print = FALSE, Xplot = FALSE)
}

for(k in treeds){
  plot.prop.parameter.variance(m = k, prop = TRUE, scenario = "rcp45", type = "dbh", print = FALSE, Xplot = FALSE)
}

# rcp 6.0
for(k in treeds){
  plot.prop.parameter.variance(m = k, prop = TRUE, scenario = "rcp60", type = "ringwidth", print = FALSE, Xplot = FALSE)
}

for(k in treeds){
  plot.prop.parameter.variance(m = k, prop = TRUE, scenario = "rcp60", type = "dbh", print = FALSE, Xplot = FALSE)
}

# rcp 8.5
for(k in treeds){
  plot.prop.parameter.variance(m = k, prop = TRUE, scenario = "rcp85", type = "ringwidth", print = FALSE, Xplot = FALSE)
}

for(k in treeds){
  plot.prop.parameter.variance(m = k, prop = TRUE, scenario = "rcp85", type = "dbh", print = FALSE, Xplot = FALSE)
}

#------------------------------------------------------------------------------------------
#  Summarise the tree ring increment proportion of uncertainty
#------------------------------------------------------------------------------------------
inc.prop.list <- list()
treeds <- c(1:515)
df.2 <- list()
for(xy in treeds){
  df <- read.csv(paste0("variance_partitioning_parameter/tree_", xy,"_Increment_rcp45_", output.base.name, "_proportion.csv"))
  df$treeid <- xy
  df$rcp <- "rcp4.5"
  df.2[[xy]]<- df
}
inc.prop.list.2.6 <- lapply(treeds, function(xy){df <- read.csv(paste0("variance_partitioning_parameter/tree_", xy,"_Increment_rcp26_", output.base.name, "_proportion.csv"))
df$treeid <- xy
df$rcp <- "rcp2.6"
df})
inc.prop <- do.call(rbind, inc.prop.list.2.6)


inc.prop.list.45 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_parameter/tree_", x,"_Increment_rcp45_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp4.5"
df})
inc.prop.45 <- do.call(rbind, inc.prop.list.45)

inc.prop.list.60 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_parameter/tree_", x,"_Increment_rcp60_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp6.0"
df})
inc.prop.60 <- do.call(rbind, inc.prop.list.60)



inc.prop.list.85 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_parameter/tree_", x,"_Increment_rcp85_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp8.5"
df})
inc.prop.85 <- do.call(rbind, inc.prop.list.85)

inc.prop.all <- rbind(inc.prop, inc.prop.45, inc.prop.60, inc.prop.85)
saveRDS(inc.prop.all, paste0("inc.prop.parameter_",output.base.name, ".RDS"))
#inc.prop <- read.csv("variance_partitioning_parameter/tree_100_Increment_rcp26_proportion.csv")

my_cols <- c("#7fc97f",
             "#beaed4",
             "#fdc086",
             "#ffff99",
             "#386cb0",
             "#f0027f",
             "#bf5b17",
             "#666666")
inc.prop.wide <- inc.prop.all %>% select(-X,) %>% group_by(x,year, rcp) %>% spread(simtype, variance)
inc.prop.wide$SDIUnc <-  inc.prop.wide$`SDI` 
inc.prop.wide$DBHUnc <-  inc.prop.wide$`DBH` - inc.prop.wide$SDI 
inc.prop.wide$PrecipUnc <-  inc.prop.wide$`Precip`- inc.prop.wide$`DBH`
inc.prop.wide$TmaxUnc <-  inc.prop.wide$`Tmax`- inc.prop.wide$`Precip`
inc.prop.wide$PxTint <-  inc.prop.wide$`PrecipxTmax` - inc.prop.wide$`Tmax`
inc.prop.wide$SICONDUnc <-  inc.prop.wide$`SICOND` - inc.prop.wide$`PrecipxTmax`
inc.prop.wide$TwoWayInteractions <-  inc.prop.wide$`all other 2-way interactions` - inc.prop.wide$`SICOND`

inc.prop.wide$PlotrandUnc <-  inc.prop.wide$`plot random effect` - inc.prop.wide$`all other 2-way interactions`

inc.prop.wide$period <- ifelse(inc.prop.wide$year >= 2075, "2075 - 2099", 
                               ifelse(inc.prop.wide$year >= 2050 & inc.prop.wide$year < 2075, "2050 - 2074", 
                                      ifelse(inc.prop.wide$year < 2050, "2019 - 2049", NA)))

inc.prop.long <- inc.prop.wide %>% group_by(period, year, treeid, x, rcp) %>% select(-`SDI` , -`Precip`, -`Tmax`, - `DBH`,
                                                                                     -`PrecipxTmax`, -`SICOND`, -`all other 2-way interactions`, -`plot random effect`) %>%
  tidyr::gather(Uncertainty, value ,-x, -year, -treeid, -period, -rcp)
#inc.prop.wide <- reshape2::melt(inc.prop.wide)
rcp.inc.prop <- inc.prop.long %>% group_by(period, Uncertainty, rcp) %>% summarise(mean = mean(value), 
                                                                                   ci.lo = quantile(value, 0.025), 
                                                                                   ci.hi = quantile(value, 0.975)) 
rcp.inc.prop$Uncertainty <- factor(rcp.inc.prop$Uncertainty , levels = c("DBHUnc", 
                                                                         "PrecipUnc" , 
                                                                         "TmaxUnc",
                                                                         "PxTint" , 
                                                                         
                                                                         "SDIUnc" , 
                                                                         "SICONDUnc",
                                                                         
                                                                         "TwoWayInteractions", 
                                                                         "PlotrandUnc"))

Uncertainty.table <- data.frame(Uncertainty = c("DBHUnc", 
                                                "PrecipUnc" , 
                                                "TmaxUnc",
                                                "PxTint" , 
                                                
                                                "SDIUnc" , 
                                                "SICONDUnc",
                                                
                                                "TwoWayInteractions", 
                                                "PlotrandUnc"),
                                Unc = c("Tree Size", 
                                        "Precipitation", 
                                        "Temperature", 
                                        "Precipitation & Temperature Interaction", 
                                        "Stand Density Index", 
                                        "Site Index", 
                                        "Other two-way interactions", 
                                        "Plot random effects"))


rcp.inc.proportion.summary <- merge(rcp.inc.prop, Uncertainty.table, by = "Uncertainty")

rcp.inc.proportion.summary$Unc <- factor(rcp.inc.proportion.summary$Unc , levels = c("Other two-way interactions", 
                                                                                     
                                                                                     
                                                                                     
                                                                                     
                                                                                     "Precipitation & Temperature Interaction", 
                                                                                     
                                                                                     "Tree Size",
                                                                                     "Site Index",
                                                                                     "Precipitation", 
                                                                                     "Stand Density Index",
                                                                                     "Temperature",
                                                                                     "Plot random effects"))

#ggplot( inc.prop.long, aes(x = rcp , y = value, fill = Uncertainty))+geom_boxplot()+facet_wrap(~period)
increment.unc.summary.prop <- ggplot(rcp.inc.proportion.summary, aes(x = rcp , y = mean, fill = Unc))+geom_bar(stat = "identity")+facet_wrap(~period)+
  ylab("Proportion of Uncertainty\n in Increment")+xlab("RCP Scenario")+scale_fill_manual(values = my_cols, name = NULL)+
  theme_bw(base_size = 12)+theme(axis.text.x = element_text(hjust = 1, angle = 45), panel.grid = element_blank())


png(height = 4, width = 6, units = "in", res = 300, paste0(output.base.name,  paste0("_Proportion_increment_unc_proportion_parameters_rcp_all_time",output.base.name,".png")))
increment.unc.summary.prop
dev.off()


#------------------------------------------------------------------------------------------
#  Summarise the tree ring Diameter proportion of uncertainty
#------------------------------------------------------------------------------------------
dbh.prop.list <- list()
treeds <- c(1:515)
dbh.prop.list.2.6 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_parameter/tree_", x,"_diameter_rcp26_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp2.6"
df})
dbh.prop <- do.call(rbind, dbh.prop.list.2.6)


dbh.prop.list.45 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_parameter/tree_", x,"_diameter_rcp45_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp4.5"
df})
dbh.prop.45 <- do.call(rbind, dbh.prop.list.45)

dbh.prop.list.60 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_parameter/tree_", x,"_diameter_rcp60_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp6.0"
df})
dbh.prop.60 <- do.call(rbind, dbh.prop.list.60)



dbh.prop.list.85 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_parameter/tree_", x,"_diameter_rcp85_", output.base.name, "_proportion.csv"))
df$treeid <- x
df$rcp <- "rcp8.5"
df})
dbh.prop.85 <- do.call(rbind, dbh.prop.list.85)

dbh.prop.all <- rbind(dbh.prop, dbh.prop.45, dbh.prop.60, dbh.prop.85)

saveRDS(dbh.prop.all, paste0("dbh.prop.parameter",output.base.name,".RDS"))
#dbh.prop <- read.csv("variance_partitioning_parameter/tree_100_Diameter_rcp26_proportion.csv")

dbh.prop.wide <- dbh.prop.all %>% select(-X,) %>% group_by(x,year, rcp) %>% spread(simtype, variance)
dbh.prop.wide$SDIUnc <-  dbh.prop.wide$`SDI` 
dbh.prop.wide$DBHUnc <-  dbh.prop.wide$`DBH` - dbh.prop.wide$SDI 
dbh.prop.wide$PrecipUnc <-  dbh.prop.wide$`Precip`- dbh.prop.wide$`DBH`
dbh.prop.wide$TmaxUnc <-  dbh.prop.wide$`Tmax`- dbh.prop.wide$`Precip`
dbh.prop.wide$PxTint <-  dbh.prop.wide$`PrecipxTmax` - dbh.prop.wide$`Tmax`
dbh.prop.wide$SICONDUnc <-  dbh.prop.wide$`SICOND` - dbh.prop.wide$`PrecipxTmax`
dbh.prop.wide$TwoWayInteractions <-  dbh.prop.wide$`all other 2-way interactions` - dbh.prop.wide$`SICOND`

dbh.prop.wide$PlotrandUnc <-  dbh.prop.wide$`plot random effect` - dbh.prop.wide$`all other 2-way interactions`

dbh.prop.wide$period <- ifelse(dbh.prop.wide$year >= 2075, "2075 - 2099", 
                               ifelse(dbh.prop.wide$year >= 2050 & dbh.prop.wide$year < 2075, "2050 - 2074", 
                                      ifelse(dbh.prop.wide$year < 2050, "2019 - 2049", NA)))

dbh.prop.long <- dbh.prop.wide %>% group_by(period, year, treeid, x, rcp) %>% select(-`SDI` , -`Precip`, -`Tmax`, - `DBH`,
                                                                                     -`PrecipxTmax`, -`SICOND`, -`all other 2-way interactions`, -`plot random effect`) %>%
  tidyr::gather(Uncertainty, value ,-x, -year, -treeid, -period, -rcp)
#dbh.prop.wide <- reshape2::melt(dbh.prop.wide)
rcp.dbh.prop <- dbh.prop.long %>% group_by(period, Uncertainty, rcp) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                                                   ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                   ci.hi = quantile(value, 0.975, na.rm = TRUE)) 
rcp.dbh.prop$Uncertainty <- factor(rcp.dbh.prop$Uncertainty , levels = c("DBHUnc", 
                                                                         "PrecipUnc" , 
                                                                         "TmaxUnc",
                                                                         "PxTint" , 
                                                                         
                                                                         "SDIUnc" , 
                                                                         "SICONDUnc",
                                                                         
                                                                         "TwoWayInteractions", 
                                                                         "PlotrandUnc"))

Uncertainty.table <- data.frame(Uncertainty = c("DBHUnc", 
                                                "PrecipUnc" , 
                                                "TmaxUnc",
                                                "PxTint" , 
                                                
                                                "SDIUnc" , 
                                                "SICONDUnc",
                                                
                                                "TwoWayInteractions", 
                                                "PlotrandUnc"),
                                Unc = c("Tree Size", 
                                        "Precipitation", 
                                        "Temperature", 
                                        "Precipitation & Temperature Interaction", 
                                        "Stand Density Index", 
                                        "Site Index", 
                                        "Other two-way interactions", 
                                        "Plot random effects"))


rcp.dbh.proportion.summary <- merge(rcp.dbh.prop, Uncertainty.table, by = "Uncertainty")

rcp.dbh.proportion.summary$Unc <- factor(rcp.dbh.proportion.summary$Unc , levels = c("Other two-way interactions", 
                                                                                     
                                                                                     
                                                                                     
                                                                                     
                                                                                     "Precipitation & Temperature Interaction", 
                                                                                     
                                                                                     "Tree Size",
                                                                                     "Site Index",
                                                                                     "Precipitation", 
                                                                                     "Stand Density Index",
                                                                                     "Temperature",
                                                                                     "Plot random effects"))

#ggplot( dbh.prop.long, aes(x = rcp , y = value, fill = Uncertainty))+geom_boxplot()+facet_wrap(~period)
Diameter.unc.summary.prop <- ggplot(rcp.dbh.proportion.summary, aes(x = rcp , y = mean, fill = Unc))+geom_bar(stat = "identity")+facet_wrap(~period)+
  ylab("Proportion of Uncertainty\n in Diameter")+xlab("RCP Scenario")+scale_fill_manual(values = my_cols, name = NULL)+
  theme_bw(base_size = 12)+theme(axis.text.x = element_text(hjust = 1, angle = 45), panel.grid = element_blank())


png(height = 4, width = 6, units = "in", res = 300, paste0(output.base.name,  paste0("_Proportion_Diameter_unc_proportion_parameters_rcp_all_time",output.base.name,".png")))
Diameter.unc.summary.prop
dev.off()


png(height = 6, width = 7, units = "in", res = 300, paste0("both_uncertainty_prop_summary_parameters",output.base.name,".png"))
#cowplot::plot_grid(
cowplot::plot_grid(increment.unc.summary.prop, Diameter.unc.summary.prop, ncol = 1, align = "hv", labels = "AUTO")#, #Uncertainty.legend,ncol = 2, rel_widths = c(1, 0.05))
dev.off()


# kh left off here
#--------------------------------------------------------------------------------------
# Plot summaries of the total increment uncertainty summarised across all the trees
#--------------------------------------------------------------------------------------
inc.tot.list <- list()
treeds <- c(1:515)
inc.tot.list.2.6 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_parameter/tree_", x,"_Increment_rcp26_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp2.6"
df})
inc.tot <- do.call(rbind, inc.tot.list.2.6)


inc.tot.list.45 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_parameter/tree_", x,"_Increment_rcp45_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp4.5"
df})
inc.tot.45 <- do.call(rbind, inc.tot.list.45)

inc.tot.list.60 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_parameter/tree_", x,"_Increment_rcp60_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp6.0"
df})
inc.tot.60 <- do.call(rbind, inc.tot.list.60)



inc.tot.list.85 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_parameter/tree_", x,"_Increment_rcp85_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp8.5"
df})
inc.tot.85 <- do.call(rbind, inc.tot.list.85)

inc.tot.all <- rbind(inc.tot, inc.tot.45, inc.tot.60, inc.tot.85)

saveRDS(inc.tot.all, paste0("INC.TOT.parameters",output.base.name,".RDS"))
#inc.tot <- read.csv("variance_partitioning_parameter/tree_100_Increment_rcp26_totortion.csv")
inc.tot.all <- inc.tot.all[complete.cases(inc.tot.all),]
inc.tot.all$diff <- ifelse(is.infinite(abs(inc.tot.all$High - inc.tot.all$Low)), 1000, abs(inc.tot.all$High - inc.tot.all$Low))
inc.tot.less <- inc.tot.all %>% filter(diff <= 5)
hist(inc.tot.less$diff )


inc.tot.wide <- inc.tot.less %>% select( -Low, -High, -X)  %>% group_by(year,treeid, rcp) %>% spread(uncertainty, diff)
inc.tot.wide$`Precipitation` <-  inc.tot.wide$Precip
inc.tot.wide$`Precipitation Temperature Interaction` <-  inc.tot.wide$PTint 
inc.tot.wide$`Stand Density Index` <-  inc.tot.wide$SDI
inc.tot.wide$`Site Index` <-  inc.tot.wide$SI
inc.tot.wide$`Temperature` <-  inc.tot.wide$tmax
inc.tot.wide$`All two way interactions` <-  inc.tot.wide$twoway
inc.tot.wide$`DBH` <-  inc.tot.wide$X
inc.tot.wide$`Random Effects` <-  inc.tot.wide$all

inc.tot.wide$period <- ifelse(inc.tot.wide$year >= 2075, "2075 - 2099", 
                              ifelse(inc.tot.wide$year >= 2050 & inc.tot.wide$year < 2075, "2050 - 2074", 
                                     ifelse(inc.tot.wide$year < 2050, "2019 - 2049", NA)))

inc.tot.long <- inc.tot.wide %>% group_by(period, year, treeid,  rcp) %>% select(-Precip , -tmax, -twoway,
                                                                                 -X, -SDI, -SI, -PTint, -all) %>%
  gather(Uncertainty, value , -year, -treeid, -period, -rcp)
#inc.tot.wide <- reshape2::melt(inc.tot.wide)
rcp.inc.tot <- inc.tot.long %>% group_by(period, Uncertainty, rcp) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                                                 ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                 ci.hi = quantile(value, 0.975, na.rm = TRUE)) 
rcp.inc.tot$Uncertainty <- factor(rcp.inc.tot$Uncertainty , levels = c("Random Effects" ,
                                                                       "All two way interactions" , 
                                                                       "DBH"   , 
                                                                       "Precipitation"  , 
                                                                       "Temperature",
                                                                       
                                                                       "Precipitation Temperature Interaction", 
                                                                       "Site Index" , 
                                                                       "Stand Density Index"))

Uncertainty.table <- data.frame(Uncertainty = c("Random Effects" ,
                                                "All two way interactions" , 
                                                "DBH"   , 
                                                "Precipitation"  , 
                                                "Temperature",
                                                
                                                "Precipitation Temperature Interaction", 
                                                "Site Index" , 
                                                "Stand Density Index"),
                                Unc = c("Random Effects" ,
                                        "All two way interactions" , 
                                        "DBH"   , 
                                        "Precipitation"  , 
                                        "Temperature",
                                        
                                        "Precipitation Temperature Interaction", 
                                        "Site Index" , 
                                        "Stand Density Index"))


rcp.inc.totortion.summary <- merge(rcp.inc.tot, Uncertainty.table, by = "Uncertainty")
rcp.inc.totortion.summary$Unc <- factor(rcp.inc.totortion.summary$Unc , levels  = c("All two way interactions", 
                                                                                    
                                                                                    "Precipitation Temperature Interaction", 
                                                                                    
                                                                                    "DBH",
                                                                                    "Site Index",
                                                                                    "Precipitation", 
                                                                                    "Stand Density Index",
                                                                                    "Temperature",
                                                                                    "Random Effects"))

#ggplot( inc.tot.long, aes(x = rcp , y = value, fill = Uncertainty))+geom_boxplot()+facet_wrap(~period)
increment.unc.summary.tot <- ggplot(rcp.inc.totortion.summary, aes(x = rcp , y = mean, fill = Unc))+geom_bar(stat = "identity")+facet_wrap(~period)+
  ylab("Mean Total Uncertainty\n in Increment")+xlab("RCP Scenario")+scale_fill_manual(values = my_cols, name = NULL)+
  theme_bw(base_size = 12)+theme(axis.text.x = element_text(hjust = 1, angle = 45), panel.grid = element_blank())


png(height = 4, width = 6, units = "in", res = 300, paste0(output.base.name, "_total_increment_unc_proportion_parameters_rcp_all_time.png"))
increment.unc.summary.tot
dev.off()


#--------------------------------------------------------------------------------------
# Plot summaries of the total Diameter uncertainty summarised across all the trees
#--------------------------------------------------------------------------------------
dbh.tot.list <- list()
treeds <- c(1:515)
dbh.tot.list.2.6 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_parameter/tree_", x,"_diameter_rcp26_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp2.6"
df})
dbh.tot <- do.call(rbind, dbh.tot.list.2.6)


dbh.tot.list.45 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_parameter/tree_", x,"_diameter_rcp45_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp4.5"
df})
dbh.tot.45 <- do.call(rbind, dbh.tot.list.45)

dbh.tot.list.60 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_parameter/tree_", x,"_diameter_rcp60_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp6.0"
df})
dbh.tot.60 <- do.call(rbind, dbh.tot.list.60)



dbh.tot.list.85 <- lapply(treeds, function(x){df <- read.csv(paste0("variance_partitioning_parameter/tree_", x,"_diameter_rcp85_", output.base.name, "_totalunc.csv"))
df$treeid <- x
df$rcp <- "rcp8.5"
df})
dbh.tot.85 <- do.call(rbind, dbh.tot.list.85)

dbh.tot.all <- rbind(dbh.tot, dbh.tot.45, dbh.tot.60, dbh.tot.85)

saveRDS(dbh.tot.all, paste0("dbh.TOT.parameters.RDS",output.base.name,".RDS"))



dbh.tot.all$diff <- ifelse(is.infinite(abs(dbh.tot.all$High - dbh.tot.all$Low)), 100, 
                           ifelse(abs(dbh.tot.all$High - dbh.tot.all$Low) >= 100,100,abs(dbh.tot.all$High - dbh.tot.all$Low)))
dbh.tot.all$mean <- dbh.tot.all$High- ((dbh.tot.all$High - dbh.tot.all$Low)/2)

dbh.tot.wide <- dbh.tot.all %>% select( -Low, -High, -X, -mean)  %>% group_by(year,treeid, rcp) %>% spread(uncertainty,diff)
dbh.tot.wide$`Precipitation` <-  dbh.tot.wide$Precip
dbh.tot.wide$`Precipitation Temperature Interaction` <-  dbh.tot.wide$PTint 
dbh.tot.wide$`Stand Density Index` <-  dbh.tot.wide$SDI
dbh.tot.wide$`Site Index` <-  dbh.tot.wide$SI
dbh.tot.wide$`Temperature` <-  dbh.tot.wide$tmax
dbh.tot.wide$`All two way interactions` <-  dbh.tot.wide$twoway
dbh.tot.wide$`DBH` <-  dbh.tot.wide$X
dbh.tot.wide$`Random Effects` <-  dbh.tot.wide$all

dbh.tot.wide$period <- ifelse(dbh.tot.wide$year >= 2075, "2075 - 2099", 
                              ifelse(dbh.tot.wide$year >= 2050 & dbh.tot.wide$year < 2075, "2050 - 2074", 
                                     ifelse(dbh.tot.wide$year < 2050, "2019 - 2049", NA)))

dbh.tot.long <- dbh.tot.wide %>% group_by(period, year, treeid,  rcp) %>% select(-Precip , -tmax, -twoway,
                                                                                 -X, -SDI, -SI, -PTint, -all) %>%
  gather(Uncertainty, value , -year, -treeid, -period, -rcp)
#dbh.tot.wide <- reshape2::melt(dbh.tot.wide)
rcp.dbh.tot <- dbh.tot.long %>% group_by(period, Uncertainty, rcp) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                                                 ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                 ci.hi = quantile(value, 0.975, na.rm = TRUE)) 
rcp.dbh.tot$Uncertainty <- factor(rcp.dbh.tot$Uncertainty , levels = c("Random Effects" ,
                                                                       "All two way interactions" , 
                                                                       "DBH"   , 
                                                                       "Precipitation"  , 
                                                                       "Temperature",
                                                                       
                                                                       "Precipitation Temperature Interaction", 
                                                                       "Site Index" , 
                                                                       "Stand Density Index"))

Uncertainty.table <- data.frame(Uncertainty = c("Random Effects" ,
                                                "All two way interactions" , 
                                                "DBH"   , 
                                                "Precipitation"  , 
                                                "Temperature",
                                                
                                                "Precipitation Temperature Interaction", 
                                                "Site Index" , 
                                                "Stand Density Index"),
                                Unc = c("Random Effects" ,
                                        "All two way interactions" , 
                                        "DBH"   , 
                                        "Precipitation"  , 
                                        "Temperature",
                                        
                                        "Precipitation Temperature Interaction", 
                                        "Site Index" , 
                                        "Stand Density Index"))


rcp.dbh.totortion.summary <- merge(rcp.dbh.tot, Uncertainty.table, by = "Uncertainty")
rcp.dbh.totortion.summary$Unc <- factor(rcp.dbh.totortion.summary$Unc , levels  = c("All two way interactions", 
                                                                                    
                                                                                    "Precipitation Temperature Interaction", 
                                                                                    
                                                                                    "DBH",
                                                                                    "Site Index",
                                                                                    "Precipitation", 
                                                                                    "Stand Density Index",
                                                                                    "Temperature",
                                                                                    "Random Effects"))

#ggplot( dbh.tot.long, aes(x = rcp , y = value, fill = Uncertainty))+geom_boxplot()+facet_wrap(~period)
Diameter.unc.summary.tot <- ggplot(rcp.dbh.totortion.summary, aes(x = rcp , y = mean, fill = Unc))+geom_bar(stat = "identity")+facet_wrap(~period)+
  ylab("Mean Total Uncertainty\n in Diameter")+xlab("RCP Scenario")+scale_fill_manual(values = my_cols, name = NULL)+
  theme_bw(base_size = 12)+theme(axis.text.x = element_text(hjust = 1, angle = 45), panel.grid = element_blank())

png(height = 4, width = 6, units = "in", res = 300, paste0(output.base.name, "_total_Diameter_unc_totortion_parameter_rcp_all_time.png"))
Diameter.unc.summary.tot
dev.off()

png(height = 6, width = 7, units = "in", res = 300, paste0("both_uncertainty_parameter_unc_totals_summary_",output.base.name,".png"))
#cowplot::plot_grid(
cowplot::plot_grid(increment.unc.summary.tot, Diameter.unc.summary.tot, ncol = 1, align = "hv", labels = "AUTO")#, #Uncertainty.legend,ncol = 2, rel_widths = c(1, 0.05))
dev.off()



#-----------------------------------------------------------------------------------------------
# summarise to see how many trees are increasing in increment over time, decreasing, or staying constant
#-----------------------------------------------------------------------------------------------
all.increment <- readRDS(paste0("INC.TOT.ALL", output.base.name, ".RDS"))
total.unc <- all.increment %>% filter(uncertainty %in% "IPP")
total.unc$mean <- total.unc$Low + abs(total.unc$High - total.unc$Low)/2
total.unc$mean <- ifelse(total.unc$mean >= 100, 100,
                         ifelse(total.unc$mean <= -100, -100, total.unc$mean))
one.tree <- total.unc %>% filter(treeid == 1)



ggplot()+geom_line(data = one.tree, aes(x = year, y = mean, color = rcp))+
  geom_ribbon(data = one.tree, aes(x = year, ymin = Low, ymax = High, fill = rcp), alpha = 0.5)+
  facet_wrap(~rcp)

one.tree.26 <- total.unc %>% filter(treeid == 1, rcp %in% "rcp2.6") 

summary(lm(mean ~ year, data = one.tree.26))


# plot out all the means
ggplot()+geom_line(data = total.unc, aes(x = year, y = mean, group = treeid))+ylim(-5, 5)+facet_wrap(~rcp)
total.unc.no.dups <- total.unc[!duplicated(total.unc %>% select(-X, -mean, - Low, -High)),]

total.unc$pos.neg.inc <- ifelse(total.unc$mean >= 0, "positive", "negative")
total.unc.alive.dead.ct <- total.unc %>% group_by(rcp, treeid, pos.neg.inc) %>%  summarise(n = n())

total.unc.alive.dead.ct$alive <- ifelse(total.unc.alive.dead.ct$n == 82, "Alive\n(positive growth)", "Dead\n(negative increments)")


total.alive.dead.summary <- total.unc.alive.dead.ct %>% group_by(rcp) %>% mutate(totaltrees = length((treeid))) %>% group_by(rcp, alive) %>% summarise(trees = n())%>%
  group_by(rcp) %>% mutate(total.trees = sum(trees)) %>% mutate(percettrees = (trees/total.trees)*100)

png(height = 6, width = 8, units = "in", res = 200, paste0("percent_trees_alive_dead_increments_",output.base.name,".png"))
ggplot(total.alive.dead.summary, aes(x = alive, y = percettrees, fill = alive))+geom_bar(stat = "identity")+facet_wrap(~rcp)+ylab("% of all trees")+xlab("")+
  theme_bw(base_size = 12)+scale_fill_manual(values = c("forestgreen", "grey"))+theme(panel.grid = element_blank())
dev.off()


total.unc$period <- ifelse(total.unc$year >= 2075, "2075 - 2099", 
                           ifelse(total.unc$year >= 2050 & total.unc$year < 2075, "2050 - 2074", 
                                  ifelse(total.unc$year < 2050, "2019 - 2049", NA)))



mean.growth.summary<- total.unc %>% group_by(rcp, period) %>% summarise(average.growth = mean(mean), 
                                                                        ci.low = quantile(Low, 0.025), 
                                                                        ci.high = quantile(High, 0.975))
ggplot(total.unc, aes(x = period, y = mean))+geom_boxplot(outlier.size = 0.1)+ylim(-10, 10)+facet_wrap(~rcp)

png(height = 6, width = 8, units = "in", res = 200, paste0("average_increments_",output.base.name,".png"))
ggplot(mean.growth.summary, aes(x = period, y = average.growth, fill = period)) + geom_bar(stat = "identity")+facet_wrap(~rcp)+
  geom_errorbar(data = mean.growth.summary, aes(x = period, ymin = ci.low, ymax = ci.high, width = 0.1)) + ylab("Average Forecasted Increment")+xlab("")+theme_bw()+
  theme(panel.grid = element_blank(), axis.text.x = element_text(hjust = 1, angle = 45))
dev.off()

plot.prop.parameter.variance(m = 8, prop = TRUE, scenario = "rcp60", type = "dbh")#+ggtitle(paste0("Contribution to  Driver Uncertainty in DBH for Tree",i))
plot.prop.parameter.variance(m = 8, prop = FALSE, scenario = "rcp60", type = "dbh")
plot.prop.parameter.variance(m = 8, prop = TRUE, scenario = "rcp60", type = "ringwidth")
plot.prop.parameter.variance(m = 8, prop = FALSE, scenario = "rcp60", type = "ringwidth")


plot.prop.parameter.variance(m = 30,  scenario = "rcp60", type = "dbh")+ggtitle(paste0("Contribution to  Driver Uncertainty in DBH for Tree",i))
plot.prop.parameter.variance(m = 30,  scenario = "rcp60", type = "ringwidth")+ggtitle(paste0("Contribution to  Driver Uncertainty in DBH for Tree",i))



#------------------------------------------------------------------------------------
# Make figures for the paper
#------------------------------------------------------------------------------------


# figure with a single forecast & unc partitioning for a tree + 
my_cols <- c("#1b9e77",
             "#d95f02",
             "black",
             "#7570b3",
             "grey")

output.base.name <- "SDI_SI.norand.X.resampled"

# make the pretty uncertainty plots
inc.prop.all <- readRDS(paste0("inc.prop.all_",output.base.name, ".RDS"))
inc.prop.wide <- inc.prop.all %>% dplyr::select(-X,) %>% group_by(x,year, rcp) %>% spread(simtype, variance)
inc.prop.wide$InitialCondition <-  inc.prop.wide$`Initial Conditions`
inc.prop.wide$ProcessError <-  inc.prop.wide$`Process Error` - inc.prop.wide$`Driver Uncertainty`
inc.prop.wide$ParameterUnc <-  inc.prop.wide$`Parameter Uncertainty` - inc.prop.wide$`Initial Conditions`
inc.prop.wide$PlotrandUnc <-  inc.prop.wide$`Plot random effect` - inc.prop.wide$`Parameter Uncertainty`
inc.prop.wide$DriverUnc <-  inc.prop.wide$`Driver Uncertainty` - inc.prop.wide$`Plot random effect`

inc.prop.wide$period <- ifelse(inc.prop.wide$year >= 2075, "2075 - 2099",
                               ifelse(inc.prop.wide$year >= 2050 & inc.prop.wide$year < 2075, "2050 - 2074",
                                      ifelse(inc.prop.wide$year < 2050, "2019 - 2049", NA)))

inc.prop.long <- inc.prop.wide %>% dplyr::group_by(period, year, treeid, x, rcp) %>% dplyr::select(-`Driver Uncertainty` , -`Initial Conditions`, -`Parameter Uncertainty`,
                                                                                                   -`Plot random effect`, -`Process Error`) %>%
  gather(Uncertainty, value ,-x, -year, -treeid, -period, -rcp)
#inc.prop.wide <- reshape2::melt(inc.prop.wide)
rcp.inc.prop <- inc.prop.long %>% dplyr::group_by(period, Uncertainty, rcp) %>% summarise(mean = mean(value, na.rm =TRUE),
                                                                                          ci.lo = quantile(value, 0.025, na.rm =TRUE),
                                                                                          ci.hi = quantile(value, 0.975, na.rm =TRUE))
rcp.inc.prop$Uncertainty <- factor(rcp.inc.prop$Uncertainty , levels = c("ProcessError",
                                                                         "DriverUnc",
                                                                         "PlotrandUnc",
                                                                         
                                                                         "ParameterUnc",
                                                                         "InitialCondition"))

Uncertainty.table <- data.frame(Uncertainty = c("ProcessError",
                                                "DriverUnc",
                                                "PlotrandUnc",
                                                "ParameterUnc",
                                                "InitialCondition"),
                                Unc = c("Process",
                                        "Driver",
                                        "Random Effects",
                                        "Parameter",
                                        "Initial Conditions"))


rcp.inc.proportion.summary <- merge(rcp.inc.prop, Uncertainty.table, by = "Uncertainty")

rcp.inc.proportion.summary$Unc <- factor(rcp.inc.proportion.summary$Unc , levels = c("Process",
                                                                                     "Driver",
                                                                                     "Random Effects",
                                                                                     "Parameter",
                                                                                     "Initial Conditions"))

#ggplot( inc.prop.long, aes(x = rcp , y = value, fill = Uncertainty))+geom_boxplot()+facet_wrap(~period)
increment.unc.summary.prop <- ggplot(rcp.inc.proportion.summary, aes(x = rcp , y = mean, fill = Unc))+geom_bar(stat = "identity")+facet_wrap(~period)+
  ylab("Proportion of Uncertainty\n in Increment")+xlab("RCP Scenario")+scale_fill_manual(values = my_cols, name = NULL)+
  theme_bw(base_size = 12)+theme(axis.text.x = element_text(hjust = 1, angle = 45), panel.grid = element_blank())
increment.unc.summary.prop

#------------------------------------------------------------------------------------------
#  Summarise the tree ring Diameter proportion of uncertainty
#--------------------------------------------------------------------------------------
dbh.prop.all <- readRDS(paste0("dbh.prop.all",output.base.name, ".RDS"))
#dbh.prop <- read.csv("variance_partitioning/tree_100_Diameter_rcp26_proportion.csv")


dbh.prop.wide <- dbh.prop.all %>% dplyr::select(-X,) %>% group_by(x,year, rcp) %>% spread(simtype, variance)
dbh.prop.wide$InitialCondition <-  dbh.prop.wide$`Initial Conditions`
dbh.prop.wide$ProcessError <-  dbh.prop.wide$`Process Error` - dbh.prop.wide$`Driver Uncertainty`
dbh.prop.wide$ParameterUnc <-  dbh.prop.wide$`Parameter Uncertainty` - dbh.prop.wide$`Initial Conditions`
dbh.prop.wide$PlotrandUnc <-  dbh.prop.wide$`Plot random effect` - dbh.prop.wide$`Parameter Uncertainty`
dbh.prop.wide$DriverUnc <-  dbh.prop.wide$`Driver Uncertainty` - dbh.prop.wide$`Plot random effect`

dbh.prop.wide$period <- ifelse(dbh.prop.wide$year >= 2075, "2075 - 2099",
                               ifelse(dbh.prop.wide$year >= 2050 & dbh.prop.wide$year < 2075, "2050 - 2074",
                                      ifelse(dbh.prop.wide$year < 2050, "2019 - 2049", NA)))

dbh.prop.long <- dbh.prop.wide %>% dplyr::group_by(period, year, treeid, x, rcp) %>% dplyr::select(-`Driver Uncertainty` , -`Initial Conditions`, -`Parameter Uncertainty`,
                                                                                                   -`Plot random effect`, -`Process Error`) %>%
  gather(Uncertainty, value ,-x, -year, -treeid, -period, -rcp)
#dbh.prop.wide <- reshape2::melt(dbh.prop.wide)
rcp.dbh.prop <- dbh.prop.long %>% group_by(period, Uncertainty, rcp) %>% summarise(mean = mean(value, na.rm =TRUE),
                                                                                   ci.lo = quantile(value, 0.025, na.rm = TRUE),
                                                                                   ci.hi = quantile(value, 0.975, na.rm = TRUE))
rcp.dbh.prop$Uncertainty <- factor(rcp.dbh.prop$Uncertainty , levels = c("ProcessError",
                                                                         "DriverUnc",
                                                                         "PlotrandUnc",
                                                                         
                                                                         "ParameterUnc",
                                                                         "InitialCondition"))

Uncertainty.table <- data.frame(Uncertainty = c("ProcessError",
                                                "DriverUnc",
                                                "PlotrandUnc",
                                                "ParameterUnc",
                                                "InitialCondition"),
                                Unc = c("Process",
                                        "Driver",
                                        "Random Effects",
                                        "Parameter",
                                        "Initial Conditions"))


rcp.dbh.proportion.summary <- merge(rcp.dbh.prop, Uncertainty.table, by = "Uncertainty")

rcp.dbh.proportion.summary$Unc <- factor(rcp.dbh.proportion.summary$Unc , levels = c("Process",
                                                                                     "Driver",
                                                                                     "Random Effects",
                                                                                     "Parameter",
                                                                                     "Initial Conditions"))
#ggplot( dbh.prop.long, aes(x = rcp , y = value, fill = Uncertainty))+geom_boxplot()+facet_wrap(~period)
Diameter.unc.summary.prop <- ggplot(rcp.dbh.proportion.summary, aes(x = rcp , y = mean, fill = Unc))+geom_bar(stat = "identity")+facet_wrap(~period)+
  ylab("Proportion of Uncertainty\n in Diameter")+xlab("RCP Scenario")+scale_fill_manual(values = my_cols, name = NULL)+
  theme_bw(base_size = 12)+theme(axis.text.x = element_text(hjust = 1, angle = 45), panel.grid = element_blank())

Uncertainty.legend<- cowplot::get_legend(increment.unc.summary.prop)

increment.unc.summary.prop
Diameter.unc.summary.prop

png(height = 6, width = 7, units = "in", res = 300, paste0("both_uncertainty_prop_summary_",output.base.name,".png"))
#cowplot::plot_grid(
cowplot::plot_grid(increment.unc.summary.prop, Diameter.unc.summary.prop, ncol = 1, align = "hv", labels = "AUTO")#, #Uncertainty.legend,ncol = 2, rel_widths = c(1, 0.05))
dev.off()

#----------------------------------------------------------------------------------------
# add in an individual breakdown for a single tree:
scenario = "rcp45"
m <- 8

variance.df <- read.csv( paste0("variance_partitioning/tree_", m,"_", "Increment", "_", scenario,"_",output.base.name,"_proportion", ".csv"))
variance.df$simtype <- factor(variance.df$simtype, levels = rev(c("Initial Conditions", "Parameter Uncertainty","Plot random effect", "Driver Uncertainty", "Process Error")))

prop.var.inc.8 <- ggplot(variance.df, aes(x=year, fill = simtype))+
  geom_ribbon(aes(ymin=0, ymax=variance), color = "grey")+
  ylab(paste("% of total variance \n for increment"))+    xlab("Year")+
  scale_fill_manual(values = my_cols, name = NULL)+#, 
  #labels = c("Process error", "Driver uncertainty",  "Parameter uncertainty","alpha uncertainty", "Initial conditions"))+
  scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                     expand = c(0, 0))+
  theme_bw()+theme(panel.grid = element_blank())
prop.var.inc.8

# for total increment uncertainty:

scenario = "rcp45"
pred.sims.class <- read.csv( paste0("variance_partitioning/tree_", m,"_", "Increment", "_", scenario,"_",output.base.name,"_totalunc", ".csv"))
#variance.df$simtype <- factor(variance.df$simtype, levels = rev(c("Initial Conditions", "Parameter Uncertainty","Plot random effect", "Driver Uncertainty", "Process Error")))


pred.sims.class$uncertainty <- factor(pred.sims.class$uncertainty, levels = c("IPP","IPD","IPA", "IP", "I"))


predY_plot.8 <- ggplot(data = pred.sims.class, aes(x=year, fill = uncertainty))+
  geom_ribbon(aes(ymin=Low, ymax=High), color = "grey")+
  ylab("Incement (cm)")+
  xlab("Year")+theme_bw()+
  scale_fill_manual(values = my_cols, name = NULL)+ theme(legend.position = "none", panel.grid = element_blank())

predY_plot.8

# DBH forecasts for tree 1:

variance.df <- read.csv( paste0("variance_partitioning/tree_", m,"_", "Diameter", "_", scenario,"_",output.base.name,"_proportion", ".csv"))
variance.df$simtype <- factor(variance.df$simtype, levels = rev(c("Initial Conditions", "Parameter Uncertainty","Plot random effect", "Driver Uncertainty", "Process Error")))

prop.var.dbh.8 <- ggplot(variance.df, aes(x=year, fill = simtype))+
  geom_ribbon(aes(ymin=0, ymax=variance), color = "grey")+
  ylab(paste("% of total variance \n for diameter"))+    xlab("Year")+
  scale_fill_manual(values = my_cols, name = NULL)+#, 
  #labels = c("Process error", "Driver uncertainty",  "Parameter uncertainty","alpha uncertainty", "Initial conditions"))+
  scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                     expand = c(0, 0))+
  theme_bw()+theme(panel.grid = element_blank(), legend.position = "bottom", legend.direction = "horizontal")
prop.var.dbh.8

# for total increment uncertainty:


pred.sims.class <- read.csv( paste0("variance_partitioning/tree_", m,"_", "Diameter", "_", scenario,"_",output.base.name,"_totalunc", ".csv"))
#variance.df$simtype <- factor(variance.df$simtype, levels = rev(c("Initial Conditions", "Parameter Uncertainty","Plot random effect", "Driver Uncertainty", "Process Error")))


pred.sims.class$uncertainty <- factor(pred.sims.class$uncertainty, levels = c("IPP","IPA","IPD", "IP", "I"))


predY_plot.dbh.8 <- ggplot(data = pred.sims.class, aes(x=year, fill = uncertainty))+
  geom_ribbon(aes(ymin=Low, ymax=High), color = "grey")+
  ylab("Diameter (cm)")+
  xlab("Year")+theme_bw()+
  scale_fill_manual(values = my_cols, name = NULL)+ theme(legend.position = "none", panel.grid = element_blank())

predY_plot.dbh.8
no.leg <- theme(legend.position = "none")
legend.unc <- cowplot::get_legend(prop.var.dbh.8)

png(height = 6, width = 10, units = "in", res = 300, "example_tree_forecasts_rcp45_w_summary_prop_unc.png")
cowplot::plot_grid(
  cowplot::plot_grid(prop.var.inc.8 + no.leg, predY_plot.8 + no.leg, increment.unc.summary.prop+ no.leg,  
                     prop.var.dbh.8 + no.leg, predY_plot.dbh.8+ no.leg, Diameter.unc.summary.prop+ no.leg ,align = "hv", ncol = 3, labels = "AUTO", label_fontface = "plain"), 
  legend.unc, ncol = 1, rel_heights = c(1, 0.05) )
dev.off()



#-------------------------------------------------------------------------------------
# Make the driver uncertainty and the parameter uncertainty summary proportion plots
#-------------------------------------------------------------------------------------

dbh.prop.all<- readRDS( paste0("dbh.prop.parameter",output.base.name,".RDS"))
#dbh.prop <- read.csv("variance_partitioning_parameter/tree_100_Diameter_rcp26_proportion.csv")

dbh.prop.wide <- dbh.prop.all %>% select(-X,) %>% group_by(x,year, rcp) %>% spread(simtype, variance)
# dbh.prop.wide$SDIUnc <-  dbh.prop.wide$`SDI` 
# dbh.prop.wide$DBHUnc <-  dbh.prop.wide$`DBH` - dbh.prop.wide$SDI 
# dbh.prop.wide$PrecipUnc <-  dbh.prop.wide$`Precip`- dbh.prop.wide$`DBH`
# dbh.prop.wide$TmaxUnc <-  dbh.prop.wide$`Tmax`- dbh.prop.wide$`Precip`
# dbh.prop.wide$PxTint <-  dbh.prop.wide$`PrecipxTmax` - dbh.prop.wide$`Tmax`
# dbh.prop.wide$SICONDUnc <-  dbh.prop.wide$`SICOND` - dbh.prop.wide$`PrecipxTmax`
# dbh.prop.wide$TwoWayInteractions <-  dbh.prop.wide$`all other 2-way interactions` - dbh.prop.wide$`SICOND`
# 
# dbh.prop.wide$PlotrandUnc <-  dbh.prop.wide$`plot random effect` - dbh.prop.wide$`all other 2-way interactions`

dbh.prop.wide$period <- ifelse(dbh.prop.wide$year >= 2075, "2075 - 2099", 
                               ifelse(dbh.prop.wide$year >= 2050 & dbh.prop.wide$year < 2075, "2050 - 2074", 
                                      ifelse(dbh.prop.wide$year < 2050, "2019 - 2049", NA)))

dbh.prop.long <- dbh.prop.wide %>% group_by(period, year, treeid, x, rcp) %>% #select(-`SDI` , -`Precip`, -`Tmax`, - `DBH`,
  #      -`PrecipxTmax`, -`SICOND`, -`all other 2-way interactions`, -`plot random effect`) %>%
  tidyr::gather(Uncertainty, value ,-x, -year, -treeid, -period, -rcp)
#dbh.prop.wide <- reshape2::melt(dbh.prop.wide)
rcp.dbh.prop <- dbh.prop.long %>% group_by(year, Uncertainty, rcp) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                                                 ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                 ci.hi = quantile(value, 0.975, na.rm = TRUE)) 
# rcp.dbh.prop$Uncertainty <- factor(rcp.dbh.prop$Uncertainty , levels = c("DBHUnc", 
#                                                                          "PrecipUnc" , 
#                                                                          "TmaxUnc",
#                                                                          "PxTint" , 
#                                                                          
#                                                                          "SDIUnc" , 
#                                                                          "SICONDUnc",
#                                                                          
#                                                                          "TwoWayInteractions", 
#                                                                          "PlotrandUnc"))

Uncertainty.table <- data.frame(Uncertainty = c("DBH", 
                                                "Precip" , 
                                                "Tmax",
                                                "PrecipxTmax" , 
                                                
                                                "SDI" , 
                                                "SICOND",
                                                
                                                "all other 2-way interactions", 
                                                "plot random effect"),
                                Unc = c("Tree Size", 
                                        "Precipitation", 
                                        "Temperature", 
                                        "Precipitation & Temperature Interaction", 
                                        "Stand Density Index", 
                                        "Site Index", 
                                        "Other two-way interactions", 
                                        "Plot random effects"))


rcp.dbh.proportion.summary <- merge(rcp.dbh.prop, Uncertainty.table, by = "Uncertainty")

rcp.dbh.proportion.summary$Unc <- factor(rcp.dbh.proportion.summary$Unc , levels = rev(c( "Stand Density Index",
                                                                                          "Tree Size",
                                                                                          "Precipitation", 
                                                                                          
                                                                                          
                                                                                          
                                                                                          
                                                                                          
                                                                                          "Temperature",
                                                                                          "Precipitation & Temperature Interaction",
                                                                                          "Site Index",
                                                                                          "Other two-way interactions", 
                                                                                          "Plot random effects")))


param_cols <- c("#7fc97f",
                "#beaed4",
                "#fdc086",
                "#ffff99",
                "#386cb0",
                "#f0027f",
                "#bf5b17",
                "#666666")
rcp.dbh.proportion.summary %>% group_by(Unc)%>% summarise(unc.mean = mean(mean))

prop.var.param.dbh  <- ggplot(rcp.dbh.proportion.summary, aes(x=year, fill = Unc))+
  geom_ribbon(aes(ymin=0, ymax=mean), color = NA)+
  ylab(paste("% of parameter variance \n for", "Diameter"))+
  xlab("Year")+
  scale_fill_manual(values = param_cols, name = NULL)+#, 
  #labels = c("Process error", "Driver uncertainty",  "Parameter uncertainty","alpha uncertainty", "Initial conditions"))+
  scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                     expand = c(0, 0))+
  theme_bw()+theme(panel.grid = element_blank())+facet_wrap(~rcp, ncol = 4)
prop.var.param.dbh

# average across all rcp scenarios:

rcp.dbh.prop.all <- dbh.prop.long %>% group_by(year, Uncertainty) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                                                ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                ci.hi = quantile(value, 0.975, na.rm = TRUE)) 


rcp.dbh.prop.period <- dbh.prop.long %>% group_by( Uncertainty, period) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                                                      ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                      ci.hi = quantile(value, 0.975, na.rm = TRUE)) 

Uncertainty.table <- data.frame(Uncertainty = c("DBH", 
                                                "Precip" , 
                                                "Tmax",
                                                "PrecipxTmax" , 
                                                
                                                "SDI" , 
                                                "SICOND",
                                                
                                                "all other 2-way interactions", 
                                                "plot random effect"),
                                Unc = c("Tree Size", 
                                        "Precipitation", 
                                        "Temperature", 
                                        "Precipitation & Temperature Interaction", 
                                        "Stand Density Index", 
                                        "Site Index", 
                                        "Other two-way interactions", 
                                        "Plot random effects"))


rcp.dbh.proportion.summary.all <- merge(rcp.dbh.prop.all, Uncertainty.table, by = "Uncertainty")

rcp.dbh.proportion.summary.all$Unc <- factor(rcp.dbh.proportion.summary.all$Unc , levels = rev(c( 
  "Stand Density Index",
  "Tree Size",
  "Precipitation", 
  "Temperature",
  "Precipitation & Temperature Interaction",
  "Site Index",
  "Other two-way interactions", 
  "Plot random effects")))


param_cols <- c("#7fc97f",
                "#beaed4",
                "#fdc086",
                
                "#ffff99",
                "#f0027f",
                "#386cb0",
                
                "#bf5b17",
                "#666666")
rcp.dbh.proportion.summary.all %>% group_by(Unc)%>% summarise(unc.mean = mean(mean))

prop.var.param.dbh.all  <- ggplot(rcp.dbh.proportion.summary.all, aes(x=year, fill = Unc))+
  geom_ribbon(aes(ymin=0, ymax=mean))+
  ylab(paste("% of parameter variance \n for", "Diameter"))+
  xlab("Year")+
  scale_fill_manual(values = param_cols, name = NULL)+#, 
  #labels = c("Process error", "Driver uncertainty",  "Parameter uncertainty","alpha uncertainty", "Initial conditions"))+
  scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                     expand = c(0, 0))+
  theme_bw()+theme(panel.grid = element_blank())#+facet_wrap(~rcp, ncol = 4)
prop.var.param.dbh.all


# Now make the same parameter plot for increment
inc.prop.all<- readRDS( paste0("inc.prop.parameter_",output.base.name,".RDS"))
#inc.prop <- read.csv("variance_partitioning_parameter/tree_100_Increment_rcp26_proportion.csv")

inc.prop.wide <- inc.prop.all %>% select(-X,) %>% group_by(x,year, rcp) %>% spread(simtype, variance)
# inc.prop.wide$SDIUnc <-  inc.prop.wide$`SDI` 
# inc.prop.wide$incUnc <-  inc.prop.wide$`inc` - inc.prop.wide$SDI 
# inc.prop.wide$PrecipUnc <-  inc.prop.wide$`Precip`- inc.prop.wide$`inc`
# inc.prop.wide$TmaxUnc <-  inc.prop.wide$`Tmax`- inc.prop.wide$`Precip`
# inc.prop.wide$PxTint <-  inc.prop.wide$`PrecipxTmax` - inc.prop.wide$`Tmax`
# inc.prop.wide$SICONDUnc <-  inc.prop.wide$`SICOND` - inc.prop.wide$`PrecipxTmax`
# inc.prop.wide$TwoWayInteractions <-  inc.prop.wide$`all other 2-way interactions` - inc.prop.wide$`SICOND`
# 
# inc.prop.wide$PlotrandUnc <-  inc.prop.wide$`plot random effect` - inc.prop.wide$`all other 2-way interactions`

inc.prop.wide$period <- ifelse(inc.prop.wide$year >= 2075, "2075 - 2099", 
                               ifelse(inc.prop.wide$year >= 2050 & inc.prop.wide$year < 2075, "2050 - 2074", 
                                      ifelse(inc.prop.wide$year < 2050, "2019 - 2049", NA)))

inc.prop.long <- inc.prop.wide %>% group_by(period, year, treeid, x, rcp) %>% #select(-`SDI` , -`Precip`, -`Tmax`, - `inc`,
  #      -`PrecipxTmax`, -`SICOND`, -`all other 2-way interactions`, -`plot random effect`) %>%
  tidyr::gather(Uncertainty, value ,-x, -year, -treeid, -period, -rcp)
#inc.prop.wide <- reshape2::melt(inc.prop.wide)
rcp.inc.prop <- inc.prop.long %>% group_by(year, Uncertainty, rcp) %>% summarise(mean = median(value, na.rm = TRUE), 
                                                                                 ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                 ci.hi = quantile(value, 0.975, na.rm = TRUE)) 
# rcp.inc.prop$Uncertainty <- factor(rcp.inc.prop$Uncertainty , levels = c("incUnc", 
#                                                                          "PrecipUnc" , 
#                                                                          "TmaxUnc",
#                                                                          "PxTint" , 
#                                                                          
#                                                                          "SDIUnc" , 
#                                                                          "SICONDUnc",
#                                                                          
#                                                                          "TwoWayInteractions", 
#                                                                          "PlotrandUnc"))

Uncertainty.table.dbh <- data.frame(Uncertainty = c("DBH", 
                                                    "Precip" , 
                                                    "Tmax",
                                                    "PrecipxTmax" , 
                                                    
                                                    "SDI" , 
                                                    "SICOND",
                                                    
                                                    "all other 2-way interactions", 
                                                    "plot random effect"),
                                    Unc = c("Tree Size", 
                                            "Precipitation", 
                                            "Temperature", 
                                            "Precipitation & Temperature Interaction", 
                                            "Stand Density Index", 
                                            "Site Index", 
                                            "Other two-way interactions", 
                                            "Plot random effects"))


rcp.inc.proportion.summary <- merge(rcp.inc.prop, Uncertainty.table.dbh, by = "Uncertainty")

rcp.inc.proportion.summary$Unc <- factor(rcp.inc.proportion.summary$Unc , levels = rev(c("Stand Density Index",
                                                                                         "Tree Size",
                                                                                         "Precipitation", 
                                                                                         "Temperature",
                                                                                         "Precipitation & Temperature Interaction",
                                                                                         "Site Index",
                                                                                         "Other two-way interactions", 
                                                                                         "Plot random effects")))


param_cols <- c("#7fc97f",
                "#beaed4",
                "#fdc086",
                
                "#ffff99",
                "#f0027f",
                "#386cb0",
                
                "#bf5b17",
                "#666666")
rcp.inc.proportion.summary %>% group_by(Unc)%>% summarise(unc.mean = mean(mean))

prop.var.param.inc  <- ggplot(rcp.inc.proportion.summary, aes(x=year, fill = Unc))+
  geom_ribbon(aes(ymin=0, ymax=mean))+
  ylab(paste("% of parameter variance \n for", "Increment"))+
  xlab("Year")+
  scale_fill_manual(values = param_cols, name = NULL)+#, 
  #labels = c("Process error", "Driver uncertainty",  "Parameter uncertainty","alpha uncertainty", "Initial conditions"))+
  scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                     expand = c(0, 0))+
  theme_bw()+theme(panel.grid = element_blank())+facet_wrap(~rcp, ncol = 4)
prop.var.param.inc

# average across all the rcps:
rcp.inc.prop.all <- inc.prop.long %>% group_by(year, Uncertainty) %>% summarise(mean = median(value, na.rm = TRUE), 
                                                                                ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                ci.hi = quantile(value, 0.975, na.rm = TRUE)) 

rcp.inc.proportion.summary.all <- merge(rcp.inc.prop.all, Uncertainty.table, by = "Uncertainty")

rcp.inc.proportion.summary.all$Unc <- factor(rcp.inc.proportion.summary.all$Unc , levels = rev(c("Stand Density Index",
                                                                                                 "Tree Size",
                                                                                                 "Precipitation", 
                                                                                                 "Temperature",
                                                                                                 "Precipitation & Temperature Interaction",
                                                                                                 "Site Index",
                                                                                                 "Other two-way interactions", 
                                                                                                 "Plot random effects")))



rcp.inc.proportion.summary.all %>% group_by(Unc)%>% summarise(unc.mean = mean(mean))

prop.var.param.inc.all  <- ggplot(rcp.inc.proportion.summary.all, aes(x=year, fill = Unc))+
  geom_ribbon(aes(ymin=0, ymax=mean))+
  ylab(paste("% of parameter variance \n for", "Increment"))+
  xlab("Year")+
  scale_fill_manual(values = param_cols, name = NULL)+#, 
  #labels = c("Process error", "Driver uncertainty",  "Parameter uncertainty","alpha uncertainty", "Initial conditions"))+
  scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                     expand = c(0, 0))+
  theme_bw()+theme(panel.grid = element_blank())
prop.var.param.inc.all



#------------------------------------------------
# Make the same plots but for the drivers
#------------------------------------------------
variance.df <- readRDS("inc.prop.driver_SDI_SI.norand.X.resampled.RDS")
variance.df$simtype <- factor(variance.df$simtype, levels = c("Temperature Uncertainty", "Precipitation Uncertainty"))

inc.prop.wide <- variance.df %>% select(-X,) %>% group_by(x,year, rcp) %>% spread(simtype, variance)

inc.prop.wide$period <- ifelse(inc.prop.wide$year >= 2075, "2075 - 2099", 
                               ifelse(inc.prop.wide$year >= 2050 & inc.prop.wide$year < 2075, "2050 - 2074", 
                                      ifelse(inc.prop.wide$year < 2050, "2019 - 2049", NA)))

inc.prop.long <- inc.prop.wide %>% group_by(period, year, treeid, x, rcp) %>% #select(-`SDI` , -`Precip`, -`Tmax`, - `inc`,
  #      -`PrecipxTmax`, -`SICOND`, -`all other 2-way interactions`, -`plot random effect`) %>%
  tidyr::gather(Uncertainty, value ,-x, -year, -treeid, -period, -rcp)
#inc.prop.wide <- reshape2::melt(inc.prop.wide)
rcp.inc.prop <- inc.prop.long %>% group_by(year, Uncertainty, rcp) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                                                 ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                 ci.hi = quantile(value, 0.975, na.rm = TRUE)) 

rcp.inc.prop$Uncertainty <- factor(rcp.inc.prop$Uncertainty, levels = rev(c("Precipitation Uncertainty", "Temperature Uncertainty")))

driver_cols <- c("#b2182b","#2166ac")

prop.var.driver.unc <- ggplot(rcp.inc.prop, aes(x=year, fill = Uncertainty))+
  geom_ribbon(aes(ymin=0, ymax=mean), color = "grey")+
  ylab(paste("% of driver uncertainty \n for", "Increment"))+
  xlab("Year")+
  scale_fill_manual(values = driver_cols, name = NULL)+#, 
  #labels = c("Process error", "Driver uncertainty",  "Parameter uncertainty","alpha uncertainty", "Initial conditions"))+
  scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                     expand = c(0, 0))+
  theme_bw()+theme(panel.grid=element_blank())+facet_grid(~rcp)
prop.var.driver.unc


## average across all RCP scenarios:
rcp.inc.prop.all <- inc.prop.long %>% group_by(year, Uncertainty) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                                                ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                ci.hi = quantile(value, 0.975, na.rm = TRUE)) 

rcp.inc.prop.all$Uncertainty <- factor(rcp.inc.prop.all$Uncertainty, levels = rev(c("Precipitation Uncertainty", "Temperature Uncertainty")))

driver_cols <- c("#b2182b","#2166ac")

prop.var.driver.unc.all <- ggplot(rcp.inc.prop.all, aes(x=year, fill = Uncertainty))+
  geom_ribbon(aes(ymin=0, ymax=mean), color = "grey")+
  ylab(paste("% of driver uncertainty \n for", "Increment"))+
  xlab("Year")+
  scale_fill_manual(values = driver_cols, name = NULL)+#, 
  #labels = c("Process error", "Driver uncertainty",  "Parameter uncertainty","alpha uncertainty", "Initial conditions"))+
  scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                     expand = c(0, 0))+
  theme_bw()+theme(panel.grid=element_blank())
prop.var.driver.unc.all


#------------------------------------------------
variance.df<- readRDS("dbh.prop.driverSDI_SI.norand.X.resampled.RDS")
variance.df$simtype <- factor(variance.df$simtype, levels = c("Temperature Uncertainty", "Precipitation Uncertainty"))

dbh.prop.wide <- variance.df %>% select(-X,) %>% group_by(x,year, rcp) %>% spread(simtype, variance)

dbh.prop.wide$period <- ifelse(dbh.prop.wide$year >= 2075, "2075 - 2099", 
                               ifelse(dbh.prop.wide$year >= 2050 & dbh.prop.wide$year < 2075, "2050 - 2074", 
                                      ifelse(dbh.prop.wide$year < 2050, "2019 - 2049", NA)))

dbh.prop.long <- dbh.prop.wide %>% group_by(period, year, treeid, x, rcp) %>% #select(-`SDI` , -`Precip`, -`Tmax`, - `dbh`,
  #      -`PrecipxTmax`, -`SICOND`, -`all other 2-way interactions`, -`plot random effect`) %>%
  tidyr::gather(Uncertainty, value ,-x, -year, -treeid, -period, -rcp)
#dbh.prop.wide <- reshape2::melt(dbh.prop.wide)
rcp.dbh.prop <- dbh.prop.long %>% group_by(year, Uncertainty, rcp) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                                                 ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                 ci.hi = quantile(value, 0.975, na.rm = TRUE)) 

rcp.dbh.prop$Uncertainty <- factor(rcp.dbh.prop$Uncertainty, levels = rev(c("Precipitation Uncertainty", "Temperature Uncertainty")))

driver_cols <- c("#b2182b","#2166ac")

prop.var.driver.unc.dbh <- ggplot(rcp.dbh.prop, aes(x=year, fill = Uncertainty))+
  geom_ribbon(aes(ymin=0, ymax=mean), color = "grey")+
  ylab(paste("% of driver uncertainty \n for", "Diameter"))+
  xlab("Year")+
  scale_fill_manual(values = driver_cols, name = NULL)+#, 
  #labels = c("Process error", "Driver uncertainty",  "Parameter uncertainty","alpha uncertainty", "Initial conditions"))+
  scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                     expand = c(0, 0))+
  theme_bw()+theme(panel.grid=element_blank())+facet_grid(~rcp)
prop.var.driver.unc.dbh



# average across all RCPS:
rcp.dbh.prop.all <- dbh.prop.long %>% group_by(year, Uncertainty) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                                                ci.lo = quantile(value, 0.025, na.rm = TRUE), 
                                                                                ci.hi = quantile(value, 0.975, na.rm = TRUE)) 

rcp.dbh.prop.all$Uncertainty <- factor(rcp.dbh.prop.all$Uncertainty, levels = rev(c("Precipitation Uncertainty", "Temperature Uncertainty")))

driver_cols <- c("#b2182b","#2166ac")

prop.var.driver.unc.dbh.all <- ggplot(rcp.dbh.prop.all, aes(x=year, fill = Uncertainty))+
  geom_ribbon(aes(ymin=0, ymax=mean), color = "grey")+
  ylab(paste("% of driver uncertainty \n for", "Diameter"))+
  xlab("Year")+
  scale_fill_manual(values = driver_cols, name = NULL)+#, 
  #labels = c("Process error", "Driver uncertainty",  "Parameter uncertainty","alpha uncertainty", "Initial conditions"))+
  scale_y_continuous(labels=paste0(seq(0,100,25),"%"),
                     expand = c(0, 0))+
  theme_bw()+theme(panel.grid=element_blank())
prop.var.driver.unc.dbh.all
library(cowplot)
driver.leg <- get_legend(prop.var.driver.unc.all + theme(legend.position = "bottom", legend.direction = "horizontal"))
param.leg <- get_legend(prop.var.param.dbh.all + theme(legend.position = "bottom", legend.direction = "horizontal"))

# make one really big figure:
png(height = 12, width = 9, units = "in", res = 300, "driver_and_parameter_prop_unc_figure.png")
cowplot::plot_grid(prop.var.driver.unc + no.leg, 
                   prop.var.driver.unc.dbh + no.leg, 
                   driver.leg,
                   prop.var.param.inc + no.leg, 
                   prop.var.param.dbh + no.leg,
                   param.leg,
                   nrow = 6, rel_heights = c(1,1,0.1, 1,1,0.18), labels = c("A", "B", " ", "C", "D", " "), label_fontface = "plain")
dev.off()


driver.leg.vert <- get_legend(prop.var.driver.unc.all )
param.leg.vert <- get_legend(prop.var.param.dbh.all )


# figure with averages across RCPS
png(height = 6, width = 8, units = "in", res = 300, "driver_and_parameter_prop_unc_figure_averaged.png")
cowplot::plot_grid(plot_grid(prop.var.driver.unc.all + no.leg, 
                             prop.var.driver.unc.dbh.all + no.leg, ncol = 2, labels = c("A", "B"), label_fontface = "plain"), 
                   driver.leg,
                   plot_grid(prop.var.param.inc.all + no.leg, 
                             prop.var.param.dbh.all + no.leg,ncol = 2, labels = c("C", "D"), label_fontface = "plain"),
                   param.leg,
                   ncol = 1, rel_heights   = c(1,0.1,1, 0.28))
dev.off()

