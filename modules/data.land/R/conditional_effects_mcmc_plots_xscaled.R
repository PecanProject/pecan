# function to read in multiple MCMC files and plot the conditional effects of tree size, climate, si, SDI, etc
library(rjags)
library(PEcAn.data.land)
install.packages("pryr")
install.packages("gridExtra")
install.packages("psych")


# filename
# climate parameter --If climate list > 2, do the P + T plots
# the file number to start reading
# file number to end reading
# switch to say whether to plot just the fixed effects, or the two way interactions:


# functions pseudocode

# read in the jags.out files & get the effects

# If climate == 1 climate parameter, just run simple model

# If climate == 2 climate parameters, do the model with temperature and precipitation

# If fixed effects, output just the 1 way fixed effects. If not,output 2 way interactions,
library(tidyverse)
library(psych)
library(gridExtra)
library(pryr)

#####################################################################################
# 2. Plot Effects for Water Year Precip full model
#####################################################################################
file.base.name <- "SDI_SI.norand.X.nadapt.5000."
output.base.name <- "SDI_SI.norand.X.nadapt.5000"
stage2 <- TRUE
workingdir <- "/home/rstudio/"
climate <- "wintP.wateryr"
cov.data = cov.data


jags.comb <- NULL

for(i in 700:750){ # note this model stopped early b/c convergence
  load(paste0(workingdir,"IGF_PIPO_AZ_mcmc/", file.base.name,i,".RData"))
  new.out <- jags.out 
  
  if(is.null(jags.comb)){
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      
      if(length(x.cols)>0){ 
        jags.comb[[j]] <- new.out[[j]][,-x.cols]
      }else{
        jags.comb[[j]] <- new.out[[j]]
      }
      
    }
    
  } else {
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      
      if(length(x.cols)>0){ 
        new.out[[j]] <- new.out[[j]][,-x.cols]
      }else{
        new.out[[j]] <- new.out[[j]]
      }
      
      jags.comb[[j]]  <- rbind(jags.comb[[j]], new.out[[j]])
      rm(jags.out)
    }
  }
}

for(i in 1:3){
  jags.comb[[i]] <- as.mcmc(jags.comb[[i]])
}
jags.comb <- as.mcmc.list(jags.comb)
#save(jags.comb,file="IGF.waterYear.PPT.RData")
saveRDS(jags.comb,file=paste0("IGF",output.base.name,".rds"))

# check for convergence via gelman-rubin
gelman.diag(jags.comb)


# check for convergence via traceplots
png(height = 18, width = 10, units = "in", res = 200, paste0(workingdir,output.base.name,"_traceplots.png"))
par(mfrow=c(8,4))
traceplot(jags.comb[, "deviance"], main = expression(deviance), xlab = "last 2500 iterations")
traceplot(jags.comb[, "mu"], main = expression(mu), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX"], main = expression(beta~DBH), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), xlab = "last 2500 iterations")

traceplot(jags.comb[, "betaSDI"], main = expression(beta~SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSI"], main = expression(beta~SICOND), xlab = "last 2500 iterations")

traceplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_wateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_tmax.fallspr"], main = expression(beta~DBH_tmax.fallspr), xlab = "last 2500 iterations")

traceplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~wateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_wateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_wateryearPPT), xlab = "last 2500 iterations")

traceplot(jags.comb[, "betatmax.fallspr"], main = expression(beta~tmax.fallspr), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_TimeSinceFIRE"], main = expression(beta~X_TimeSinceFIRE), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_TimeSinceFIRE"], main = expression(beta~SI_TimeSinceFIRE), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaTimeSinceFIRE_wintP.wateryr"], main = expression(beta~TimeSinceFIRE_wintP.wateryr), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI_TimeSinceFIRE"], main = expression(beta~SDI_TimeSinceFIRE), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaTimeSinceFIRE_tmax.fallspr"], main = expression(beta~SDI_TimeSinceFIRE), xlab = "last 2500 iterations")


traceplot(jags.comb[, "betaTimeSinceFIRE"], main = expression(beta~TimeSinceFIRE), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI_tmax.fallspr"], main = expression(beta~SDI_tmax.fallspr), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_tmax.fallspr"], main = expression(beta~SI_tmax.fallspr), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betatmax.fallspr_wintP.wateryr"], main = expression(beta~tmax.fallspr_wintP.wateryr), xlab = "last 2500 iterations")


traceplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "tau_inc"], main = "tau_inc", xlab = "last 2500 iterations")
traceplot(jags.comb[, "tau_dbh"], main = "tau_dbh", xlab = "last 2500 iterations")
traceplot(jags.comb[, "tau_PLOT"], main = "tau_PLOT", xlab = "last 2500 iterations")
traceplot(jags.comb[, "tau_add"], main = "tau_add", xlab = "last 2500 iterations")

dev.off()

#-------------------------------------------------------------------------------
# Make parameter dotplots:
#-------------------------------------------------------------------------------
out <- as.matrix(jags.comb)
summary(out)
betas <- out[,grep(pattern = "beta",colnames(out))]
# just get the fixed effects:

betas.df <- data.frame(betas)
betas.random <- betas.df[, grep(patter = "betaX_PLOT", colnames(betas))]
names.fixed <- names(betas.df)[!(names(betas.df) %in% colnames(betas.random))] # get the names of fixed effects
betas.fixed <- betas.df[,names.fixed]

betas.fixed.m <- reshape2::melt(betas.fixed)
model.params <- betas.fixed.m %>% group_by(variable) %>% summarise(median = quantile(value, 0.5), 
                                                                   ci.lo = quantile(value, 0.025), 
                                                                   ci.hi = quantile(value, 0.975))
colnames(model.params)[1]<- c("Parameter")


dotplot.fixed <- ggplot(model.params, aes(x= Parameter, y = median ))+geom_point()+geom_hline(aes(yintercept = 0), color = "lightgrey", linetype = "dashed")+
  geom_errorbar(aes(x = Parameter, ymin = ci.lo, ymax = ci.hi), width = 0.01)+theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())+ylab("Coefficient  Value")

png(height = 4, width = 6, units = "in", res = 200, paste0(output.base.name, "_parameter_dotplots.png"))
dotplot.fixed
dev.off()

# export the model.params object so we can compare:
model.params$model <- paste0(output.base.name, "model3")
write.csv(model.params, paste0(output.base.name, "_parameter_cis.csv"), row.names = FALSE)



# if the model run was stage2  then lets plot the posteriors and priors together:
if(stage2 ==TRUE){
  #priors <- readRDS("/home/rstudio/INV_FIA_DATA/data/IGFPPT.Tmax.fs.only.climint.40000.rds")
  #priors <- readRDS(gzcon(url("https://de.cyverse.org/dl/d/1FF350EA-4CE4-4561-9BDA-7E90C062DBB4/IGFX_X2_scaled.rds")))
  priors <- readRDS("/home/rstudio/IGFSDI_SI.norand.X.nadapt.5000.rds")
  
  
  
  library(coda)
  means <- apply(as.matrix(posterior.ests), 2, mean)
  vars <- apply(as.matrix(posterior.ests), 2, var)
  SD <- apply(as.matrix(posterior.ests), 2, sd)
  
  # generate data frame with a summary of the posterior estimates
  posterior.summary <- data.frame(means = apply(as.matrix(posterior.ests), 2, mean),
                                  vars = apply(as.matrix(posterior.ests), 2, var),
                                  SD = apply(as.matrix(posterior.ests), 2, sd))
  posterior.summary$parameter <- rownames(posterior.summary)
  tau.inc <- (as.matrix(priors[,"tau_inc"]))
  tau.inc.post <- (as.matrix(jags.comb[,"tau_inc"]))
  
  # get beta column names
  betas <- colnames(as.matrix(priors))[grep(colnames(as.matrix(priors)), pattern = "beta")]
  taus <- colnames(as.matrix(priors))[grep(colnames(as.matrix(priors)), pattern = "tau")]
  all.params <-c(betas, taus)
  
  
  
  png(height = 12, width = 6, units = "in", res = 300, paste0(workingdir,output.base.name,"_priors_posteriors.png"))
  par(mfrow = c(5,3), xpd = NA)
  for(i in 1:length(all.params)){
    
    # find ranges:
    high <- max(as.matrix(priors[,all.params[i]]), as.matrix(jags.comb[,all.params[i]]))
    low <- min(as.matrix(jags.comb[,all.params[i]]), as.matrix(priors[,all.params[i]]))
    
    
    plot(density(as.matrix(priors[,all.params[i]])), main = all.params[i], xlim = c(low, high))
    lines(density(as.matrix(jags.comb[,all.params[i]])),col = "red")
    #legend("bottom",legend=c("prior","posterior"),col=c("black","red"),lwd=3)
    
    
  }
  legend("right",inset=c(-1.5, -1), legend=c("prior","posterior"),col=c("black","red"),lwd=3)
  dev.off()
  
}

png(height = 10, width = 8, units = "in", res = 200, paste0(workingdir,output.base.name,"_ACFplots.png")
    
    a<- acfplot(jags.comb[, "deviance"], main = "deviance", aspect = 1)
    b<- acfplot(jags.comb[, "mu"], main = expression(mu), aspect = 1)
    c<- acfplot(jags.comb[, "betaX"], main = expression(beta~DBH), aspect = 1)
    d<- acfplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), aspect = 1)
    e<- acfplot(jags.comb[, "betaSDI"], main = expression(beta~SDI),  aspect = 1)
    f<- acfplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), aspect = 1)
    g<- acfplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), aspect = 1)
    h<- acfplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_JanJulPPT), aspect = 1)
    i<- acfplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~wateryearPPT), aspect = 1)
    j<- acfplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_wateryearPPT), aspect = 1)
    k<- acfplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_wateryearPPT), aspect = 1)
    l<- acfplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), aspect = 1)
    grid.arrange(a,b,c,e,f,g,h,i,j,k,l)
    dev.off()
    
    
    
    
    ## standardized regression coef
    
    out <- as.matrix(jags.comb)
    summary(out)
    betas <- out[,grep(pattern = "beta",colnames(out))]
    B0 <- out[,"mu"]
    summary(as.mcmc(betas))
    apply(betas,2,summary)
    apply(betas,2,function(x){sum(x>0)/length(x)})
    save(out, jags.stuff, file="WaterYear.PPT.RData")
    
    png(height = 10, width = 12, units = "in", res = 200, paste0(workingdir, output.base.name,"_posterior_param_cors.png"))
    psych::pairs.panels(as.matrix(jags.comb[, c("deviance", "mu","tau_dbh", "tau_inc", "tau_add", colnames(betas))]), 
                        method = "pearson", # correlation method
                        hist.col = "#00AFBB",
                        density = TRUE,  # show density plots
                        ellipses = TRUE # show correlation ellipses
    )
    dev.off()
    
    png(height = 10, width = 12, units = "in", res = 200, paste0("workingdir", output.base.name,"_posterior_tau_cors.png"))
    psych::pairs.panels(as.matrix(jags.comb[, c("deviance", "mu","tau_dbh", "tau_inc", "tau_add","tau_PLOT")]), 
                        method = "pearson", # correlation method
                        hist.col = "#00AFBB",
                        density = TRUE,  # show density plots
                        ellipses = TRUE # show correlation ellipses
    )
    dev.off()
    
    # check to see if the plot randome effects are explained by any of the covariates:
    alphas <- out[,grep(pattern = "alpha",colnames(out))]
    alpha.m <- reshape2::melt(alphas)
    
    alpha.summary <- alpha.m %>% group_by(Var2) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                              ci.lo = quantile(value, 0.025, na.rm =TRUE), 
                                                              ci.hi = quantile(value, 0.975, na.rm =TRUE))
    
    PLOT.df <- data.frame(PLOT = unique(cov.data$PLOT), 
                          plt.num = 1:length(unique(cov.data$PLOT)))
    alpha.summary$plt.num <- 1:length(unique(cov.data$PLOT))
    cov.plots <- left_join(cov.data, PLOT.df)
    
    alpha.summary.plt <- left_join(alpha.summary, cov.plots)
    
    a <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= ELEV, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
      geom_point(data = alpha.summary.plt, aes(x= ELEV, y =mean))+theme_bw()+ylab(expression(alpha~Plot))
    
    b <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= SDI, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
      geom_point(data = alpha.summary.plt, aes(x= SDI, y =mean))+theme_bw()+stat_smooth(data = alpha.summary.plt, aes(x= SDI, y =mean), method = "lm")+ylab(expression(alpha~Plot))
    
    c <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= SICOND, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
      geom_point(data = alpha.summary.plt, aes(x= SICOND, y =mean))+theme_bw()+ylab(expression(alpha~Plot))
    
    d <-ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= SLOPE, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
      geom_point(data = alpha.summary.plt, aes(x= SLOPE, y =mean))+theme_bw()+ylab(expression(alpha~Plot))
    
    e <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= ASPECT, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
      geom_point(data = alpha.summary.plt, aes(x= ASPECT, y =mean))+theme_bw()+ylab(expression(alpha~Plot))
    
    f <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= TRTCD1, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
      geom_point(data = alpha.summary.plt, aes(x= TRTCD1, y =mean))+theme_bw()+ylab(expression(alpha~Plot))
    
    g <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= DSTRBCD1, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
      geom_point(data = alpha.summary.plt, aes(x= DSTRBCD1, y =mean))+theme_bw()+ylab(expression(alpha~Plot))
    
    h <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= STAGE2, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
      geom_point(data = alpha.summary.plt, aes(x= STAGE2, y =mean))+theme_bw()+stat_smooth(data = alpha.summary.plt, aes(x= STAGE2, y =mean), method = "lm")+ylab(expression(alpha~Plot))
    
    i<- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= STAGE3, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
      geom_point(data = alpha.summary.plt, aes(x= STAGE3, y =mean))+theme_bw()+ylab(expression(alpha~Plot))
    
    j<-ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= STDAGE, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
      geom_point(data = alpha.summary.plt, aes(x= STDAGE, y =mean))+theme_bw()+stat_smooth(data = alpha.summary.plt, aes(x= STDAGE, y =mean), method = "lm")+ylab(expression(alpha~Plot))
    
    k<-ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= MAP, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
      geom_point(data = alpha.summary.plt, aes(x= MAP, y =mean))+stat_smooth(data = alpha.summary.plt, aes(x= MAP, y =mean), method = "lm")+theme_bw()+ylab(expression(alpha~Plot))
    
    l<-ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= MAT, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
      geom_point(data = alpha.summary.plt, aes(x= MAT, y =mean))+stat_smooth(data = alpha.summary.plt, aes(x= MAT, y =mean), method = "lm")+theme_bw()+ylab(expression(alpha~Plot))
    
    
    #ggplot()+geom_point(data = alpha.summary.plt, aes(x= MAP, y = SDI, color =mean))+scale_color_gradient(low = "blue", high = "red")+theme_bw()+ylab(expression(alpha~Plot))
    
    png(height = 10, width = 8, units = "in", res = 200, paste0("/home/rstudio/",output.base.name,"_alpha_effects_by_envt.png"))
    grid.arrange(a,b,c,d,e,f,g,h,i,j,k,l)
    dev.off()
    
    # need to also look at climate variables:
    ## calculate an average tree
    clim.data <- readRDS("/home/rstudio/PRISM_non_scaled.rds")
    #clim.data <- readRDS("PRISM_non_scaled.rds")
    hist(z0)
    x <- mean(z0,na.rm = TRUE)-30
    Xhl <- quantile(z0,c(1/6,5/6),na.rm=TRUE)-30
    SDI <- mean(cov.data$SDI,na.rm = TRUE)
    SDIhl <- quantile(cov.data$SDI,c(1/6,5/6),na.rm = TRUE)
    SI <- mean(cov.data$SICOND, na.rm = TRUE)
    SIhl <- quantile(cov.data$SICOND,c(1/6,5/6),na.rm = TRUE)
    
    
    # select the climate variable of interest (for 1 climate var)
    
    wintP <- mean(time_data[[c(climate)]], na.rm = TRUE)
    wintPhl <- quantile(time_data[[c(climate)]],c(1/6,5/6),na.rm = TRUE)
    
    
    wateryrP <- mean(time_data$wintP.wateryr,na.rm = TRUE)
    wateryrPhl <- quantile(time_data$wintP.wateryr,c(1/6,5/6),na.rm = TRUE)
    
    sprfallTmax <- mean(time_data$tmax.fallspr,na.rm = TRUE)
    sprfallTmaxhl <- quantile(time_data$tmax.fallspr,c(1/6,5/6),na.rm = TRUE)
    sprfallTmaxreadlhl <- quantile(clim.data$tmax.fallspr,c(1/6,5/6),na.rm = TRUE)
    
    
    timesinceFire <- mean(time_data$TimeSinceFIRE,na.rm = TRUE)
    timesinceFirexhl <- quantile(time_data$TimeSinceFIRE,c(1/6,5/6),na.rm = TRUE)
    
    
    # standardize the beta coefficients:
    
    stdBeta <- rep(NA,11)
    stdBeta[1] <- mean(betas[,"betaSDI"])/SDI
    stdBeta[2] <- mean(betas[,"betaSDI_wintP.wateryr"])/SDI/wateryrP
    stdBeta[5] <- mean(betas[,"betaSICOND_wintP.wateryr"])/SI/wateryrP
    stdBeta[3] <- mean(betas[,"betaSICOND"])/SI
    stdBeta[4] <- mean(betas[,"betaSICOND_SDI"])/SI/SDI
    stdBeta[6] <- mean(betas[,"betaX"])/x
    stdBeta[7] <- mean(betas[,"betaX2"])/(x^2)
    stdBeta[8] <- mean(betas[,"betaX_SDI"])/x/SDI
    stdBeta[9] <- mean(betas[,"betaX_SICOND"])/x/SI
    stdBeta[10] <- mean(betas[,"betaX_wintP.wateryr"])/x/wateryrP
    stdBeta[11] <- mean(betas[,"betawintP.wateryr"])/wateryrP
    
    
    names(stdBeta) <- colnames(betas)
    format(stdBeta*10^6,scientific = FALSE)
    format(sort(abs(stdBeta*10^6),decreasing=TRUE),scientific = FALSE)
    
    
    # create plots of conditional effects 
    ## Size
    ns = 500 ## number of samples
    i = sample.int(nrow(betas),ns)
    xrng <- range(z0,na.rm = TRUE)
    xseq <- seq(xrng[1],xrng[2],by=1)
    xseq <- (1:58)-30
    incX <- matrix(NA,ns,length(xseq))
    
    betas <- out[,grep(pattern = "beta",colnames(out))]
    
    # this code just makes all the betas with SICOND as zero if they dont exist
    if("betaSICOND" %in% colnames(betas) ==FALSE){
      betas$betaSICOND <- 0
      betas$betaSICOND_tmax.fallspr <- 0
      betas$betaSICOND_wintP.wateryr <- 0
      betas$betaX_SICOND <- 0
    }
    
    if("betaX" %in% colnames(betas)==FALSE){
      betas$betaX <- 0
    }
    
    if("betaX_SDI" %in% colnames(betas)==FALSE){
      betas$betaX_SDI <- 0
    }
    # derive a mean value for betaX (this isnt really the true betaX, but will help)
    betas$derivedbetX <- rowMeans(betas[,5:294])
    if("derivedbetX" %in% colnames(betas)==TRUE){
      betas$betaX <- betas$derivedbetX
    }
    
    for(k in seq_along(i)){
      j <- i[k]
      incX[k,] <- B0[j]+ 
        betas[j,"betaX"]*xseq + 
        betas[j,"betaX2"]*(xseq)*xseq +
        betas[j,"betaSDI"]*SDI + 
        betas[j,"betaSDI_wintP.wateryr"]*SDI*wateryrP + 
        betas[j,"betaSICOND"]*SI + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wateryrP + 
        betas[j,"betaX_SICOND"]*xseq*SI+
        
        betas[j,"betaX_SDI"]*xseq*SDI  + 
        betas[j,"betaX_wintP.wateryr"]*xseq*wateryrP +
        betas[j,"betawintP.wateryr"]*wateryrP + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*xseq*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + 
        betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP # +
    }
    
    CIX <- apply(incX, 2, quantile,c(0.025,0.5,0.975)) # get CI on X
    
    source("visualization/R/ciEnvelope.R") # read in ci.envelope function
    
    
    # plot as pseudo object to save for later
    Tree.Size.Effect %<a-% {
      plot(xseq+30,CIX[2,],ylim=c(0,max(CIX)),type='n',ylab="Diameter Increment (cm)",xlab="DBH (cm)",cex.lab=1.5)
      ciEnvelope(xseq+30,CIX[1,],CIX[3,],col = "lightgrey")
      lines(xseq+30,CIX[2,],lwd=2)
      abline(h=0)
    }
    
    TreeSize.mid <- reshape2::melt(CIX) %>% spread(Var1, value)%>% add_column(size = "Medium", Size = xseq+30)
    Size.Effect.gg <- ggplot()+geom_ribbon(data = TreeSize.mid, aes(Size, ymin = `2.5%`, ymax = `97.5%`), alpha = 0.5)+
      geom_line(data = TreeSize.mid, aes(Size, y = `50%`))+ylab("Predicted Growth")+xlab("Tree Diameter (cm)")+theme_bw()+
      theme(panel.grid = element_blank())
    
    
    
    ##SDI
    hist(cov.data$SDI)
    
    # KH note: need to standardize SDIseq first since I ran the model with standardized covariates
    SDIseq.real <- seq(0,400,by=10)
    SDIseq <- (SDIseq.real-mean(temp2$SDI))/sd(temp2$SDI)
    
    incSDI <- matrix(NA,ns,length(SDIseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      
      incSDI[k,]  <- B0[j]+ 
        betas[j,"betaX"]*x + 
        betas[j,"betaX2"]*(x)*x +
        betas[j,"betaSDI"]*SDIseq + 
        betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wateryrP + 
        betas[j,"betaSICOND"]*SI + 
        #betas[j,"betaSICOND_SDIseq"]*SDIseq*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wateryrP + 
        betas[j,"betaX_SICOND"]*x*SI+
        betas[j,"betaX_SDI"]*x*SDIseq  + 
        betas[j,"betaX_wintP.wateryr"]*x*wateryrP +
        betas[j,"betawintP.wateryr"]*wateryrP + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + 
        betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP
      
    }
    CIsdi <- apply(incSDI,2,quantile,c(0.025,0.5,0.975))
    
    SDI.Effect %<a-% {
      plot(
        SDIseq.real,
        CIsdi[2, ],
        ylim = c(0, max(CIsdi)),
        type = 'n',
        xlab = "Stand Density Index",
        ylab = "Diameter Increment (cm)",
        cex.lab = 1.5)
      ciEnvelope(SDIseq.real, CIsdi[1, ], CIsdi[3, ], col = "lightgrey")
      lines(SDIseq.real, CIsdi[2, ], lwd = 2)
      abline(h = 0)
    }
    
    sdi.mid <- reshape2::melt(CIsdi) %>% spread(Var1, value)%>% add_column(size = "Medium", SDI = SDIseq.real)
    SDI.Effect.gg <- ggplot()+geom_ribbon(data = sdi.mid, aes(SDI, ymin = `2.5%`, ymax = `97.5%`), alpha = 0.5)+
      geom_line(data = sdi.mid, aes(SDI, y = `50%`))+ylab("Predicted Growth")+xlab("Stand Density Index")+theme_bw()+
      theme(panel.grid = element_blank())
    
    
    ## SDI * size
    incSDIXhi <- matrix(NA,ns,length(SDIseq))
    incSDIXlo <- matrix(NA,ns,length(SDIseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incSDIXlo[k,] <- B0[j]+ 
        betas[j,"betaX"]*Xhl[1] + 
        betas[j,"betaX2"]*(Xhl[1])*Xhl[1] +
        betas[j,"betaSDI"]*SDIseq + 
        betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wateryrP + 
        betas[j,"betaSICOND"]*SI + 
        #betas[j,"betaSICOND_SDIseq"]*SDIseq*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wateryrP + 
        betas[j,"betaX_SICOND"]*Xhl[1]*SI+
        betas[j,"betaX_SDI"]*Xhl[1]*SDIseq  + 
        betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wateryrP +
        betas[j,"betawintP.wateryr"]*wateryrP + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + 
        betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP 
      
      incSDIXhi[k,] <- B0[j]+ 
        betas[j,"betaX"]*Xhl[2] + 
        betas[j,"betaX2"]*(Xhl[2])*Xhl[2] +
        betas[j,"betaSDI"]*SDIseq + 
        betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wateryrP + 
        betas[j,"betaSICOND"]*SI + 
        #betas[j,"betaSICOND_SDIseq"]*SDIseq*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wateryrP + 
        betas[j,"betaX_SICOND"]*Xhl[2]*SI+
        betas[j,"betaX_SDI"]*Xhl[2]*SDIseq  + 
        betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wateryrP +
        betas[j,"betawintP.wateryr"]*wateryrP + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + 
        betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP 
    }
    
    
    CIsdiXlo <- apply(incSDIXlo,2,quantile,c(0.025,0.5,0.975))
    CIsdiXhi <- apply(incSDIXhi,2,quantile,c(0.025,0.5,0.975))
    
    # SDI plot
    SDI.DBH.Effect %<a-% {
      plot(SDIseq.real,CIsdi[2,],ylim=c(0,max(CIsdi[,1]+0.05)),type='l',xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5,lwd=3)
      ciEnvelope(SDIseq.real,CIsdi[1,],CIsdi[3,],col = "lightgrey")
      ciEnvelope(SDIseq.real,CIsdiXlo[1,],CIsdiXlo[3,],col = "lightblue")
      ciEnvelope(SDIseq.real,CIsdiXhi[1,],CIsdiXhi[3,],col = "pink")
      lines(SDIseq.real,CIsdiXlo[2,],lwd=3,col="blue")
      lines(SDIseq.real,CIsdiXhi[2,],lwd=3,col="red")
      lines(SDIseq.real,CIsdi[2,],lwd=3,col="black")
      legend("topright",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
      abline(h=0)
    }
    
    
    
    # same but ggplot and prettier:
    sdi.low <- reshape2::melt(CIsdiXlo) %>% spread(Var1, value) %>% add_column(size = "Small", SDI = SDIseq.real)
    sdi.high <- reshape2::melt(CIsdiXhi) %>% spread(Var1, value) %>% add_column(size = "Large", SDI = SDIseq.real)
    sdi.mid <- reshape2::melt(CIsdi) %>% spread(Var1, value)%>% add_column(size = "Medium", SDI = SDIseq.real)
    
    all.sdi <- rbind(sdi.low, sdi.high, sdi.mid)
    
    SDI.DBH.Effect.gg <- ggplot()+geom_ribbon(data = all.sdi, aes(SDI, ymin = `2.5%`, ymax = `97.5%`, fill = size), alpha = 0.5)+
      geom_line(data = all.sdi, aes(SDI, y = `50%`, color = size))+ylab("Predicted Growth")+xlab("Stand Density Index")+theme_bw()+
      theme(panel.grid = element_blank())
    
    ##SI
    # hist(cov.data$SI)
    # # KH note: need to standardize SDIseq first since I ran the model with standardized covariates
    SIseq.real <- 20:60
    SIseq <- (SIseq.real-mean(temp2$COND_SICOND))/sd(temp2$COND_SICOND)
    #SIseq <- 20:60
    incSI <- matrix(NA,ns,length(SDIseq))
    #
    for(k in seq_along(i)){
      j <- i[k]
      incSI[k,] <-  B0[j]+ 
        betas[j,"betaX"]*x + 
        betas[j,"betaX2"]*(x)*x +
        betas[j,"betaSDI"]*SDI + 
        betas[j,"betaSDI_wintP.wateryr"]*SDI*wateryrP + 
        betas[j,"betaSICOND"]*SIseq + 
        #betas[j,"betaSICOND_SDI"]*SDI*SIseq +  
        betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax + 
        betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wateryrP + 
        betas[j,"betaX_SICOND"]*x*SIseq+
        betas[j,"betaX_SDI"]*x*SDI  + 
        betas[j,"betaX_wintP.wateryr"]*x*wateryrP +
        betas[j,"betawintP.wateryr"]*wateryrP + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + 
        betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP 
      
    }
    CIsi <- apply(incSI,2,quantile,c(0.025,0.5,0.975))
    #
    SI.Effect %<a-% {
      plot(SIseq.real,CIsi[2,],ylim=c(0,max(CIsi)),type='n',xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
      ciEnvelope(SIseq.real,CIsi[1,],CIsi[3,],col = "lightgrey")
      lines(SIseq.real,CIsi[2,],lwd=2)
      abline(h=0)
    }
    
    
    si.mid <- reshape2::melt(CIsi) %>% spread(Var1, value)%>% add_column(size = "Medium", SI = SIseq.real)
    SI.Effect.gg <- ggplot()+geom_ribbon(data = si.mid, aes(SI, ymin = `2.5%`, ymax = `97.5%`), alpha = 0.5)+
      geom_line(data = si.mid, aes(SI, y = `50%`))+ylab("Predicted Growth")+xlab("Site Index")+theme_bw()+
      theme(panel.grid = element_blank())
    
    #
    # ## SI x DBH
    incSIXlo <- matrix(NA,ns,length(SDIseq))
    incSIXmed <- matrix(NA,ns,length(SDIseq))
    incSIXhi <- matrix(NA,ns,length(SDIseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incSIXlo[k,] <- B0[j]+ 
        betas[j,"betaX"]*Xhl[1] + 
        betas[j,"betaX2"]*(Xhl[1])*Xhl[1] +
        betas[j,"betaSDI"]*SDI + 
        betas[j,"betaSDI_wintP.wateryr"]*SDI*wateryrP + 
        betas[j,"betaSICOND"]*SIseq + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax + 
        betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wateryrP + 
        betas[j,"betaX_SICOND"]*Xhl[1]*SIseq+
        betas[j,"betaX_SDI"]*Xhl[1]*SDI  + 
        betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wateryrP +
        betas[j,"betawintP.wateryr"]*wateryrP + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + 
        betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP 
      
      
      incSIXhi[k,] <- B0[j]+ 
        betas[j,"betaX"]*Xhl[2] + 
        betas[j,"betaX2"]*(Xhl[2])*Xhl[2] +
        betas[j,"betaSDI"]*SDI + 
        betas[j,"betaSDI_wintP.wateryr"]*SDI*wateryrP + 
        betas[j,"betaSICOND"]*SIseq + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax + 
        betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wateryrP + 
        betas[j,"betaX_SICOND"]*Xhl[2]*SIseq+
        betas[j,"betaX_SDI"]*Xhl[2]*SDI  + 
        betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wateryrP +
        betas[j,"betawintP.wateryr"]*wateryrP + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + 
        betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP 
      
      incSIXmed[k,] <-B0[j]+ 
        betas[j,"betaX"]*x + 
        betas[j,"betaX2"]*(x)*x +
        betas[j,"betaSDI"]*SDI + 
        betas[j,"betaSDI_wintP.wateryr"]*SDI*wateryrP + 
        betas[j,"betaSICOND"]*SIseq + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax + 
        betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wateryrP + 
        betas[j,"betaX_SICOND"]*x*SIseq+
        betas[j,"betaX_SDI"]*x*SDI  + 
        betas[j,"betaX_wintP.wateryr"]*x*wateryrP +
        betas[j,"betawintP.wateryr"]*wateryrP + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + 
        betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP 
    }
    
    CIsiXl <- apply(incSIXlo,2,quantile,c(0.025,0.5,0.975))
    CIsiXh <- apply(incSIXhi,2,quantile,c(0.025,0.5,0.975))
    CIsiXm <- apply(incSIXmed,2,quantile,c(0.025,0.5,0.975))
    
    SI.DBH.Effect %<a-% {
      plot(SIseq.real,CIsiXm[2,],ylim=c(0,max(CIsi)),type='l',lwd=3,xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
      #ciEnvelope(SIseq,CIsi[1,],CIsi[3,],col = "lightgrey")
      lines(SIseq.real,CIsiXl[2,],lwd=3,col="blue")
      lines(SIseq.real,CIsiXh[2,],lwd=3,col="red")
      legend("bottomright",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3,cex=1)
    }
    
    
    # same but ggplot and prettier:
    si.low <- reshape2::melt(CIsiXl) %>% spread(Var1, value) %>% add_column(size = "Small", si = SIseq.real)
    si.high <- reshape2::melt(CIsiXh) %>% spread(Var1, value) %>% add_column(size = "Large", si = SIseq.real)
    si.mid <- reshape2::melt(CIsiXm) %>% spread(Var1, value)%>% add_column(size = "Medium", si = SIseq.real)
    
    all.si <- rbind(si.low, si.high, si.mid)
    
    si.DBH.Effect.gg <- ggplot()+geom_ribbon(data = all.si, aes(si, ymin = `2.5%`, ymax = `97.5%`, fill = size), alpha = 0.5)+
      geom_line(data = all.si, aes(si, y = `50%`, color = size))+ylab("Predicted Growth")+xlab("Site Index")+theme_bw()+
      theme(panel.grid = element_blank())
    
    ## wintP
    clim.data <- readRDS("PRISM_non_scaled.rds")
    hist(time_data$wintP.wateryr)
    wintPseq.real <- 0:800
    wintPseq <- (wintPseq.real-mean(as.matrix(clim.data$wintP.wateryr)))/sd(as.matrix(clim.data$wintP.wateryr))
    incP <- matrix(NA,ns,length(wintPseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      
      incP[k,] <- B0[j]+ 
        betas[j,"betaX"]*x + 
        betas[j,"betaX2"]*(x)*x +
        betas[j,"betaSDI"]*SDI + 
        betas[j,"betaSDI_wintP.wateryr"]*SDI* wintPseq + 
        betas[j,"betaSICOND"]*SI + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI* wintPseq + 
        betas[j,"betaX_SICOND"]*x*SI+
        betas[j,"betaX_SDI"]*x*SDI  + 
        betas[j,"betaX_wintP.wateryr"]*x* wintPseq +
        betas[j,"betawintP.wateryr"]* wintPseq + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + 
        betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq
      
    }
    
    CIwintP <- apply(incP,2,quantile,c(0.025,0.5,0.975))
    
    Climate.Effect %<a-% {
      plot(wintPseq.real, CIwintP[2,],ylim=c(0,max(CIwintP + 0.05)),type='n',xlab="Water Year Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
      ciEnvelope(wintPseq.real,CIwintP[1,], CIwintP[3,],col = "lightgrey")
      lines(wintPseq.real, CIwintP[2,],lwd=2)
      abline(h=0)
    }
    
    wintP.mid <- reshape2::melt(CIwintP) %>% spread(Var1, value)%>% add_column(wintPze = "Medium", wintP = wintPseq.real)
    wintP.Effect.gg <- ggplot()+geom_ribbon(data = wintP.mid, aes(wintP, ymin = `2.5%`, ymax = `97.5%`), alpha = 0.5)+
      geom_line(data = wintP.mid, aes(wintP, y = `50%`))+ylab("Predicted Growth")+xlab("Total Water Year Precipitation")+theme_bw()+
      theme(panel.grid = element_blank())
    
    
    ## PRECIP x SDI
    incPSDIlo <- matrix(NA,ns,length(wintPseq))
    incPSDIhi <- matrix(NA,ns,length(wintPseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incPSDIlo[k,] <-   B0[j]+ 
        betas[j,"betaX"]*x + 
        betas[j,"betaX2"]*(x)*x +
        betas[j,"betaSDI"]*SDIhl[1] + 
        betas[j,"betaSDI_wintP.wateryr"]*SDIhl[1]* wintPseq + 
        betas[j,"betaSICOND"]*SI + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI* wintPseq + 
        betas[j,"betaX_SICOND"]*x*SI+
        betas[j,"betaX_SDI"]*x*SDIhl[1]  + 
        betas[j,"betaX_wintP.wateryr"]*x* wintPseq +
        betas[j,"betawintP.wateryr"]* wintPseq + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + 
        betas[j,"betaSDI_tmax.fallspr"]*SDIhl[1]*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq
      
      incPSDIhi[k,] <-  B0[j]+ 
        betas[j,"betaX"]*x + 
        betas[j,"betaX2"]*(x)*x +
        betas[j,"betaSDI"]*SDIhl[2] + 
        betas[j,"betaSDI_wintP.wateryr"]*SDIhl[2]* wintPseq + 
        betas[j,"betaSICOND"]*SI + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI* wintPseq + 
        betas[j,"betaX_SICOND"]*x*SI+
        betas[j,"betaX_SDI"]*x*SDIhl[2]  + 
        betas[j,"betaX_wintP.wateryr"]*x* wintPseq +
        betas[j,"betawintP.wateryr"]* wintPseq + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + 
        betas[j,"betaSDI_tmax.fallspr"]*SDIhl[2]*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq
      
    }
    
    CIwintPSDIlo <- apply(incPSDIlo,2,quantile,c(0.025,0.5,0.975))
    CIwintPSDIhi <- apply(incPSDIhi,2,quantile,c(0.025,0.5,0.975))
    
    Climate.SDI.Effect %<a-% {
      plot(wintPseq.real,CIwintP[2,],ylim=c(0,max(CIwintPSDIlo[2,])),type='l',lwd=3,xlab="Water Year Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
      #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
      lines(wintPseq.real,CIwintPSDIlo[2,],lwd=2,col="blue")
      lines(wintPseq.real,CIwintPSDIhi[2,],lwd=2,col="red")
      legend("bottomleft",legend=c("low SDI","mean SDI","high SDI"),col=c("blue",1,2),lwd=3)
    }
    
    # same but ggplot and prettier:
    sdi.P.low <- reshape2::melt(CIwintPSDIlo) %>% spread(Var1, value) %>% add_column(sdi = "Low", precip = wintPseq.real)
    sdi.P.high <- reshape2::melt( CIwintPSDIhi) %>% spread(Var1, value) %>% add_column(sdi = "High", precip = wintPseq.real)
    sdi.P.mid <- reshape2::melt(CIwintP) %>% spread(Var1, value)%>% add_column(sdi = "Medium", precip = wintPseq.real)
    
    all.sdi.P <- rbind(sdi.P.low, sdi.P.high, sdi.P.mid)
    
    sdi.Precip.Effect.gg <- ggplot()+geom_ribbon(data =all.sdi.P, aes(precip, ymin = `2.5%`, ymax = `97.5%`, fill = sdi), alpha = 0.5)+
      geom_line(data = all.sdi.P, aes(precip, y = `50%`, color = sdi))+ylab("Predicted Growth")+xlab("Site Index")+theme_bw()+
      theme(panel.grid = element_blank())
    
    
    
    ## PRECIP x DBH
    incPXlo <- matrix(NA,ns,length(wintPseq))
    incPXhi <- matrix(NA,ns,length(wintPseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incPXlo[k,] <-   B0[j]+ 
        betas[j,"betaX"]*Xhl[1] + 
        betas[j,"betaX2"]*(Xhl[1])*Xhl[1] +
        betas[j,"betaSDI"]*SDI + 
        betas[j,"betaSDI_wintP.wateryr"]*SDI* wintPseq + 
        betas[j,"betaSICOND"]*SI + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI* wintPseq + 
        betas[j,"betaX_SICOND"]*Xhl[1]*SI+
        betas[j,"betaX_SDI"]*Xhl[1]*SDI  + 
        betas[j,"betaX_wintP.wateryr"]*Xhl[1]* wintPseq +
        betas[j,"betawintP.wateryr"]* wintPseq + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + 
        betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq
      
      incPXhi[k,] <-  B0[j]+ 
        betas[j,"betaX"]*Xhl[2] + 
        betas[j,"betaX2"]*(Xhl[2])*Xhl[2] +
        betas[j,"betaSDI"]*SDI + 
        betas[j,"betaSDI_wintP.wateryr"]*SDI* wintPseq + 
        betas[j,"betaSICOND"]*SI + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI* wintPseq + 
        betas[j,"betaX_SICOND"]*Xhl[2]*SI+
        betas[j,"betaX_SDI"]*Xhl[2]*SDI  + 
        betas[j,"betaX_wintP.wateryr"]*Xhl[2]* wintPseq +
        betas[j,"betawintP.wateryr"]* wintPseq + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + 
        betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq
    }
    
    CIwintPXlo <- apply(incPXlo,2,quantile,c(0.025,0.5,0.975))
    CIwintPXhi <- apply(incPXhi,2,quantile,c(0.025,0.5,0.975))
    
    Climate.DBH.Effect %<a-% {
      plot(wintPseq.real,CIwintP[2,],ylim=c(0,max(CIwintP)+0.1),type='l',lwd=3,xlab="Water Year Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
      #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
      lines(wintPseq.real,CIwintPXlo[2,],lwd=2,col="blue")
      lines(wintPseq.real,CIwintPXhi[2,],lwd=2,col="red")
      legend("bottomright",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
    }
    
    # same but ggplot and prettier:
    P.x.low <- reshape2::melt(CIwintPXhi) %>% spread(Var1, value) %>% add_column(size = "Small", precip = wintPseq.real)
    P.x.high <- reshape2::melt(CIwintPXlo) %>% spread(Var1, value) %>% add_column(size = "Large", precip = wintPseq.real)
    P.x.mid <- reshape2::melt(CIwintP) %>% spread(Var1, value)%>% add_column(size = "Medium", precip = wintPseq.real)
    
    all.size.P <- rbind(P.x.low, P.x.high, P.x.mid)
    
    Precipitation.DBH.Effect.gg <- ggplot()+geom_ribbon(data =all.size.P, aes(precip, ymin = `2.5%`, ymax = `97.5%`, fill = size), alpha = 0.5)+
      geom_line(data = all.size.P, aes(precip, y = `50%`, color = size))+ylab("Predicted Growth")+xlab("Water Year Precipitation")+theme_bw()+
      theme(panel.grid = element_blank())
    
    
    
    # ## Precip X SI
    incP_SIlo <- matrix(NA,ns,length(wintPseq))
    incP_SIhi <- matrix(NA,ns,length(wintPseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incP_SIlo[k,] <- B0[j]+ 
        betas[j,"betaX"]*x + 
        betas[j,"betaX2"]*(x)*x +
        betas[j,"betaSDI"]*SDI + 
        betas[j,"betaSDI_wintP.wateryr"]*SDI* wintPseq + 
        betas[j,"betaSICOND"]*SIhl[1] + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SIhl[1]*sprfallTmax + 
        betas[j,"betaSICOND_wintP.wateryr"]*SIhl[1]* wintPseq + 
        betas[j,"betaX_SICOND"]*x*SIhl[1]+
        betas[j,"betaX_SDI"]*x*SDI  + 
        betas[j,"betaX_wintP.wateryr"]*x* wintPseq +
        betas[j,"betawintP.wateryr"]* wintPseq + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + 
        betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq
      
      
      incP_SIhi[k,] <- B0[j]+ 
        betas[j,"betaX"]*x + 
        betas[j,"betaX2"]*(x)*x +
        betas[j,"betaSDI"]*SDI + 
        betas[j,"betaSDI_wintP.wateryr"]*SDI* wintPseq + 
        betas[j,"betaSICOND"]*SIhl[2] + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SIhl[2]*sprfallTmax + 
        betas[j,"betaSICOND_wintP.wateryr"]*SIhl[2]* wintPseq + 
        betas[j,"betaX_SICOND"]*x*SIhl[2]+
        betas[j,"betaX_SDI"]*x*SDI  + 
        betas[j,"betaX_wintP.wateryr"]*x* wintPseq +
        betas[j,"betawintP.wateryr"]* wintPseq + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + 
        betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq
      
    }
    
    CIwintPSIlo <- apply(incP_SIlo,2,quantile,c(0.025,0.5,0.975))
    CIwintPSIhi <- apply(incP_SIhi,2,quantile,c(0.025,0.5,0.975))
    
    Climate.SI.Effect %<a-% {
      plot(wintPseq.real,CIwintPSIhi[2,],ylim=c(0,max(CIwintPSIhi[2,])),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
      #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
      lines(wintPseq.real,CIwintPSIlo[2,],lwd=2,col="grey")
      lines(wintPseq.real,CIwintPSIhi[2,],lwd=2,col="purple")
      legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
    }
    
    # same but ggplot and prettier:
    P.si.low <- reshape2::melt(CIwintPSIlo) %>% spread(Var1, value) %>% add_column(si = "Low", precip = wintPseq.real)
    P.si.high <- reshape2::melt(CIwintPSIhi) %>% spread(Var1, value) %>% add_column(si = "High", precip = wintPseq.real)
    P.si.mid <- reshape2::melt(CIwintP) %>% spread(Var1, value)%>% add_column(si = "Medium", precip = wintPseq.real)
    
    all.si.P <- rbind(P.si.low, P.si.high, P.si.mid)
    
    Precipitation.SI.Effect.gg <- ggplot()+geom_ribbon(data =all.si.P, aes(precip, ymin = `2.5%`, ymax = `97.5%`, fill = si), alpha = 0.5)+
      geom_line(data = all.si.P, aes(precip, y = `50%`, color = si))+ylab("Predicted Growth")+xlab("Water Year Precipitation")+theme_bw()+
      theme(panel.grid = element_blank())
    # 
    # 
    # 
    # ## SI X SDI
    # 
    # incSDI_SIlo <- matrix(NA,ns,length(SDIseq))
    # incSDI_SIhi <- matrix(NA,ns,length(SDIseq))
    # 
    # for(k in seq_along(i)){
    #   j <- i[k]
    #   incSDI_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    #     betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[1] + 
    #     betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x[1] + 
    #     betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintP +
    #     betas[j,"betawintP.wateryr"]*wintP+
    #     # temperature
    #     betas[j,"betaSICOND_tmax.fallspr"]*SIhl[1]*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*x[1]*sprfallTmax +
    #     betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax +
    #     betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintP +
    #     # time since fire
    #     betas[j,"betaSICOND_TimeSinceFIRE"]*SIhl[1]*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*x[1]*timesinceFire +
    #     betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDIseq*timesinceFire +
    #     betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmax
    #   
    #   
    #   incSDI_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    #     betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[2] + 
    #     betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x[1] + 
    #     betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintP +
    #     betas[j,"betawintP.wateryr"]*wintP+
    #     # temperature
    #     betas[j,"betaSICOND_tmax.fallspr"]*SIhl[2]*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*x[1]*sprfallTmax +
    #     betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax +
    #     betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintP +
    #     # time since fire
    #     betas[j,"betaSICOND_TimeSinceFIRE"]*SIhl[2]*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*x[1]*timesinceFire +
    #     betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDIseq*timesinceFire +
    #     betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmax
    #   
    # }
    # CISDISIlo <- apply(incSDI_SIlo,2,quantile,c(0.025,0.5,0.975))
    # CISDISIhi <- apply(incSDI_SIhi,2,quantile,c(0.025,0.5,0.975))
    # 
    # SDI.SI.Effect %<a-% {
    #   plot(SDIseq.real,CISDISIhi[2,],ylim=c(0,max(CISDISIhi[2,])),type='l',lwd=3,xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5, col = "purple")
    #   # #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "purple")
    #   lines(SDIseq.real,CISDISIlo[2,],lwd=2,col="grey")
    #   #lines(wintPseq.real,CIwintPSIhi[2,],lwd=2,col="purple")
    #   legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
    # }
    # 
    
    # Temperature effect
    
    
    #clim.data <- readRDS("PRISM_non_scaled.rds")
    hist(time_data$tmax.fallspr)
    range(clim.data$tmax.fallspr)
    tmaxseq.real <- 17:31
    tmaxseq <- (tmaxseq.real-mean(as.matrix(clim.data$tmax.fallspr)))/sd(as.matrix(clim.data$tmax.fallspr))
    incT <- matrix(NA,ns,length(tmaxseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      
      incT[k,] <- B0[j]+ 
        betas[j,"betaX"]*x + 
        betas[j,"betaX2"]*(x)*x +
        betas[j,"betaSDI"]*SDI + 
        betas[j,"betaSDI_wintP.wateryr"]*SDI* wintP + 
        betas[j,"betaSICOND"]*SI + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SI*tmaxseq + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + 
        betas[j,"betaX_SICOND"]*x*SI+
        betas[j,"betaX_SDI"]*x*SDI  + 
        betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*x*tmaxseq +
        betas[j,"betatmax.fallspr"]*tmaxseq + 
        betas[j,"betaSDI_tmax.fallspr"]*SDI*tmaxseq +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*tmaxseq*wintP
    }
    
    CItmax <- apply(incT,2,quantile,c(0.025,0.5,0.975))
    
    Tmax.Effect %<a-% {
      plot(tmaxseq.real, CItmax[2,],ylim=c(0,max(CIwintP + 0.05)),type='n',xlab="Tmax fall spr (DegC)",ylab="Diameter Increment (cm)",cex.lab=1.5)
      ciEnvelope(tmaxseq.real,CItmax[1,], CItmax[3,],col = "lightgrey")
      lines(tmaxseq.real, CItmax[2,],lwd=2)
      abline(h=0)
    }
    
    tmax.mid <- reshape2::melt(CItmax) %>% spread(Var1, value)%>% add_column(size = "Medium", Tmax = tmaxseq.real)
    tmax.Effect.gg <- ggplot()+geom_ribbon(data = tmax.mid, aes(Tmax, ymin = `2.5%`, ymax = `97.5%`), alpha = 0.5)+
      geom_line(data = tmax.mid, aes(Tmax, y = `50%`))+ylab("Predicted Growth")+xlab("Fall-Spring Tmax (DegC)")+theme_bw()+
      theme(panel.grid = element_blank())
    
    
    
    
    
    # Temperature x SDI
    
    incTemplo <- matrix(NA,ns,length(tmaxseq))
    incTemphi <- matrix(NA,ns,length(tmaxseq))
    for(k in seq_along(i)){
      j <- i[k]
      
      incTemplo[k,] <-  B0[j]+ 
        betas[j,"betaX"]*x + 
        betas[j,"betaX2"]*(x)*x +
        betas[j,"betaSDI"]*SDIhl[1] + 
        betas[j,"betaSDI_wintP.wateryr"]*SDIhl[1]* wintP + 
        betas[j,"betaSICOND"]*SI + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SI*tmaxseq + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + 
        betas[j,"betaX_SICOND"]*x*SI+
        betas[j,"betaX_SDI"]*x*SDIhl[1]  + 
        betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*x*tmaxseq +
        betas[j,"betatmax.fallspr"]*tmaxseq + 
        betas[j,"betaSDI_tmax.fallspr"]*SDIhl[1]*tmaxseq +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*tmaxseq*wintP
      
      
      incTemphi[k,] <- B0[j]+ 
        betas[j,"betaX"]*x + 
        betas[j,"betaX2"]*(x)*x +
        betas[j,"betaSDI"]*SDIhl[2] + 
        betas[j,"betaSDI_wintP.wateryr"]*SDIhl[2]* wintP + 
        betas[j,"betaSICOND"]*SI + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SI*tmaxseq + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + 
        betas[j,"betaX_SICOND"]*x*SI+
        betas[j,"betaX_SDI"]*x*SDIhl[2]  + 
        betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*x*tmaxseq +
        betas[j,"betatmax.fallspr"]*tmaxseq + 
        betas[j,"betaSDI_tmax.fallspr"]*SDIhl[2]*tmaxseq +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*tmaxseq*wintP
      
    }
    
    CItmaxlo <- apply(incTemplo,2,quantile,c(0.025,0.5,0.975))
    CItmaxhi <- apply(incTemphi,2,quantile,c(0.025,0.5,0.975))
    
    Tmax.SDI.Effect %<a-% {
      plot(tmaxseq.real,CItmax[2,],ylim=c(0,max(CItmaxlo[2,])),type='l',lwd=3,xlab="Fall Spring Tmax (degC)",ylab="Diameter Increment (cm)",cex.lab=1.5)
      #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
      lines(tmaxseq.real,CItmaxlo[2,],lwd=2,col="blue")
      lines(tmaxseq.real,CItmaxhi[2,],lwd=2,col="red")
      legend("bottomleft",legend=c("low SDI","mean SDI","high SDI"),col=c("blue",1,2),lwd=3)
    }
    
    # make ggplot version
    sdi.T.low <- reshape2::melt(CItmaxlo) %>% spread(Var1, value) %>% add_column(sdi = "Low", tmax = tmaxseq.real)
    sdi.T.high <- reshape2::melt( CItmaxhi) %>% spread(Var1, value) %>% add_column(sdi = "High", tmax = tmaxseq.real)
    sdi.T.mid <- reshape2::melt(CItmax) %>% spread(Var1, value)%>% add_column(sdi = "Medium", tmax = tmaxseq.real)
    
    all.sdi.T <- rbind(sdi.T.low, sdi.T.high, sdi.T.mid)
    
    tmax.SDI.Effect.gg <- ggplot()+geom_ribbon(data =all.sdi.T, aes(tmax, ymin = `2.5%`, ymax = `97.5%`, fill = sdi), alpha = 0.5)+
      geom_line(data = all.sdi.T, aes(tmax, y = `50%`, color = sdi))+ylab("Predicted Growth")+xlab("Fall - Spring Tmax (DegC)")+theme_bw()+
      theme(panel.grid = element_blank())
    
    
    
    
    ## Tmax x DBH
    incTXlo <- matrix(NA,ns,length(tmaxseq))
    incTXhi <- matrix(NA,ns,length(tmaxseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      
      incTXlo[k,] <-B0[j]+ 
        betas[j,"betaX"]*Xhl[1] + 
        betas[j,"betaX2"]*(Xhl[1])*Xhl[1] +
        betas[j,"betaSDI"]*SDI + 
        betas[j,"betaSDI_wintP.wateryr"]*SDI* wintP + 
        betas[j,"betaSICOND"]*SI + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SI*tmaxseq + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + 
        betas[j,"betaX_SICOND"]*Xhl[1]*SI+
        betas[j,"betaX_SDI"]*Xhl[1]*SDI  + 
        betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
        betas[j,"betawintP.wateryr"]*wintP + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*Xhl[1]*tmaxseq +
        betas[j,"betatmax.fallspr"]*tmaxseq + 
        betas[j,"betaSDI_tmax.fallspr"]*SDI*tmaxseq +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*tmaxseq*wintP
      
      incTXhi[k,] <-B0[j]+ 
        betas[j,"betaX"]*Xhl[2] + 
        betas[j,"betaX2"]*(Xhl[2])*Xhl[2] +
        betas[j,"betaSDI"]*SDI + 
        betas[j,"betaSDI_wintP.wateryr"]*SDI* wintP + 
        betas[j,"betaSICOND"]*SI + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SI*tmaxseq + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + 
        betas[j,"betaX_SICOND"]*Xhl[2]*SI+
        betas[j,"betaX_SDI"]*Xhl[2]*SDI  + 
        betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
        betas[j,"betawintP.wateryr"]*wintP + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*Xhl[2]*tmaxseq +
        betas[j,"betatmax.fallspr"]*tmaxseq + 
        betas[j,"betaSDI_tmax.fallspr"]*SDI*tmaxseq +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*tmaxseq*wintP
    }
    
    CItmaxXlo <- apply(incTXlo,2,quantile,c(0.025,0.5,0.975))
    CItmaxXhi <- apply(incTXhi,2,quantile,c(0.025,0.5,0.975))
    
    Tmax.DBH.Effect %<a-% {
      plot(tmaxseq.real,CItmax[2,],ylim=c(0,max(CItmax)+0.1),type='l',lwd=3,xlab="Fall-Spr Tmax (DegC)",ylab="Diameter Increment (cm)",cex.lab=1.5)
      #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
      lines(tmaxseq.real,CItmaxXlo[2,],lwd=2,col="blue")
      lines(tmaxseq.real,CItmaxXhi[2,],lwd=2,col="red")
      legend("bottomright",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
    }
    
    # same but ggplot and prettier:
    T.x.low <- reshape2::melt(CItmaxXlo) %>% spread(Var1, value) %>% add_column(size = "Small", tmax = tmaxseq.real)
    T.x.high <- reshape2::melt(CItmaxXhi) %>% spread(Var1, value) %>% add_column(size = "Large", tmax = tmaxseq.real)
    T.x.mid <- reshape2::melt(CItmax) %>% spread(Var1, value)%>% add_column(size = "Medium", tmax = tmaxseq.real)
    
    all.size.T <- rbind(T.x.low, T.x.high, T.x.mid)
    
    Tmax.DBH.Effect.gg <- ggplot()+geom_ribbon(data =all.size.T, aes(tmax, ymin = `2.5%`, ymax = `97.5%`, fill = size), alpha = 0.5)+
      geom_line(data = all.size.T, aes(tmax, y = `50%`, color = size))+ylab("Predicted Growth")+xlab("Fall - Spring Tmax (DegC)")+theme_bw()+
      theme(panel.grid = element_blank())
    
    # ## Tmax X SI
    # # temperature x SICOND
    incT_SIlo <- matrix(NA,ns,length(tmaxseq))
    incT_SIhi <- matrix(NA,ns,length(tmaxseq))
    # 
    for(k in seq_along(i)){
      j <- i[k]
      incT_SIlo[k,] <- B0[j]+ 
        betas[j,"betaX"]*x + 
        betas[j,"betaX2"]*(x)*x +
        betas[j,"betaSDI"]*SDI + 
        betas[j,"betaSDI_wintP.wateryr"]*SDI* wintP + 
        betas[j,"betaSICOND"]*SIhl[1] + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SIhl[1]*tmaxseq + 
        betas[j,"betaSICOND_wintP.wateryr"]*SIhl[1]*wintP + 
        betas[j,"betaX_SICOND"]*x*SIhl[1]+
        betas[j,"betaX_SDI"]*x*SDI  + 
        betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*x*tmaxseq +
        betas[j,"betatmax.fallspr"]*tmaxseq + 
        betas[j,"betaSDI_tmax.fallspr"]*SDI*tmaxseq +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*tmaxseq*wintP
      
      
      incT_SIhi[k,] <- B0[j]+ 
        betas[j,"betaX"]*x + 
        betas[j,"betaX2"]*(x)*x +
        betas[j,"betaSDI"]*SDI + 
        betas[j,"betaSDI_wintP.wateryr"]*SDI* wintP + 
        betas[j,"betaSICOND"]*SIhl[2] + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SIhl[2]*tmaxseq + 
        betas[j,"betaSICOND_wintP.wateryr"]*SIhl[2]*wintP + 
        betas[j,"betaX_SICOND"]*x*SIhl[2]+
        betas[j,"betaX_SDI"]*x*SDI  + 
        betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*x*tmaxseq +
        betas[j,"betatmax.fallspr"]*tmaxseq + 
        betas[j,"betaSDI_tmax.fallspr"]*SDI*tmaxseq +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*tmaxseq*wintP
    }
    
    CItmaxSIlo <- apply(incT_SIlo,2,quantile,c(0.025,0.5,0.975))
    CItmaxSIhi <- apply(incT_SIhi,2,quantile,c(0.025,0.5,0.975))
    
    Tmax.SI.Effect %<a-% {
      plot(tmaxseq.real,CItmaxSIhi[2,],ylim=c(0,max(CItmaxSIhi[2,])),type='l',lwd=3,xlab="Fall Spr Tmax (DegC)",ylab="Diameter Increment (cm)",cex.lab=1.5)
      #PEcAn.visualization::ciEnvelope(tmaxseq,CItmax[1,],CItmax[3,],col = "lightgrey")
      lines(tmaxseq.real,CItmaxSIlo[2,],lwd=2,col="grey")
      lines(tmaxseq.real,CItmaxSIhi[2,],lwd=2,col="purple")
      legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
    }
    
    # same but ggplot and prettier:
    T.si.low <- reshape2::melt(CItmaxSIlo) %>% spread(Var1, value) %>% add_column(si = "Low", tmax = tmaxseq.real)
    T.si.high <- reshape2::melt(CItmaxSIhi) %>% spread(Var1, value) %>% add_column(si = "High", tmax = tmaxseq.real)
    T.si.mid <- reshape2::melt(CItmax) %>% spread(Var1, value)%>% add_column(si = "Medium", tmax = tmaxseq.real)
    
    all.si.T <- rbind(T.si.low, T.si.high, T.si.mid)
    
    Tmax.SI.Effect.gg <- ggplot()+geom_ribbon(data =all.si.T, aes(tmax, ymin = `2.5%`, ymax = `97.5%`, fill = si), alpha = 0.5)+
      geom_line(data = all.si.T, aes(tmax, y = `50%`, color = si))+ylab("Predicted Growth")+xlab("Fall - Spring Tmax (DegC)")+theme_bw()+
      theme(panel.grid = element_blank())
    
    
    
    # temperature x Precipitation
    
    
    ## Tmax X Precip
    
    incP_tlo <- matrix(NA,ns,length(wintPseq))
    incP_thi <- matrix(NA,ns,length(wintPseq))
    
    
    
    
    for(k in seq_along(i)){
      j <- i[k]
      incP_tlo[k,] <-  B0[j]+ 
        betas[j,"betaX"]*x + 
        betas[j,"betaX2"]*(x)*x +
        betas[j,"betaSDI"]*SDI + 
        betas[j,"betaSDI_wintP.wateryr"]*SDI* wintPseq + 
        betas[j,"betaSICOND"]*SI + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxhl[1] + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + 
        betas[j,"betaX_SICOND"]*x*SI+
        betas[j,"betaX_SDI"]*x*SDI  + 
        betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
        betas[j,"betawintP.wateryr"]*wintPseq + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxhl[1] +
        betas[j,"betatmax.fallspr"]*sprfallTmaxhl[1] + 
        betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxhl[1] +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmaxhl[1]*wintPseq
      
      incP_thi[k,] <-   B0[j]+ 
        betas[j,"betaX"]*x + 
        betas[j,"betaX2"]*(x)*x +
        betas[j,"betaSDI"]*SDI + 
        betas[j,"betaSDI_wintP.wateryr"]*SDI* wintPseq + 
        betas[j,"betaSICOND"]*SI + 
        #betas[j,"betaSICOND_SDI"]*SDI*SI +  
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxhl[2] + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + 
        betas[j,"betaX_SICOND"]*x*SI+
        betas[j,"betaX_SDI"]*x*SDI  + 
        betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
        betas[j,"betawintP.wateryr"]*wintPseq + 
        # temperature
        betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxhl[2] +
        betas[j,"betatmax.fallspr"]*sprfallTmaxhl[2] + 
        betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxhl[2] +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmaxhl[2]*wintPseq
      
    }
    
    
    CIwintPTlo <- apply(incP_tlo,2,quantile,c(0.025,0.5,0.975))
    CIwintPThi <- apply(incP_thi,2,quantile,c(0.025,0.5,0.975))
    
    
    Precip.Tmax.Effect %<a-% {
      plot(wintPseq.real,CIwintPThi[2,],ylim=c(0,max(CIwintPTlo[2,])),type='l',lwd=3,xlab="Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
      #PEcAn.visualization::ciEnvelope(tmaxseq,CItmax[1,],CItmax[3,],col = "lightgrey")
      lines(wintPseq.real,CIwintPTlo[2,],lwd=2,col="grey")
      lines(wintPseq.real,CIwintPThi[2,],lwd=2,col="purple")
      legend("bottomleft",legend=c("21.9 DegC","25.3 DegC"),col=c("grey", "purple"),lwd=3)
    }
    
    
    P.tmax.low <- reshape2::melt(CIwintPTlo) %>% spread(Var1, value) %>% add_column(tmax = "Low", precip = wintPseq.real)
    P.tmax.high <- reshape2::melt(CIwintPThi) %>% spread(Var1, value) %>% add_column(tmax = "High", precip = wintPseq.real)
    P.tmax.mid <- reshape2::melt(CIwintP) %>% spread(Var1, value)%>% add_column(tmax = "Medium", precip = wintPseq.real)
    
    all.tmax.P <- rbind(P.tmax.low, P.tmax.high, P.tmax.mid)
    
    Precipitation.tmax.Effect.gg <- ggplot()+geom_ribbon(data =all.tmax.P, aes(precip, ymin = `2.5%`, ymax = `97.5%`, fill = tmax), alpha = 0.5)+
      geom_line(data = all.tmax.P, aes(precip, y = `50%`, color = tmax))+ylab("Predicted Growth")+xlab("Water Year Precipitation")+theme_bw()+
      theme(panel.grid = element_blank())
    
    # make into one big plot
    
    plot.effects.pryr %<a-% {
      split.screen(c(2, 2))
      # plot effects
      screen(1)
      SDI.Effect
      
      screen(2)
      Tree.Size.Effect
      
      screen(3)
      Climate.Effect
      
      screen(4)
      Tmax.Effect
      
      
      close.screen(all=TRUE)
    }
    
    png(height = 8, width = 6.5, units = "in", res = 200, paste0(workingdir,"/","Full_effects_",output.base.name,".png"))
    plot.effects.pryr
    dev.off()
    
    
    
    # plot interactions
    plot.interaction.effects.pryr %<a-%{
      split.screen(c(2, 3))
      screen(1)
      SDI.DBH.Effect
      
      screen(2)
      Climate.DBH.Effect
      
      screen(3)
      Climate.SDI.Effect
      
      screen(4)
      Tmax.DBH.Effect
      
      screen(5)
      Tmax.SDI.Effect
      
      screen(6)
      Precip.Tmax.Effect
      
      
      
      
      
      close.screen(all=TRUE)
    }
    
    png(height = 8, width = 13, units = "in", res = 200, paste0(workingdir,"/","Full_interaction_effects_", output.base.name,".png"))
    plot.interaction.effects.pryr
    dev.off()
    
    # now plot out the ggplots main effects:
    
    library(cowplot) 
    
    png(height = 8, width =10, units = "in", res = 200, paste0(workingdir,"/", "Full_effects_ggplots",output.base.name,".png"))
    plot_grid(
      SDI.Effect.gg + ylim(0,0.55), 
      SI.Effect.gg + ylim(0,0.55), 
      Size.Effect.gg+ ylim(0,0.55), 
      tmax.Effect.gg+ ylim(0,0.55), 
      wintP.Effect.gg+ ylim(0,0.55),
      
      ncol = 3, align = "hv"
    )
    dev.off()
    
    png(height = 8, width =13, units = "in", res = 200, paste0(workingdir,"/", "Interaction_effects_ggplots",output.base.name,".png"))
    plot_grid(
      SDI.DBH.Effect.gg + ylim(0,0.55)+ scale_fill_manual(values = c('Large'="#4dac26", 'Medium'="grey", 'Small'="#d01c8b")) + scale_color_manual(values = c('Large'="#4dac26", 'Medium'="grey", 'Small'="#d01c8b"))+theme(legend.position = c(0.2, 0.2)), 
      Precipitation.DBH.Effect.gg + ylim(0,0.55)+ scale_fill_manual(values = c('Large'="#4dac26", 'Medium'="grey", 'Small'="#d01c8b")) + scale_color_manual(values = c('Large'="#4dac26", 'Medium'="grey", 'Small'="#d01c8b"))+theme(legend.position = c(0.2, 0.2)), 
      Precipitation.SI.Effect.gg+ ylim(0,0.55)  + scale_fill_manual(values = c('High'="#5e3c99", 'Medium'="grey", 'Low'="#e66101")) + scale_color_manual(values = c('High'="#5e3c99", 'Medium'="grey", 'Low'="#e66101"))+theme(legend.position = c(0.2, 0.2)), 
      sdi.Precip.Effect.gg + ylim(0, 0.55)+ scale_fill_manual(values = c('High'="#018571", 'Medium'="grey", 'Low'="#a6611a")) + scale_color_manual(values = c('High'="#018571", 'Medium'="grey", 'Low'="#a6611a"))+xlab("Water Year Precipitation (mm)")+theme(legend.position = c(0.2, 0.2)), 
      Tmax.DBH.Effect.gg + ylim(0,0.55)+ scale_fill_manual(values = c('Large'="#4dac26", 'Medium'="grey", 'Small'="#d01c8b")) + scale_color_manual(values = c('Large'="#4dac26", 'Medium'="grey", 'Small'="#d01c8b"))+theme(legend.position = c(0.2, 0.2)), 
      tmax.SDI.Effect.gg + ylim(0,0.55)+ scale_fill_manual(values = c('High'="#018571", 'Medium'="grey", 'Low'="#a6611a")) + scale_color_manual(values = c('High'="#018571", 'Medium'="grey", 'Low'="#a6611a"))+theme(legend.position = c(0.2, 0.2)), 
      Tmax.SI.Effect.gg + ylim(0,0.55) + scale_fill_manual(values = c('High'="#5e3c99", 'Medium'="grey", 'Low'="#e66101")) + scale_color_manual(values = c('High'="#5e3c99", 'Medium'="grey", 'Low'="#e66101"))+theme(legend.position = c(0.2, 0.2)), 
      Precipitation.tmax.Effect.gg+ ylim(0,0.55)+ scale_fill_manual(values = c('High'="#ca0020", 'Medium'="grey", 'Low'="#0571b0")) + scale_color_manual(values = c('High'="#ca0020", 'Medium'="grey", 'Low'="#0571b0"))+theme(legend.position = c(0.2, 0.2)), 
      
      ncol = 4, align = "hv"
    )
    
    
    dev.off()
    
    
    
    
    
    # plot out the taus:
    out.taus <- out[,c("tau_inc", "tau_PLOT", "tau_dbh", "tau_add")]
    out.taus.m <- reshape2::melt(out.taus)
    tau.summary<- out.taus.m %>% group_by(Var2) %>% summarise(median = quantile(value, 0.5, na.rm=TRUE), 
                                                              ci.low = quantile(value, 0.025, na.rm=TRUE),
                                                              ci.high = quantile(value, 0.975, na.rm=TRUE),)
    
    
    png(height = 4, width = 6, units = "in", res = 200, paste0(output.base.name, "_posterior_precision_plots.png"))
    ggplot()+geom_errorbar(data = tau.summary, aes(x = Var2, ymin = ci.low, ymax = ci.high), width = 0.1)+
      geom_point(data = tau.summary, aes(x = Var2, y = median))+ylab("Posterior Precision")+xlab("")+theme_bw()
    dev.off()
    
    out.taus.m$sigma <- 1/sqrt(out.taus.m$value)
    sigma.summary<- out.taus.m %>% group_by(Var2) %>% summarise(median = quantile(sigma, 0.5, na.rm=TRUE), 
                                                                ci.low = quantile(sigma, 0.025, na.rm=TRUE),
                                                                ci.high = quantile(sigma, 0.975, na.rm=TRUE),)
    
    png(height = 4, width = 6, units = "in", res = 200, paste0(output.base.name, "_posterior_sigma_plots.png"))
    ggplot()+geom_errorbar(data = sigma.summary, aes(x = Var2, ymin = ci.low, ymax = ci.high), width = 0.1)+
      geom_point(data = sigma.summary, aes(x = Var2, y = median))+ylab("Posterior Sigma")+xlab("")+theme_bw()
    dev.off()
    
    # plot up all the betas
    betas.m <- reshape2::melt(betas)
    beta.summary<- betas.m %>% group_by(Var2) %>% summarise(median = quantile(value, 0.5, na.rm=TRUE), 
                                                            ci.low = quantile(value, 0.025, na.rm=TRUE),
                                                            ci.high = quantile(value, 0.975, na.rm=TRUE),)
    
    
    
    png(height = 4, width = 6, units = "in", res = 200, paste0(output.base.name, "_posterior_beta_plots.png"))
    ggplot()+geom_errorbar(data = beta.summary, aes(x = Var2, ymin = ci.low, ymax = ci.high), width = 0.1)+
      geom_point(data = beta.summary, aes(x = Var2, y = median))+ylab("Posterior Fixed Effects")+xlab("")+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
    dev.off()
    
    stage1.ests <-as.matrix(posterior.ests) 
    # get the priors for the stage2 model--ie the posteriors for stage 1:
    s1.taus <- stage1.ests[,c("tau_inc", "tau_PLOT", "tau_dbh", "tau_add")]
    s1.taus.m <- reshape2::melt(s1.taus)
    prior.tau.summary<- s1.taus.m %>% group_by(Var2) %>% summarise(median = quantile(value, 0.5, na.rm=TRUE), 
                                                                   ci.low = quantile(value, 0.025, na.rm=TRUE),
                                                                   ci.high = quantile(value, 0.975, na.rm=TRUE),)
    
    prior.tau.summary$model <- "Stage 1"
    tau.summary$model <- "Stage 2"
    
    both.taus <- rbind(prior.tau.summary, tau.summary)
    
    png(height = 4, width = 6, units = "in", res = 200, paste0(output.base.name, "_posterior_taus_stage1_stage2_plots.png"))
    ggplot()+geom_errorbar(data = both.taus, aes(x = Var2, ymin = ci.low, ymax = ci.high, color = model), width = 0.1)+
      geom_point(data =both.taus, aes(x = Var2, y = median, color = model))+ylab("Posterior Precisions")+xlab("")+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
    dev.off()
    