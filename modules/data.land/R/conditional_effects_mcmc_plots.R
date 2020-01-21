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
# function to read in multiple MCMC files and plot the conditional effects of tree size, climate, si, SDI, etc
library(rjags)
library(PEcAn.data.land)
#install.packages("pryr")
#install.packages("gridExtra")
#install.packages("psych")


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
file.base.name <- "Full.model.stg1.distrubcd."
output.base.name <- "Full.model.stg1.distrubcd."
#stage2 <- TRUE
stage2 <- FALSE
workingdir <- "/Users/kah/Documents/docker_pecan/pecan/"
climate <- "wintP.wateryr"
cov.data = cov.data
jags.comb <- NULL



for(i in 395:400){ # note this model stopped early b/c convergence
  load(paste0(workingdir,"/IGF_PIPO_AZ_mcmc/cyverse_runs/", file.base.name,i,".RData"))
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
traceplot(jags.comb[, "betaSDI"], main = expression(beta~SDI), xlab = "last 2500 iterations")
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




# if the model run was stage2  then lets plot the posteriors and priors together:
if(stage2 ==TRUE){
  priors <- readRDS("/home/rstudio/INV_FIA_DATA/data/IGFPPT.Tmax.fs.only.climint.40000.rds")
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
    hist(z0)
    x <- mean(z0,na.rm = TRUE)
    Xhl <- quantile(z0,c(1/6,5/6),na.rm=TRUE)
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
    xseq <- 1:58
    incX <- matrix(NA,ns,length(xseq))
    
    
    # need to add time since fire and all the temp interactions
    for(k in seq_along(i)){
      j <- i[k]
      incX[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wateryrP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wateryrP + betas[j,"betaX"]*xseq +
        betas[j,"betaX_SDI"]*xseq*SDI + betas[j,"betaX_SICOND"]*xseq*SI + betas[j,"betaX_wintP.wateryr"]*xseq*wateryrP +
        betas[j,"betawintP.wateryr"]*wateryrP + 
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*xseq*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*xseq*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wateryrP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmax
    }
    
    CIX <- apply(incX, 2, quantile,c(0.025,0.5,0.975)) # get CI on X
    
    source("/home/rstudio/pecan/visualization/R/ciEnvelope.R") # read in ci.envelope function
    
    
    # plot as pseudo object to save for later
    Tree.Size.Effect %<a-% {
      plot(xseq,CIX[2,],ylim=c(0,max(CIX)),type='n',ylab="Diameter Increment (cm)",xlab="DBH (cm)",cex.lab=1.5)
      ciEnvelope(xseq,CIX[1,],CIX[3,],col = "lightgrey")
      lines(xseq,CIX[2,],lwd=2)
      abline(h=0)
    }
    
    
    ##SDI
    hist(cov.data$SDI)
    
    # KH note: need to standardize SDIseq first since I ran the model with standardized covariates
    SDIseq.real <- seq(0,400,by=10)
    SDIseq <- (SDIseq.real-mean(temp2$SDI))/sd(temp2$SDI)
    
    incSDI <- matrix(NA,ns,length(SDIseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incSDI[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDIseq + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP+ 
        
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*x*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDIseq*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wateryrP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmax
      
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
    
    ## SDI * size
    incSDIXhi <- matrix(NA,ns,length(SDIseq))
    incSDIXlo <- matrix(NA,ns,length(SDIseq))
    for(k in seq_along(i)){
      j <- i[k]
      incSDIXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[1] + 
        betas[j,"betaX_SDI"]*Xhl[1]*SDIseq + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
        betas[j,"betawintP.wateryr"]*wintP+ 
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*Xhl[1]*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDIseq*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wateryrP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmax
      
      
      
      incSDIXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[2] +
        betas[j,"betaX_SDI"]*Xhl[2]*SDIseq + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*Xhl[2]*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDIseq*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wateryrP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmax
      
    }
    CIsdiXlo <- apply(incSDIXlo,2,quantile,c(0.025,0.5,0.975))
    CIsdiXhi <- apply(incSDIXhi,2,quantile,c(0.025,0.5,0.975))
    
    # SDI plot
    SDI.DBH.Effect %<a-% {
      plot(SDIseq.real,CIsdi[2,],ylim=c(0,max(CIsdi[,1]+0.05)),type='l',xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5,lwd=3)
      #ciEnvelope(SDIseq,CIsdi[1,],CIsdi[3,],col = "lightgrey")
      lines(SDIseq.real,CIsdiXlo[2,],lwd=3,col="blue")
      lines(SDIseq.real,CIsdiXhi[2,],lwd=3,col="red")
      legend("topright",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
      abline(h=0)
    }
    
    ##SI
    hist(cov.data$SI)
    # KH note: need to standardize SDIseq first since I ran the model with standardized covariates
    SIseq.real <- 20:60
    SIseq <- (SIseq.real-mean(temp2$COND_SICOND))/sd(temp2$COND_SICOND)
    #SIseq <- 20:60
    incSI <- matrix(NA,ns,length(SDIseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incSI[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
        betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIseq + betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SIseq*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*x*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wateryrP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmax
      
    }
    CIsi <- apply(incSI,2,quantile,c(0.025,0.5,0.975))
    
    SI.Effect %<a-% {
      plot(SIseq.real,CIsi[2,],ylim=c(0,max(CIsi)),type='n',xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
      ciEnvelope(SIseq.real,CIsi[1,],CIsi[3,],col = "lightgrey")
      lines(SIseq.real,CIsi[2,],lwd=2)
      abline(h=0)
    }
    
    
    ## SI x DBH
    incSIXlo <- matrix(NA,ns,length(SDIseq))
    incSIXmed <- matrix(NA,ns,length(SDIseq))
    incSIXhi <- matrix(NA,ns,length(SDIseq))
    for(k in seq_along(i)){
      j <- i[k]
      incSIXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
        betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*Xhl[1] + 
        betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SIseq + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SIseq*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*Xhl[1]*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wateryrP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmax
      
      
      
      incSIXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
        betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*Xhl[2] + 
        betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SIseq + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SIseq*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*Xhl[2]*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wateryrP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmax
      
      
      
      incSIXmed[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
        betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIseq + betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SIseq*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*x*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wateryrP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmax
      
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
    
    ## wintP
    clim.data <- readRDS("PRISM_non_scaled.rds")
    hist(time_data$wintP.wateryr)
    wintPseq.real <- 0:800
    wintPseq <- (wintPseq.real-mean(clim.data$wintP.wateryr))/sd(clim.data$wintP.wateryr)
    incP <- matrix(NA,ns,length(wintPseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incP[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
        betas[j,"betawintP.wateryr"]*wintPseq+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*x*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintPseq + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmax
      
    }
    
    CIwintP <- apply(incP,2,quantile,c(0.025,0.5,0.975))
    
    Climate.Effect %<a-% {
      plot(wintPseq.real, CIwintP[2,],ylim=c(0,max(CIwintP + 0.05)),type='n',xlab="Water Year Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
      ciEnvelope(wintPseq.real,CIwintP[1,], CIwintP[3,],col = "lightgrey")
      lines(wintPseq.real, CIwintP[2,],lwd=2)
      abline(h=0)
    }
    
    ## PRECIP x SDI
    incPSDIlo <- matrix(NA,ns,length(wintPseq))
    incPSDIhi <- matrix(NA,ns,length(wintPseq))
    for(k in seq_along(i)){
      j <- i[k]
      incPSDIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[1] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl[1]*wintPseq + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[1]*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDIhl[1] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
        betas[j,"betawintP.wateryr"]*wintPseq +
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDIhl[1]*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*x*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDIhl[1]*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintPseq + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmax
      
      
      incPSDIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[2] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl[2]*wintPseq + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[2]*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDIhl[2] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
        betas[j,"betawintP.wateryr"]*wintPseq + 
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDIhl[2]*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*x*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDIhl[2]*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintPseq + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmax
      
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
    
    ## PRECIP x DBH
    incPXlo <- matrix(NA,ns,length(wintPseq))
    incPXhi <- matrix(NA,ns,length(wintPseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incPXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*Xhl[1] + 
        betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintPseq +
        betas[j,"betawintP.wateryr"]*wintPseq+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*Xhl[1]*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintPseq + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmax
      
      
      incPXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*Xhl[2] + 
        betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintPseq +
        betas[j,"betawintP.wateryr"]*wintPseq+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*Xhl[2]*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintPseq + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmax
      
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
    
    ## Precip X SI
    incP_SIlo <- matrix(NA,ns,length(wintPseq))
    incP_SIhi <- matrix(NA,ns,length(wintPseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incP_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
        betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[1] + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x[1] + 
        betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintPseq +
        betas[j,"betawintP.wateryr"]*wintPseq+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SIhl[1]*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*x[1]*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SIhl[1]*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*x[1]*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintPseq + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmax
      
      
      
      incP_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
        betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[2] + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x[1] +
        betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintPseq +
        betas[j,"betawintP.wateryr"]*wintPseq+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SIhl[2]*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*x[1]*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SIhl[2]*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*x[1]*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintPseq + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmax
      
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
    
    
    
    ## SI X SDI
    
    incSDI_SIlo <- matrix(NA,ns,length(SDIseq))
    incSDI_SIhi <- matrix(NA,ns,length(SDIseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incSDI_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
        betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[1] + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x[1] + 
        betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SIhl[1]*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*x[1]*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SIhl[1]*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*x[1]*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDIseq*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmax
      
      
      incSDI_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
        betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[2] + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x[1] + 
        betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SIhl[2]*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*x[1]*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SIhl[2]*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*x[1]*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDIseq*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmax
      
    }
    CISDISIlo <- apply(incSDI_SIlo,2,quantile,c(0.025,0.5,0.975))
    CISDISIhi <- apply(incSDI_SIhi,2,quantile,c(0.025,0.5,0.975))
    
    SDI.SI.Effect %<a-% {
      plot(SDIseq.real,CISDISIhi[2,],ylim=c(0,max(CISDISIhi[2,])),type='l',lwd=3,xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5, col = "purple")
      # #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "purple")
      lines(SDIseq.real,CISDISIlo[2,],lwd=2,col="grey")
      #lines(wintPseq.real,CIwintPSIhi[2,],lwd=2,col="purple")
      legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
    }
    
    
    # Temperature effect
    
    
    #clim.data <- readRDS("PRISM_non_scaled.rds")
    hist(time_data$tmax.fallspr)
    range(clim.data$tmax.fallspr)
    tmaxseq.real <- 17:31
    tmaxseq <- (tmaxseq.real-mean(clim.data$tmax.fallspr))/sd(clim.data$tmax.fallspr)
    incT <- matrix(NA,ns,length(tmaxseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incT[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*tmaxseq + betas[j,"betaX_tmax.fallspr"]*x*tmaxseq +
        betas[j,"betatmax.fallspr"]*tmaxseq + betas[j,"betaSDI_tmax.fallspr"]*SDI*tmaxseq +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*tmaxseq*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*x*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*tmaxseq
      
    }
    
    CItmax <- apply(incT,2,quantile,c(0.025,0.5,0.975))
    
    Tmax.Effect %<a-% {
      plot(tmaxseq.real, CItmax[2,],ylim=c(0,max(CIwintP + 0.05)),type='n',xlab="Tmax fall spr (DegC)",ylab="Diameter Increment (cm)",cex.lab=1.5)
      ciEnvelope(tmaxseq.real,CItmax[1,], CItmax[3,],col = "lightgrey")
      lines(tmaxseq.real, CItmax[2,],lwd=2)
      abline(h=0)
    }
    
    # temperature x X
    
    # Temperature x SDI
    
    incTemplo <- matrix(NA,ns,length(tmaxseq))
    incTemphi <- matrix(NA,ns,length(tmaxseq))
    for(k in seq_along(i)){
      j <- i[k]
      incTemplo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[1] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl[1]*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[1]*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDIhl[1] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP +
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*tmaxseq + betas[j,"betaX_tmax.fallspr"]*x*tmaxseq +
        betas[j,"betatmax.fallspr"]*tmaxseq + betas[j,"betaSDI_tmax.fallspr"]*SDIhl[1]*tmaxseq +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*tmaxseq*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*x*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDIhl[1]*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*tmaxseq
      
      
      incTemphi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[2] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl[2]*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[2]*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDIhl[2] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP + 
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*tmaxseq+ betas[j,"betaX_tmax.fallspr"]*x*tmaxseq +
        betas[j,"betatmax.fallspr"]*tmaxseq + betas[j,"betaSDI_tmax.fallspr"]*SDIhl[2]*tmaxseq +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*tmaxseq*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*x*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDIhl[2]*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*tmaxseq
      
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
    
    ## Tmax x DBH
    incTXlo <- matrix(NA,ns,length(tmaxseq))
    incTXhi <- matrix(NA,ns,length(tmaxseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incTXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[1] + 
        betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*tmaxseq + betas[j,"betaX_tmax.fallspr"]*Xhl[1]*tmaxseq +
        betas[j,"betatmax.fallspr"]*tmaxseq + betas[j,"betaSDI_tmax.fallspr"]*SDI*tmaxseq +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*tmaxseq*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*Xhl[1]*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*tmaxseq
      
      
      incTXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[2] + 
        betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*tmaxseq + betas[j,"betaX_tmax.fallspr"]*Xhl[2]*tmaxseq +
        betas[j,"betatmax.fallspr"]*tmaxseq + betas[j,"betaSDI_tmax.fallspr"]*SDI*tmaxseq +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*tmaxseq*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*Xhl[2]*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*tmaxseq
      
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
    
    ## Tmax X SI
    # temperature x SICOND
    incT_SIlo <- matrix(NA,ns,length(tmaxseq))
    incT_SIhi <- matrix(NA,ns,length(tmaxseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incT_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[1] + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x[1] + 
        betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SIhl[1]*tmaxseq + betas[j,"betaX_tmax.fallspr"]*x[1]*tmaxseq +
        betas[j,"betatmax.fallspr"]*tmaxseq + betas[j,"betaSDI_tmax.fallspr"]*SDI*tmaxseq +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*tmaxseq*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SIhl[1]*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*x[1]*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*tmaxseq
      
      
      
      incT_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[2] + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x[1] +
        betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SIhl[2]*tmaxseq + betas[j,"betaX_tmax.fallspr"]*x[1]*tmaxseq +
        betas[j,"betatmax.fallspr"]*tmaxseq + betas[j,"betaSDI_tmax.fallspr"]*SDI*tmaxseq +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*tmaxseq*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SIhl[2]*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*x[1]*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*tmaxseq
      
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
    
    # temperature x Precipitation
    
    ## Tmax x DBH
    incTXlo <- matrix(NA,ns,length(tmaxseq))
    incTXhi <- matrix(NA,ns,length(tmaxseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incTXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[1] + 
        betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*tmaxseq + betas[j,"betaX_tmax.fallspr"]*Xhl[1]*tmaxseq +
        betas[j,"betatmax.fallspr"]*tmaxseq + betas[j,"betaSDI_tmax.fallspr"]*SDI*tmaxseq +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*tmaxseq*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*Xhl[1]*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*tmaxseq
      
      
      incTXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[2] + 
        betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*tmaxseq + betas[j,"betaX_tmax.fallspr"]*Xhl[2]*tmaxseq +
        betas[j,"betatmax.fallspr"]*tmaxseq + betas[j,"betaSDI_tmax.fallspr"]*SDI*tmaxseq +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*tmaxseq*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*Xhl[2]*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*tmaxseq
      
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
    
    ## Tmax X SI
    # temperature x SICOND
    incP_tlo <- matrix(NA,ns,length(wintPseq))
    incP_thi <- matrix(NA,ns,length(wintPseq))
    
    
    
    
    for(k in seq_along(i)){
      j <- i[k]
      incP_tlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq+ 
        betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[1] + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq+ betas[j,"betaX"]*x[1] + 
        betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintPseq+
        betas[j,"betawintP.wateryr"]*wintPseq+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SIhl[1]*sprfallTmaxhl[1] + betas[j,"betaX_tmax.fallspr"]*x[1]*sprfallTmaxhl[1] +
        betas[j,"betatmax.fallspr"]*sprfallTmaxhl[1] + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxhl[1] +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmaxhl[1]*wintPseq+
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SIhl[1]*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*x[1]*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintPseq+ betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmaxhl[1]
      
      
      
      incP_thi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq+ 
        betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[2] + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq+ betas[j,"betaX"]*x[1] +
        betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintPseq+
        betas[j,"betawintP.wateryr"]*wintPseq+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SIhl[2]*sprfallTmaxhl[2] + betas[j,"betaX_tmax.fallspr"]*x[1]*sprfallTmaxhl[2] +
        betas[j,"betatmax.fallspr"]*sprfallTmaxhl[2] + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxhl[2] +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmaxhl[2]*wintPseq+
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SIhl[2]*timesinceFire + betas[j,"betaX_TimeSinceFIRE"]*x[1]*timesinceFire +
        betas[j,"betaTimeSinceFIRE"]*timesinceFire + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFire +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFire*wintPseq+ betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFire*sprfallTmaxhl[2]
      
    }
    CIwintPTlo <- apply(incP_tlo,2,quantile,c(0.025,0.5,0.975))
    CIwintPThi <- apply(incP_thi,2,quantile,c(0.025,0.5,0.975))
    
    
    Precip.Tmax.Effect %<a-% {
      plot(wintPseq.real,CIwintPThi[2,],ylim=c(0,max(CIwintPThi[2,])),type='l',lwd=3,xlab="Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
      #PEcAn.visualization::ciEnvelope(tmaxseq,CItmax[1,],CItmax[3,],col = "lightgrey")
      lines(wintPseq.real,CIwintPTlo[2,],lwd=2,col="grey")
      lines(wintPseq.real,CIwintPThi[2,],lwd=2,col="purple")
      legend("bottomleft",legend=c("21.9 DegC","25.3 DegC"),col=c("grey", "purple"),lwd=3)
    }
    
    # temperature x timesince fire
    
    incTFirelo <- matrix(NA,ns,length(tmaxseq))
    incTFirehi <- matrix(NA,ns,length(tmaxseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incTFirelo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*tmaxseq + betas[j,"betaX_tmax.fallspr"]*x*tmaxseq +
        betas[j,"betatmax.fallspr"]*tmaxseq + betas[j,"betaSDI_tmax.fallspr"]*SDI*tmaxseq +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*tmaxseq*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFirexhl[1] + betas[j,"betaX_TimeSinceFIRE"]*x*timesinceFirexhl[1] +
        betas[j,"betaTimeSinceFIRE"]*timesinceFirexhl[1]  + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFirexhl[1]  +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFirexhl[1]*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFirexhl[1]*tmaxseq
      
      
      incTFirehi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*tmaxseq + betas[j,"betaX_tmax.fallspr"]*x*tmaxseq +
        betas[j,"betatmax.fallspr"]*tmaxseq + betas[j,"betaSDI_tmax.fallspr"]*SDI*tmaxseq +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*tmaxseq*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFirexhl[2] + betas[j,"betaX_TimeSinceFIRE"]*x*timesinceFirexhl[2] +
        betas[j,"betaTimeSinceFIRE"]*timesinceFirexhl[2] + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFirexhl[2] +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFirexhl[2]*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFirexhl[2]*tmaxseq
      
    }
    
    CItmaxFirelo <- apply(incTFirelo,2,quantile,c(0.025,0.5,0.975))
    CItmaxFirehi <- apply(incTFirehi,2,quantile,c(0.025,0.5,0.975))
    
    Tmax.Fire.Effect %<a-% {
      plot(tmaxseq.real,  CItmaxFirehi[2,],ylim=c(0,max(  CItmaxFirehi)+0.1),type='l',lwd=3,xlab="Tmax (DegC)",ylab="Diameter Increment (cm)",cex.lab=1.5)
      #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
      lines(tmaxseq.real,  CItmaxFirelo[2,],lwd=2,col="blue")
      #lines(tmaxseq.real,CItmaxXhi[2,],lwd=2,col="red")
      legend("bottomright",legend=c("3 years", "51 years"),col=c("blue",1,2),lwd=3)
    }
    
    
    # time since fire
    
    hist(time_data$TimeSinceFIRE)
    
    timesinceFire <- mean(time_data$TimeSinceFIRE,na.rm = TRUE)
    timesinceFirexhl <- quantile(time_data$TimeSinceFIRE,c(1/6,5/6),na.rm = TRUE)
    
    Fireseq <- -20:51
    #tmaxseq <- (tmaxseq.real-mean(clim.data$))/sd(clim.data$tmax.fallspr)
    incF <- matrix(NA,ns,length(Fireseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incF[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*Fireseq + betas[j,"betaX_TimeSinceFIRE"]*x*Fireseq +
        betas[j,"betaTimeSinceFIRE"]*Fireseq + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*Fireseq +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*Fireseq*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*Fireseq*sprfallTmax
      
    }
    
    CIfire <- apply(incF,2,quantile,c(0.025,0.5,0.975))
    
    Fire.Effect %<a-% {
      plot(Fireseq, CIfire[2,],ylim=c(0,max(CIfire + 0.05)),type='n',xlab="Time Since Fire (years)",ylab="Diameter Increment (cm)",cex.lab=1.5)
      ciEnvelope(Fireseq,CIfire[1,], CIfire[3,],col = "lightgrey")
      lines(Fireseq, CIfire[2,],lwd=2)
      abline(h=0)
    }
    
    
    # time since fire x DBH
    
    incFXlo <- matrix(NA,ns,length(Fireseq))
    incFXhi <- matrix(NA,ns,length(Fireseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incFXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[1] + 
        betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*Fireseq + betas[j,"betaX_TimeSinceFIRE"]*Xhl[1]*Fireseq +
        betas[j,"betaTimeSinceFIRE"]*Fireseq + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*Fireseq +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*Fireseq*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*Fireseq*sprfallTmax
      
      
      incFXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[2] + 
        betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*Fireseq + betas[j,"betaX_TimeSinceFIRE"]*Xhl[2]*Fireseq +
        betas[j,"betaTimeSinceFIRE"]*Fireseq + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*Fireseq +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*Fireseq*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*Fireseq*sprfallTmax
      
    }
    
    CIfireXlo <- apply(incFXlo,2,quantile,c(0.025,0.5,0.975))
    CIfireXhi <- apply(incFXhi,2,quantile,c(0.025,0.5,0.975))
    
    Fire.DBH.Effect %<a-% {
      plot(Fireseq,CIfire[2,],ylim=c(0,max(CIfireXhi)+0.1),type='l',lwd=3,xlab="Time Since Fire (years)",ylab="Diameter Increment (cm)",cex.lab=1.5)
      #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
      lines(Fireseq,CIfireXlo[2,],lwd=2,col="blue")
      lines(Fireseq,CIfireXhi[2,],lwd=2,col="red")
      legend("bottomright",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
    }
    
    # time since fire x SDI
    
    incFSDIlo <- matrix(NA,ns,length(Fireseq))
    incFSDIhi <- matrix(NA,ns,length(Fireseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incFSDIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[1] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl[1]*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[1]*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDIhl[1] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDIhl[1]*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*Fireseq + betas[j,"betaX_TimeSinceFIRE"]*x*Fireseq +
        betas[j,"betaTimeSinceFIRE"]*Fireseq + betas[j,"betaSDI_TimeSinceFIRE"]*SDIhl[1]*Fireseq +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*Fireseq*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*Fireseq*sprfallTmax
      
      
      incFSDIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[2] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl[2]*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[2]*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDIhl[2] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDIhl[2]*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*Fireseq + betas[j,"betaX_TimeSinceFIRE"]*x*Fireseq +
        betas[j,"betaTimeSinceFIRE"]*Fireseq + betas[j,"betaSDI_TimeSinceFIRE"]*SDIhl[2]*Fireseq +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*Fireseq*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*Fireseq*sprfallTmax
      
    }
    
    CIfireSDIlo <- apply(incFSDIlo,2,quantile,c(0.025,0.5,0.975))
    CIfireSDIhi <- apply(incFSDIhi,2,quantile,c(0.025,0.5,0.975))
    
    Fire.SDI.Effect %<a-% {
      plot(Fireseq,CIfire[2,],ylim=c(0,max(CIfireSDIhi)+0.1),type='l',lwd=3,xlab="Time Since Fire (years)",ylab="Diameter Increment (cm)",cex.lab=1.5)
      #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
      lines(Fireseq,CIfireSDIlo[2,],lwd=2,col="blue")
      lines(Fireseq,CIfireSDIhi[2,],lwd=2,col="red")
      legend("bottomright",legend=c("low SDI","mean","high SDI"),col=c("blue",1,2),lwd=3)
    }
    
    # time since fire x SICOND
    incFSIlo <- matrix(NA,ns,length(Fireseq))
    incFSIhi <- matrix(NA,ns,length(Fireseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incFSIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[1] + 
        betas[j,"betaSICOND_wintP.wateryr"]*SIhl[1]*wintP + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SIhl[1]*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SIhl[1]*Fireseq + betas[j,"betaX_TimeSinceFIRE"]*x*Fireseq +
        betas[j,"betaTimeSinceFIRE"]*Fireseq + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*Fireseq +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*Fireseq*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*Fireseq*sprfallTmax
      
      
      incFSIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[2] + 
        betas[j,"betaSICOND_wintP.wateryr"]*SIhl[2]*wintP + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIhl[2] + betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SIhl[2]*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintP +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SIhl[2]*Fireseq + betas[j,"betaX_TimeSinceFIRE"]*x*Fireseq +
        betas[j,"betaTimeSinceFIRE"]*Fireseq + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*Fireseq +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*Fireseq*wintP + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*Fireseq*sprfallTmax
      
    }
    
    CIfireSIlo <- apply(incFSIlo,2,quantile,c(0.025,0.5,0.975))
    CIfireSIhi <- apply(incFSIhi,2,quantile,c(0.025,0.5,0.975))
    
    Fire.SI.Effect %<a-% {
      plot(Fireseq,CIfire[2,],ylim=c(0,max(CIfireSIhi)+0.1),type='l',lwd=3,xlab="Time Since Fire (years)",ylab="Diameter Increment (cm)",cex.lab=1.5)
      #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
      lines(Fireseq,CIfireSIlo[2,],lwd=2,col="blue")
      lines(Fireseq,CIfireSIhi[2,],lwd=2,col="red")
      legend("bottomright",legend=c("low SDI","mean","high SDI"),col=c("blue",1,2),lwd=3)
    }
    
    # time since fire x Precipitation
    # temperature x timesince fire
    
    incPFirelo <- matrix(NA,ns,length(wintPseq))
    incPFirehi <- matrix(NA,ns,length(wintPseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incPFirelo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
        betas[j,"betawintP.wateryr"]*wintPseq+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFirexhl[1] + betas[j,"betaX_TimeSinceFIRE"]*x*timesinceFirexhl[1] +
        betas[j,"betaTimeSinceFIRE"]*timesinceFirexhl[1]  + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFirexhl[1]  +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFirexhl[1]*wintPseq + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFirexhl[1]*sprfallTmax
      
      
      incPFirehi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
        betas[j,"betawintP.wateryr"]*wintPseq+
        # temperature
        betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
        betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax +
        betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq +
        # time since fire
        betas[j,"betaSICOND_TimeSinceFIRE"]*SI*timesinceFirexhl[2] + betas[j,"betaX_TimeSinceFIRE"]*x*timesinceFirexhl[2] +
        betas[j,"betaTimeSinceFIRE"]*timesinceFirexhl[2] + betas[j,"betaSDI_TimeSinceFIRE"]*SDI*timesinceFirexhl[2] +
        betas[j,"betaTimeSinceFIRE_wintP.wateryr"]*timesinceFirexhl[2]*wintPseq + betas[j,"betaTimeSinceFIRE_tmax.fallspr"]*timesinceFirexhl[2]*sprfallTmax
      
    }
    
    CIwintPFirelo <- apply(incPFirelo,2,quantile,c(0.025,0.5,0.975))
    CIwintPFirehi <- apply(incPFirehi,2,quantile,c(0.025,0.5,0.975))
    
    Precip.Fire.Effect %<a-% {
      plot(wintPseq.real,  CIwintPFirehi[2,],ylim=c(0,max(  CIwintPFirehi)),type='l',lwd=3,xlab="Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
      #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
      lines(wintPseq.real,  CIwintPFirelo[2,],lwd=2,col="blue")
      #lines(tmaxseq.real,CItmaxXhi[2,],lwd=2,col="red")
      legend("bottomright",legend=c("3 years", "51 years"),col=c("blue",1,2),lwd=3)
    }
    
    
    plot.effects.pryr %<a-% {
      split.screen(c(3, 2))
      # plot effects
      screen(1)
      SI.Effect
      
      screen(2)
      SDI.Effect
      
      screen(3)
      Tree.Size.Effect
      
      screen(4)
      Climate.Effect
      
      screen(5)
      Tmax.Effect
      
      screen(6)
      Fire.Effect
      
      close.screen(all=TRUE)
    }
    
    png(height = 14, width = 6.5, units = "in", res = 200, paste0("/home/rstudio/Full_effects_",output.base.name,".png"))
    plot.effects.pryr
    dev.off()
    
    
    
    # plot interactions
    plot.interaction.effects.pryr %<a-%{
      split.screen(c(5, 3))
      screen(1)
      SI.DBH.Effect
      screen(2)
      SDI.DBH.Effect
      screen(3)
      Climate.DBH.Effect
      
      screen(4)
      Climate.SI.Effect
      screen(5)
      Climate.SDI.Effect
      screen(6)
      SDI.SI.Effect
      
      screen(7)
      Tmax.DBH.Effect
      
      screen(8)
      Tmax.SI.Effect
      screen(9)
      Tmax.SDI.Effect
      
      screen(10)
      Precip.Tmax.Effect
      
      screen(11)
      Fire.DBH.Effect
      
      screen(12)
      Fire.SI.Effect
      
      screen(13)
      Fire.SDI.Effect
      
      screen(14)
      Tmax.Fire.Effect
      
      screen(15)
      Precip.Fire.Effect
      
      close.screen(all=TRUE)
    }
    
    png(height = 24, width = 10, units = "in", res = 200, paste0("/home/rstudio/Full_interaction_effects_", output.base.name,".png"))
    plot.interaction.effects.pryr
    dev.off()
    
    etaSDI_wintP.wateryr"], main = expression(beta~SDI_wateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_wateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "tau_inc"], main = "tau_inc", xlab = "last 2500 iterations")
traceplot(jags.comb[, "tau_dbh"], main = "tau_dbh", xlab = "last 2500 iterations")
traceplot(jags.comb[, "tau_PLOT"], main = "tau_PLOT", xlab = "last 2500 iterations")
traceplot(jags.comb[, "tau_add"], main = "tau_add", xlab = "last 2500 iterations")

dev.off()




# if the model run was stage2  then lets plot the posteriors and priors together:
if(stage2 ==TRUE){
  priors <- readRDS("/home/rstudio/INV_FIA_DATA/data/IGFPPT.noX2.tau.norm.129_0.0128.rds")
  tau.inc <- (as.matrix(priors[,"tau_inc"]))
  tau.inc.post <- (as.matrix(jags.comb[,"tau_inc"]))
  
  # get beta column names
  betas <- colnames(as.matrix(priors))[grep(colnames(as.matrix(priors)), pattern = "beta")]
  taus <- colnames(as.matrix(priors))[grep(colnames(as.matrix(priors)), pattern = "tau")]
  all.params <-c(betas, taus)
  
  
  
  png(height = 10, width = 6, units = "in", res = 300, paste0(workingdir,output.base.name,"_priors_posteriors.png"))
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
                          plt.num = 1:344)
    alpha.summary$plt.num <- 1:344
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
    hist(z0)
    x <- mean(z0,na.rm = TRUE)
    Xhl <- quantile(z0,c(1/6,5/6),na.rm=TRUE)
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
    xseq <- 1:58
    incX <- matrix(NA,ns,length(xseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incX[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wateryrP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wateryrP + betas[j,"betaX"]*xseq +
        betas[j,"betaX_SDI"]*xseq*SDI + betas[j,"betaX_SICOND"]*xseq*SI + betas[j,"betaX_wintP.wateryr"]*xseq*wateryrP +
        betas[j,"betawintP.wateryr"]*wateryrP
    }
    
    CIX <- apply(incX, 2, quantile,c(0.025,0.5,0.975)) # get CI on X
    
    source("/home/rstudio/pecan/visualization/R/ciEnvelope.R") # read in ci.envelope function
    
    
    # plot as pseudo object to save for later
    Tree.Size.Effect %<a-% {
      plot(xseq,CIX[2,],ylim=c(0,max(CIX)),type='n',ylab="Diameter Increment (cm)",xlab="DBH (cm)",cex.lab=1.5)
      ciEnvelope(xseq,CIX[1,],CIX[3,],col = "lightgrey")
      lines(xseq,CIX[2,],lwd=2)
      abline(h=0)
    }
    
    
    ##SDI
    hist(cov.data$SDI)
    
    # KH note: need to standardize SDIseq first since I ran the model with standardized covariates
    SDIseq.real <- seq(0,400,by=10)
    SDIseq <- (SDIseq.real-mean(temp2$SDI))/sd(temp2$SDI)
    
    incSDI <- matrix(NA,ns,length(SDIseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incSDI[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDIseq + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP
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
    
    ## SDI * size
    incSDIXhi <- matrix(NA,ns,length(SDIseq))
    incSDIXlo <- matrix(NA,ns,length(SDIseq))
    for(k in seq_along(i)){
      j <- i[k]
      incSDIXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[1] + 
        betas[j,"betaX_SDI"]*Xhl[1]*SDIseq + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
        betas[j,"betawintP.wateryr"]*wintP
      incSDIXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[2] +
        betas[j,"betaX_SDI"]*Xhl[2]*SDIseq + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
        betas[j,"betawintP.wateryr"]*wintP
    }
    CIsdiXlo <- apply(incSDIXlo,2,quantile,c(0.025,0.5,0.975))
    CIsdiXhi <- apply(incSDIXhi,2,quantile,c(0.025,0.5,0.975))
    
    # SDI plot
    SDI.DBH.Effect %<a-% {
      plot(SDIseq.real,CIsdi[2,],ylim=c(0,max(CIsdi[,1]+0.05)),type='l',xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5,lwd=3)
      #ciEnvelope(SDIseq,CIsdi[1,],CIsdi[3,],col = "lightgrey")
      lines(SDIseq.real,CIsdiXlo[2,],lwd=3,col="blue")
      lines(SDIseq.real,CIsdiXhi[2,],lwd=3,col="red")
      legend("topright",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
      abline(h=0)
    }
    
    ##SI
    hist(cov.data$SI)
    # KH note: need to standardize SDIseq first since I ran the model with standardized covariates
    SIseq.real <- 20:60
    SIseq <- (SIseq.real-mean(temp2$COND_SICOND))/sd(temp2$COND_SICOND)
    #SIseq <- 20:60
    incSI <- matrix(NA,ns,length(SDIseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incSI[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
        betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIseq + betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP
    }
    CIsi <- apply(incSI,2,quantile,c(0.025,0.5,0.975))
    
    SI.Effect %<a-% {
      plot(SIseq.real,CIsi[2,],ylim=c(0,max(CIsi)),type='n',xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
      ciEnvelope(SIseq.real,CIsi[1,],CIsi[3,],col = "lightgrey")
      lines(SIseq.real,CIsi[2,],lwd=2)
      abline(h=0)
    }
    
    
    ## SI x DBH
    incSIXlo <- matrix(NA,ns,length(SDIseq))
    incSIXmed <- matrix(NA,ns,length(SDIseq))
    incSIXhi <- matrix(NA,ns,length(SDIseq))
    for(k in seq_along(i)){
      j <- i[k]
      incSIXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
        betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*Xhl[1] + 
        betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SIseq + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
        betas[j,"betawintP.wateryr"]*wintP
      incSIXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
        betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*Xhl[2] + 
        betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SIseq + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
        betas[j,"betawintP.wateryr"]*wintP
      incSIXmed[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
        betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
        betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIseq + betas[j,"betaX_wintP.wateryr"]*x*wintP +
        betas[j,"betawintP.wateryr"]*wintP
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
    
    ## wintP
    clim.data <- readRDS("/home/rstudio/pecan/PRISM_non_scaled.rds")
    hist(time_data$wintP.wateryr)
    wintPseq.real <- 0:800
    wintPseq <- (wintPseq.real-mean(clim.data$wintP.wateryr))/sd(clim.data$wintP.wateryr)
    incP <- matrix(NA,ns,length(wintPseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incP[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
        betas[j,"betawintP.wateryr"]*wintPseq
    }
    
    CIwintP <- apply(incP,2,quantile,c(0.025,0.5,0.975))
    
    Climate.Effect %<a-% {
      plot(wintPseq.real, CIwintP[2,],ylim=c(0,max(CIwintP + 0.05)),type='n',xlab="Water Year Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
      ciEnvelope(wintPseq.real,CIwintP[1,], CIwintP[3,],col = "lightgrey")
      lines(wintPseq.real, CIwintP[2,],lwd=2)
      abline(h=0)
    }
    
    ## PRECIP x SDI
    incPSDIlo <- matrix(NA,ns,length(wintPseq))
    incPSDIhi <- matrix(NA,ns,length(wintPseq))
    for(k in seq_along(i)){
      j <- i[k]
      incPSDIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[1] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl[1]*wintPseq + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[1]*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDIhl[1] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
        betas[j,"betawintP.wateryr"]*wintPseq
      
      incPSDIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[2] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl[2]*wintPseq + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[2]*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + 
        betas[j,"betaX_SDI"]*x*SDIhl[2] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
        betas[j,"betawintP.wateryr"]*wintPseq
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
    
    ## PRECIP x DBH
    incPXlo <- matrix(NA,ns,length(wintPseq))
    incPXhi <- matrix(NA,ns,length(wintPseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incPXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*Xhl[1] + 
        betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintPseq +
        betas[j,"betawintP.wateryr"]*wintPseq
      incPXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
        betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*Xhl[2] + 
        betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintPseq +
        betas[j,"betawintP.wateryr"]*wintPseq
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
    
    ## Precip X SI
    incP_SIlo <- matrix(NA,ns,length(wintPseq))
    incP_SIhi <- matrix(NA,ns,length(wintPseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incP_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
        betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[1] + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x[1] + 
        betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintPseq +
        betas[j,"betawintP.wateryr"]*wintPseq
      incP_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
        betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[2] + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x[1] +
        betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintPseq +
        betas[j,"betawintP.wateryr"]*wintPseq
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
    
    
    
    ## SI X SDI
    
    incSDI_SIlo <- matrix(NA,ns,length(SDIseq))
    incSDI_SIhi <- matrix(NA,ns,length(SDIseq))
    
    for(k in seq_along(i)){
      j <- i[k]
      incSDI_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
        betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[1] + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x[1] + 
        betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintP +
        betas[j,"betawintP.wateryr"]*wintP
      
      incSDI_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
        betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[2] + 
        betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x[1] + 
        betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintP +
        betas[j,"betawintP.wateryr"]*wintP
    }
    CISDISIlo <- apply(incSDI_SIlo,2,quantile,c(0.025,0.5,0.975))
    CISDISIhi <- apply(incSDI_SIhi,2,quantile,c(0.025,0.5,0.975))
    
    SDI.SI.Effect %<a-% {
      plot(SDIseq.real,CISDISIhi[2,],ylim=c(0,max(CISDISIhi[2,])),type='l',lwd=3,xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5, col = "purple")
      # #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "purple")
      lines(SDIseq.real,CISDISIlo[2,],lwd=2,col="grey")
      #lines(wintPseq.real,CIwintPSIhi[2,],lwd=2,col="purple")
      legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
    }
    
    
    
    plot.effects.pryr %<a-% {
      split.screen(c(2, 2))
      # plot effects
      screen(1)
      SI.Effect
      
      screen(2)
      SDI.Effect
      screen(3)
      Tree.Size.Effect
      screen(4)
      Climate.Effect
      close.screen(all=TRUE)
    }
    
    png(height = 10, width = 6.5, units = "in", res = 200, paste0("/home/rstudio/Full_effects_",output.base.name,".png"))
    plot.effects.pryr
    dev.off()
    
    
    
    # plot interactions
    plot.interaction.effects.pryr %<a-%{
      split.screen(c(3, 2))
      screen(1)
      SI.DBH.Effect
      screen(2)
      SDI.DBH.Effect
      screen(3)
      Climate.DBH.Effect
      
      screen(4)
      Climate.SI.Effect
      screen(5)
      Climate.SDI.Effect
      screen(6)
      SDI.SI.Effect
      close.screen(all=TRUE)
    }
    
    png(height = 14, width = 6.5, units = "in", res = 200, paste0("/home/rstudio/Full_interaction_effects_", output.base.name,".png"))
    plot.interaction.effects.pryr
    dev.off()
    
    