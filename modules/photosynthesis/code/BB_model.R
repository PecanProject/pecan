## BB_model_Wolz
## Programmer: Kevin Wolz
## Last Updated: 5/07/12
## Support From: IB 509 Statistical Modeling Class, Professor Mike Dietze, T.A. Ryan Kelly, Student Xiaohui Feng 
##
## This script fits the Ball-Berry model for stomatal conductance to data of soybean leaves 
## at ambient atmospheric conditions. The fit is done in a Bayesian context using the Markov Chain Monte Carlo 
## method via interface with BUGS. Data was gather using a Licor-6400 from the ambient rings at SoyFace at the 
## University of Illinois at Urbana-Champaign in Champaign, IL.  

## load libraries
library(R2WinBUGS)
library(BRugs)
library(plotrix)

## load data
dat = read.csv("Kdata_Project.csv", header=T)		## raw data to analyze
dat = dat[which(dat$id == 1),]					## select data at saturating PARi

MLE = read.csv("param_compare.csv", header=T)		## parameters from MLE analysis for comparison 

## MCMC MODEL
my.model  = function(){
	## Parameter model
	g0 ~ dlnorm(1e-10,10000)			## BB intercept prior (weak)
	m ~ dnorm(10,0.1)					## BB slope prior (weak)
	tau.BB ~ dgamma(0.1,0.00001)		## BB model precision prior (weak)

	for(i in 1:n){
    	## Process Model
		pred.gs[i] <- g0 + m*an[i]*H[i]/ca[i]		## Ball-Berry model
		
		## Data Model
		gs[i] ~ dnorm(pred.gs[i], tau.BB)       	## likelihood
    	pG[i] ~ dnorm(pred.gs[i], tau.BB)    		## prediction
     	}
}

write.model(my.model,"BBmodel.txt")				## save model to text file

## MCMC INITIALIZATION & DATA SELECTION
init <- list()
	init[[1]] <- list(m=8, 	g0=-0.01, tau.BB=2000)	## chain 1 initial conditions
	init[[2]] <- list(m=10, g0=0, 	tau.BB=1000) 	## chain 2 initial conditions
 	init[[3]] <- list(m=12, g0=0.1, tau.BB=1500)    ## chain 3 initial conditions
 
leaf.list = unique(dat$leaf)					## select leaf subset to test

BBmcmc <- list()         					## initialize output object                              

## MCMC LOOP	
for(s in leaf.list){
  	sel = which(dat$leaf == s)				## pick leaf for this loop
 	
	an = dat$Photo[sel]					## define net photosynthesis
	ca = dat$CO2S[sel]					## define atmospheric [CO2]
	H = dat$RH_S[sel]/100					## define relative humidity
	gs = dat$Cond[sel]					## define stomatal conductance

  	mydat <- list(an=an, n=length(an), gs=gs, ca=ca, H=H)	## data list for current leaf
 
 	BB <- openbugs(mydat,					## data
 			init,						## initial conditions
 			model.file = "BBmodel.txt",		## model
 			n.chains = 3,				## number of chains
 			n.iter = 50000,				## number of iterations
 			n.burnin = 10000,				## burn in
 			n.thin = 20,				## thin
 			parameters.to.save = c("g0", "m", "tau.BB", "pred.gs", "pG") ## parameters to save
 			)                              
 	
	BBmcmc[[s]] = BB						## save output object
}

## OUTPUT ANALYSIS
mcmcg0 <- list()
mcmcm <- list()
mcmcg0.sd <- list()
mcmcm.sd <- list()
mcmctauBB <- list()
mcmctauBB.sd <- list()
i = 1		## counter 

## data summary
sink("BB_MCMC_Summary.txt") 
for(j in leaf.list){
	print(j)
	print(BBmcmc[[j]])} 
sink() 

for(s in leaf.list){
	## select data for each leaf
	sel1 = which(dat$leaf == s)

	## trace and density plots	
	pdf(paste("Leaf_",s,"_BB_Model_Trace.pdf",sep=""))
	plot(as.mcmc.list(BBmcmc[[s]]))
	dev.off()

	## predictions vs measurements
	gs = dat$Cond[sel1]
	axismax = max(max(gs),max(BBmcmc[[s]]$mean$pred.gs))
	pdf(paste("Leaf_",s, "_BB_Pred-vs-Meas.pdf",sep=""))
	plot(gs,
		BBmcmc[[s]]$mean$pred.gs, 
		ylim = c(0,axismax),
		xlim = c(0,axismax), 
		pch = 19,
		main = "Predicted gs vs Measured gs", 
		xlab = "Measured gs (mol m-2 s-1)", 
		ylab = "Predicted gs (mol m-2 s-1)",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
     	dev.off()

	## mcmc means of each leaf
	mcmcg0[i] = BBmcmc[[s]]$mean$g0
	mcmcm[i] = BBmcmc[[s]]$mean$m
	mcmctauBB[i] = BBmcmc[[s]]$mean$tau.BB 
	mcmcg0.sd[i] = BBmcmc[[s]]$sd$g0
	mcmcm.sd[i] = BBmcmc[[s]]$sd$m
	mcmctauBB.sd[i] = BBmcmc[[s]]$sd$tau.BB
	i = i + 1
}
	mcmcg0 = as.numeric(mcmcg0)
	mcmcm = as.numeric(mcmcm)
	mcmctauBB = as.numeric(mcmctauBB)
	mcmcg0.sd = as.numeric(mcmcg0.sd)
	mcmcm.sd = as.numeric(mcmcm.sd)
	mcmctauBB.sd = as.numeric(mcmctauBB.sd)

output <- data.frame(mcmcm, mcmcm.sd, mcmcg0, mcmcg0.sd, mcmctauBB, mcmctauBB.sd)	
write.csv(output, file = "BB_output.csv")

	## MLE means of each leaf
	MLEg0 = MLE$g0.nobound 
	MLEm = MLE$m.nobound

	## compare mcmc means to MLE means for g0
	pdf(paste("Mean_Comparison-g0.pdf",sep=""))
	plot(MLEg0, 
		mcmcg0,
		ylim = c(-0.2,0.2),
		xlim = c(-0.2,0.2), 
		pch = 19,
		main = "MCMC g0 Means vs MLE g0 Means", 
		xlab = "MLE g0 (mol m-2 s-1)", 
		ylab = "MCMC g0 (mol m-2 s-1)",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(MLEg0,mcmcg0,mcmcg0.sd,add=TRUE)
	dev.off()

	## compare mcmc means to MLE means for m
	pdf(paste("Mean_Comparison-m.pdf",sep=""))
	plot(MLEm, 
		mcmcm,
		ylim = c(5,20),
		xlim = c(5,20), 
		pch = 19,
		main = "MCMC m Means vs MLE m Means", 
		xlab = "MLE m", 
		ylab = "MCMC m",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(MLEm,mcmcm,mcmcm.sd,add=TRUE)
	dev.off()

## Analysis with MCMC Object
for(s in leaf.list){
	## select data for each leaf
	sel2 = which(dat$leaf == s)
	an = dat$Photo[sel2]					## define net photosynthesis
	ca = dat$CO2S[sel2]					## define atmospheric [CO2]
	H = dat$RH_S[sel2]/100					## define relative humidity
	gs = dat$Cond[sel2]					## define stomatal conductance

	## convert output chains to mcmc objects
	M <- mcmc(BBmcmc[[s]]$sims.list$m)
	G0 <- mcmc(BBmcmc[[s]]$sims.list$g0)
	T <- mcmc(BBmcmc[[s]]$sims.list$tau.BB)
	n <- length(BBmcmc[[s]]$sims.list$g0)
	
	## autocorrelation plots of each leaf
	pdf(paste("Leaf_",s,"_BB_m_Autocorr.pdf",sep=""))
	autocorr.plot(M)
	dev.off()

	pdf(paste("Leaf_",s,"_BB_g0_Autocorr.pdf",sep=""))
	autocorr.plot(G0)
	dev.off()

	## parameter correlation plots of each leaf
	pdf(paste("Leaf_",s,"_BB_Param_Corr.pdf",sep=""))
	plot(BBmcmc[[s]]$sims.list$g0,
		BBmcmc[[s]]$sims.list$m,
		pch = 19,
		main = "BB Parameter Correlation", 
		xlab = "g0", 
		ylab = "m",
		cex.main = 1.6,
		cex.lab = 1.4)
	dev.off()

	## credible and prediction intervals
	xpred <- seq(0,0.055,0.001)				## sequence of x values to make predictions at
	npred <- length(xpred)
	ypred <- matrix(NA,nrow=n,ncol=npred)		## storage for prediction interval
	ycred <- matrix(NA,nrow=n,ncol=npred)		## storage for credible interval

	for(g in 1:n){
   		Ey <- G0[g] + M[g]*xpred
    		ycred[g,] <- Ey
    		ypred[g,] <- rnorm(npred,Ey,sqrt(1/T[g]))
	}

	ci <- apply(ycred,2,quantile,c(0.025,0.5,0.975))## credible interval and median
	pi <- apply(ypred,2,quantile,c(0.025,0.975))	## prediction interval
	
	pdf(paste("Leaf_",s,"_BB_Plot_Fit.pdf",sep=""))
	plot(an*H/ca,
		gs,
		ylim = c(-0.5,1),
		xlim = c(0,0.055), 
		pch = 19,
		main = "BB Model Fit", 
		xlab = "A*H/Ca", 
		ylab = "Stomatal Conductance (gs) (mol m-2 s-1)",
		cex.main = 1.6,
		cex.lab = 1.4)

	lines(xpred,ci[1,],col=3,lty=2)			## lower CI
	lines(xpred,ci[2,],col=3,lwd=2)			## median
	lines(xpred,ci[3,],col=3,lty=2)			## upper CI
	lines(xpred,pi[1,],col=4,lty=2)			## lower PI
	lines(xpred,pi[2,],col=4,lty=2)			## upper PI

	legend(0, 1, 
		c("MCMC Fit", "95% Credible Interval", "95% Predictive Interval"), 
		col = c(3,3,4),
      	lty = c(1, 2, 2))
	dev.off()
}
