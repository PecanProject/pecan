## C3_photomodel_Wolz
## Programmer: Kevin Wolz
## Last Updated: 4/11/12
## Support From: IB 509 Statistical Modeling Class, Professor Mike Dietze, T.A. Ryan Kelly, Student Xiaohui Feng 
##
## This script fits the Farquhar model for photosynthesis to data of soybean leaves 
## at ambient atmospheric conditions. The fit is done in a Bayesian context using the Markov Chain Monte Carlo 
## method via interface with BUGS. Data was gather using a Licor-6400 from the ambient rings at SoyFace at the 
## University of Illinois at Urbana-Champaign in Champaign, IL.  

## load libraries
library(R2WinBUGS)
library(BRugs)
library(plotrix)

## load data
dat=read.csv("Kdata_Project.csv", header=T)		## raw data to analyze
dat = dat[which(dat$id == 1),]					## select data at saturating PARi

MLE = read.csv("param_compare.csv", header=T)		## parameters from MLE analysis for comparison 

## MCMC MODEL
my.model  = function(){
	## Parameter model
	Jmax ~ dlnorm(log(100),log(25)) 	## maximum electron transport rate prior
 	Vcmax ~ dlnorm(log(180),log(35))		## maximum rubisco capacity prior
 	R ~ dlnorm(log(0.7),log(0.15))			## leaf respiration prior 
	alpha ~ dnorm(log(0.8),log(0.1))		## quantum yield (mol e-/mol photon)
	tau.FvCB ~ dgamma(0.1,0.1)				## FvCB model precision prior (weak) 
	
	for(i in 1:n){
    		## Process Model
		q2[i] <- q[i]*alpha*phi*beta
		J[i] <- (q2[i]+Jmax-sqrt((q2[i]+Jmax)*(q2[i]+Jmax)-4*theta*q2[i]*Jmax))/(2*theta)	## potential electron transport rate

     		Aj[i] <- J[i] * (ci[i]-G.star)/(4.5*ci[i]+10.5*G.star)    					## electron transport limited rate
     		Ac[i] <- Vcmax * (ci[i]-G.star)/(ci[i]+Kc*(1+po/Ko))                         	## rubisco limited rate

		Adiagnostic[i] <- Aj[i] - Ac[i]

     		pred.An[i] <- min(Aj[i], Ac[i]) - R    	## Farquhar model
     		
		## Data Model
		an[i] ~ dnorm(pred.An[i], tau.FvCB)       	## likelihood
     	pA[i] ~ dnorm(pred.An[i], tau.FvCB)    		## prediction
     	}
}

write.model(my.model,"c3model.txt")					## save model to text file

init <- list()
	init[[1]] <- list(R=1,   Vcmax=90,  alpha=0.80, tau.FvCB=10, Jmax=165)#, theta=0.65)	## chain 1 initial conditions (black)
	init[[2]] <- list(R=1.3, Vcmax=100, alpha=0.85, tau.FvCB=10, Jmax=170)#, theta=0.7)	## chain 2 initial conditions (red)
 	init[[3]] <- list(R=1.6, Vcmax=110, alpha=0.90, tau.FvCB=10, Jmax=175)#, theta=0.75)	## chain 3 initial conditions (green)

leaf.list = unique(dat$leaf)					## select leaf subset to test

c3mcmc <- list()           					## initialize output object                            

## MCMC LOOP
for(s in leaf.list){
  	sel = which(dat$leaf == s)				## pick leaf for this loop
  	
	an = dat$Photo[sel]						## define net photosynthesis
  	ci = dat$Ci[sel]						## define intracellular [CO2]
  	q = dat$PARi[sel]						## define incident radiation

  	mydat <- list(an=an, ci=ci, q=q, n=length(an), Kc=400, Ko=275, po=210, phi=0.85, beta=0.5, theta=0.7, G.star=44.25) ## data for current leaf
 
 	c3 <- openbugs(mydat,					## data
 			init,							## initial conditions
 			model.file = "c3model.txt",		## model
 			n.chains = 3,					## number of chains
 			n.iter = 100000,					## number of iterations
 			n.burnin = 50000,				## burn in
 			n.thin = 20,					## thin
 			parameters.to.save = c("R", "Vcmax", "alpha", "Jmax", "tau.FvCB", "pred.An", "pA", "Adiagnostic") ## parameters to save
 			)                              
 	
	c3mcmc[[s]] = c3						## save output object
	print(s)
}

## OUTPUT ANALYSIS
c3mcmcV <- list()
c3mcmcJ <- list()   
c3mcmcR <- list()   
c3mcmcalpha <- list()   
c3mcmctauFvCB <- list() 

c3mcmcV.sd <- list()
c3mcmcJ.sd <- list()   
c3mcmcR.sd <- list()   
c3mcmcalpha.sd <- list()   
c3mcmctauFvCB.sd <- list() 

i = 1		## counter 

## data summary
sink("FvCB_MCMC_Summary.txt") 
for(j in leaf.list){
	print(j)
	print(c3mcmc[[j]])} 
sink() 

for(s in leaf.list){
	## select data for each leaf
	sel1 = which(dat$leaf == s)

	## trace and density plots	
	pdf(paste("Leaf_",s,"_FvCB_Model_Trace.pdf",sep=""))
	plot(as.mcmc.list(c3mcmc[[s]]))
	dev.off()

	## predictions vs measurements
	an = dat$Photo[sel1]
	axismax = max(max(an),max(c3mcmc[[s]]$mean$pred.An))
	pdf(paste("Leaf_",s,"_FvCB_Pred-vs-Meas.pdf",sep=""))
	plot(an,
		c3mcmc[[s]]$mean$pred.An, 
		ylim = c(-5,60),
		xlim = c(-5,60), 
		pch = 19,
		main = "Predicted An vs measured An", 
		xlab = "Measured An (umol m-2 s-1)", 
		ylab = "Predicted An (umol m-2 s-1)",
		cex.main = 1.6,
		cex.lab = 1.4)	
	abline(0, 1, col = "dark green", lwd = 3)
     	dev.off()

	## mcmc means of each leaf
	c3mcmcV[i] = c3mcmc[[s]]$mean$Vcmax
	c3mcmcJ[i] = c3mcmc[[s]]$mean$Jmax
	c3mcmcR[i] = c3mcmc[[s]]$mean$R
	c3mcmcalpha[i] = c3mcmc[[s]]$mean$alpha
	c3mcmctauFvCB[i] = c3mcmc[[s]]$mean$tau.FvCB

	## mcmc sd of each leaf
	c3mcmcV.sd[i] = c3mcmc[[s]]$sd$Vcmax
	c3mcmcJ.sd[i] = c3mcmc[[s]]$sd$Jmax
	c3mcmcR.sd[i] = c3mcmc[[s]]$sd$R
	c3mcmcalpha.sd[i] = c3mcmc[[s]]$sd$alpha
	c3mcmctauFvCB.sd[i] = c3mcmc[[s]]$sd$tau.FvCB
	i = i + 1
}
	
	c3mcmcV = as.numeric(c3mcmcV)
	c3mcmcJ = as.numeric(c3mcmcJ)
	c3mcmcR = as.numeric(c3mcmcR)
	c3mcmcalpha = as.numeric(c3mcmcalpha)
	c3mcmctauFvCB = as.numeric(c3mcmctauFvCB)

	c3mcmcV.sd = as.numeric(c3mcmcV.sd)
	c3mcmcJ.sd = as.numeric(c3mcmcJ.sd)
	c3mcmcR.sd = as.numeric(c3mcmcR.sd)
	c3mcmcalpha.sd = as.numeric(c3mcmcalpha.sd)
	c3mcmctauFvCB.sd = as.numeric(c3mcmctauFvCB.sd)
	
output <- data.frame(c3mcmcV, c3mcmcV.sd, c3mcmcJ, c3mcmcJ.sd, c3mcmcalpha, c3mcmcalpha.sd, c3mcmcR, c3mcmcR.sd, c3mcmctauFvCB, c3mcmctauFvCB.sd)
write.csv(output, file = "FvCB_output.csv")

	## MLE means of each leaf
	MLEV = MLE$Katie.Vcmax 
	MLEJ = MLE$Katie.Jmax

	## compare mcmc means to Manual means for Vcmax
	pdf(paste("Mean_Comparison-Vcmax.pdf",sep=""))
	plot(MLEV, 
		c3mcmcV,
		ylim = c(50,150),
		xlim = c(50,150), 
		pch = 19,
		main = "MCMC Vcmax Means vs Manual Vcmax Fit", 
		xlab = "Manual Vcmax Fit (umol m-2 s-1)", 
		ylab = "MCMC Vcmax (umol m-2 s-1)",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(MLEV,c3mcmcV,c3mcmcV.sd,add=TRUE)
	dev.off()

	## compare mcmc means to Manual means for Jmax
	pdf(paste("Mean_Comparison-Jmax.pdf",sep=""))
	plot(MLEJ, 
		c3mcmcJ,
		ylim = c(100,250),
		xlim = c(100,250), 
		pch = 19,
		main = "MCMC Jmax Means vs Maunal Jmax Fit", 
		xlab = "Manual Jmax Fit (umol m-2 s-1)", 
		ylab = "MCMC Jmax (umol m-2 s-1)",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(MLEJ,c3mcmcJ,c3mcmcJ.sd,add=TRUE)
	dev.off()


## Analysis with MCMC Object
Kc=400
Ko=275
po=210
f=0.15
phi=0.85
beta=0.5
theta=0.7
G.star=44.25

for(s in leaf.list){
	## select data for each leaf
	sel2 = which(dat$leaf == s)
	an = dat$Photo[sel2]						## define net photosynthesis
  	ci = dat$Ci[sel2]						## define intracellular [CO2]
  	q = dat$PARi[sel2]						## define incident radiation

	## convert output chains to mcmc objects
	r <- c3mcmc[[s]]$sims.list$R
	v <- c3mcmc[[s]]$sims.list$Vcmax
	a <- c3mcmc[[s]]$sims.list$alpha
	j <- c3mcmc[[s]]$sims.list$Jmax
	t <- c3mcmc[[s]]$sims.list$tau.FvCB

	R <- mcmc(r)
	V <- mcmc(v)
	A <- mcmc(a)
	J <- mcmc(j)
	T <- mcmc(t)
	n <- length(c3mcmc[[s]]$sims.list$R)

	df <- data.frame(r,v,a,j,g,t)

	## parameter pairs plot of each leaf
	jpeg(paste("Leaf_",s,"_FvCB_Params_Corr_Plot.jpeg",sep=""))
	pairs(df,c("Rd","Vcmax","alpha","Jmax","Model Prec"))
	dev.off() 
	
	## autocorrelation plots of each leaf
	pdf(paste("Leaf_",s,"_FvCB_Vcmax_Autocorr.pdf",sep=""))
	autocorr.plot(V)
	dev.off()

	pdf(paste("Leaf_",s,"_FvCB_Jmax_Autocorr.pdf",sep=""))
	autocorr.plot(J)
	dev.off()

	## parameter correlation plots of each leaf
	pdf(paste("Leaf_",s,"_FvCB_Param_Corr.pdf",sep=""))
	plot(c3mcmc[[s]]$sims.list$Vcmax,
		c3mcmc[[s]]$sims.list$Jmax,
		pch = 19,
		main = "FvCB Parameter Correlation", 
		xlab = "Vcmax", 
		ylab = "Jmax",
		cex.main = 1.6,
		cex.lab = 1.4)
	dev.off()

	## credible and prediction intervals
	sorted = sort.int(ci,index.return=TRUE)
	index = sorted$ix
	xpred <- ci[index]					## sequence of x values to make predictions at
	npred <- length(xpred)
	ypred <- matrix(NA,nrow=n,ncol=npred)		## storage for prediction interval
	ycred <- matrix(NA,nrow=n,ncol=npred)		## storage for credible interval
	
	q2 <- matrix(NA,1,npred)
	Jm <- matrix(NA,1,npred)
	Aj <- matrix(NA,1,npred)
	Ac <- matrix(NA,1,npred)
	for(g in 1:n){
		q2[1,] <- q[index]*A[g]*(1-f)/2
		Jm[1,] <- (q2[1,]+J[g]-sqrt((q2[1,]+J[g])*(q2[1,]+J[g])-4*theta*q2[1,]*J[g]))/(2*theta)	## potential electron transport rate

    		Aj[1,] <- Jm[1,] * (xpred-G.star)/(4.5*xpred+10.5*G[g])    	## electron transport limited rate
    		Ac[1,] <- V[g] * (xpred-G.star)/(xpred+Kc*(1+po/Ko))     ## rubisco limited rate

    		Ey <- pmin(Aj[1,], Ac[1,]) - R[g]    		## Farquhar model
    		ycred[g,] <- Ey
    		ypred[g,] <- rnorm(npred,Ey,sqrt(1/T[g]))
	}

	credi <- apply(ycred,2,quantile,c(0.025,0.5,0.975))## credible interval and median
	predi <- apply(ypred,2,quantile,c(0.025,0.975))	## prediction interval
	
	pdf(paste("Leaf_",s,"_FvCB_Plot_Fit.pdf",sep=""))
	plot(ci,
		an,
		ylim = c(0,35),
		xlim = c(0,1300), 
		pch = 19,
		main = "FvCB Model Fit", 
		xlab = "Intracellular [CO2] (Ci) (ppm)", 
		ylab = "Net Photosynthesis (An) (umol m-2 s-1)",
		cex.main = 1.6,
		cex.lab = 1.4)

	lines(xpred,credi[1,],col=3,lty=2)			## lower CI
	lines(xpred,credi[2,],col=3,lwd=2)			## median
	lines(xpred,credi[3,],col=3,lty=2)			## upper CI
	lines(xpred,predi[1,],col=4,lty=2)			## lower PI
	lines(xpred,predi[2,],col=4,lty=2)			## upper PI

	legend(700, 5, 
		c("MCMC Fit", "95% Credible Interval", "95% Predictive Interval"), 
		col = c(3,3,4),
     	lty = c(1, 2, 2))
	dev.off()
}