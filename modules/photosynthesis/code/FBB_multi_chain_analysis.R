### FBB_analysis
## Required library
library(coda)

## make variable to store acceptance stats
accept = list()

## select indices of interations to use in analysis
keep = seq(start,end,by=thin)

## Plot tracked plottable parameters. Otherwise, save them as NULL.

## Parameter Vcmax
if(exists('Vcmax.mcmc') && !is.null(Vcmax.mcmc)) { 
	mcmc.Vcmax = mcmc.list(
				mcmc(Vcmax.mcmc[keep,1],start,end,thin),
				mcmc(Vcmax.mcmc[keep,2],start,end,thin),
				mcmc(Vcmax.mcmc[keep,3],start,end,thin))
	pdf(paste(SaveLeafDir, runName, "_plot.Vcmax.pdf",sep=""))
	plot(mcmc.Vcmax, main="Vcmax")
	dev.off()
	accept$Vcmax = sum((diff(Vcmax.mcmc)!=0))/(niter*nchains-1)
} else {
	Vcmax.mcmc = NULL
	accept$Vcmax = NULL
}

## Parameter Jmax
if(exists('Jmax.mcmc') && !is.null(Jmax.mcmc)) { 
	mcmc.Jmax = mcmc.list(
				mcmc(Jmax.mcmc[keep,1],start,end,thin),
				mcmc(Jmax.mcmc[keep,2],start,end,thin),
				mcmc(Jmax.mcmc[keep,3],start,end,thin))
	pdf(paste(SaveLeafDir, runName, "_plot.Jmax.pdf",sep=""))
	plot(mcmc.Jmax, main="Jmax")
	dev.off()
	accept$Jmax = sum((diff(Jmax.mcmc)!=0))/(niter*nchains-1)
} else {
	Jmax.mcmc = NULL
	accept$Jmax = NULL
}

## Parameter R
if(exists('R.mcmc') && !is.null(R.mcmc)) { 
	mcmc.R = mcmc.list(
				mcmc(R.mcmc[keep,1],start,end,thin),
				mcmc(R.mcmc[keep,2],start,end,thin),
				mcmc(R.mcmc[keep,3],start,end,thin)) 
	pdf(paste(SaveLeafDir, runName, "_plot.R.pdf",sep=""))
	plot(mcmc.R, main="R")
	dev.off()
	accept$R = sum((diff(R.mcmc)!=0))/(niter*nchains-1)
} else {
	R.mcmc = NULL
	accept$R = NULL
}

## Parameter Gstar
if(exists('Gstar.mcmc') && !is.null(Gstar.mcmc)) { 
	mcmc.Gstar = mcmc.list(
				mcmc(Gstar.mcmc[keep,1],start,end,thin),
				mcmc(Gstar.mcmc[keep,2],start,end,thin),
				mcmc(Gstar.mcmc[keep,3],start,end,thin)) 
	pdf(paste(SaveLeafDir, runName, "_plot.Gstar.pdf",sep=""))
	plot(mcmc.Gstar, main="Gstar")
	dev.off()
	accept$Gstar = sum((diff(Gstar.mcmc)!=0))/(niter*nchains-1)
} else {
	Gstar.mcmc = NULL
	accept$Gstar = NULL
}

## Parameter alpha
if(exists('alpha.mcmc') && !is.null(alpha.mcmc)) { 
	mcmc.alpha = mcmc.list(
				mcmc(alpha.mcmc[keep,1],start,end,thin),
				mcmc(alpha.mcmc[keep,2],start,end,thin),
				mcmc(alpha.mcmc[keep,3],start,end,thin))  
	pdf(paste(SaveLeafDir, runName, "_plot.alpha.pdf",sep=""))
	plot(mcmc.alpha, main="alpha")
	dev.off()
	accept$alpha = sum((diff(alpha.mcmc)!=0))/(niter*nchains-1)
} else {
	alpha.mcmc = NULL
	accept$alpha = NULL
}

## Parameter m
if(exists('m.mcmc') && !is.null(m.mcmc)) { 
	mcmc.m = mcmc.list(
				mcmc(m.mcmc[keep,1],start,end,thin),
				mcmc(m.mcmc[keep,2],start,end,thin),
				mcmc(m.mcmc[keep,3],start,end,thin))  
	pdf(paste(SaveLeafDir, runName, "_plot.m.pdf",sep=""))
	plot(mcmc.m, main="m")
	dev.off()
	accept$m = sum((diff(m.mcmc)!=0))/(niter*nchains-1)
} else {
	m.mcmc = NULL
	accept$m = NULL
}

## Parameter g0
if(exists('g0.mcmc') && !is.null(g0.mcmc)) { 
	mcmc.g0 = mcmc.list(
				mcmc(g0.mcmc[keep,1],start,end,thin),
				mcmc(g0.mcmc[keep,2],start,end,thin),
				mcmc(g0.mcmc[keep,3],start,end,thin)) 
	pdf(paste(SaveLeafDir, runName, "_plot.g0.pdf",sep=""))
	plot(mcmc.g0, main="g0")
	dev.off()
	accept$g0 = sum((diff(g0.mcmc)!=0))/(niter*nchains-1)
} else {
	g0.mcmc = NULL
	accept$g0 = NULL
}

## Parameter tauF
if(exists('tauF.mcmc') && !is.null(tauF.mcmc)) { 
	mcmc.tauF = mcmc.list(
				mcmc(tauF.mcmc[keep,1],start,end,thin),
				mcmc(tauF.mcmc[keep,2],start,end,thin),
				mcmc(tauF.mcmc[keep,3],start,end,thin)) 
	pdf(paste(SaveLeafDir, runName, "_plot.tauF.pdf",sep=""))
	plot(mcmc.tauF, main="tauF")
	dev.off()
} else {
	tauF.mcmc = NULL
}

## Parameter tauBB
if(exists('tauBB.mcmc') && !is.null(tauBB.mcmc)) { 
	mcmc.tauBB = mcmc.list(
				mcmc(tauBB.mcmc[keep,1],start,end,thin),
				mcmc(tauBB.mcmc[keep,2],start,end,thin),
				mcmc(tauBB.mcmc[keep,3],start,end,thin))  
	pdf(paste(SaveLeafDir, runName, "_plot.tauBB.pdf",sep=""))
	plot(mcmc.tauBB, main="tauBB")
	dev.off()
} else {
	tauBB.mcmc = NULL
}

## Parameter An.pred
if(exists('An.pred.mcmc') && !is.null(An.pred.mcmc)) {
	An.ave = An.pred.mcmc[1:niter,]
	if(nchains>1){
		for(c in 2:nchains){
			An.ave = (An.ave+An.pred.mcmc[(1+niter*(c-1)):(niter*c),])/2
		}
	}
	mcmc.An.pred = mcmc(An.ave[keep,],start,end,thin) 
	for(a in 1:npoints) {
		pdf(paste(SaveLeafDir, runName, "_plot.An.pred", a, ".pdf",sep=""))
		plot(mcmc.An.pred[,a], main=paste("An.pred[",a,"]",sep=""))
		dev.off()
	}
} else {
	An.pred.mcmc = NULL
}

## Parameter gs.pred
if(exists('gs.pred.mcmc') && !is.null(gs.pred.mcmc)) {
	gs.ave = gs.pred.mcmc[1:niter,]
	if(nchains>1){
		for(c in 2:nchains){
			gs.ave = (gs.ave+gs.pred.mcmc[(1+niter*(c-1)):(niter*c),])/2
		}
	}
	mcmc.gs.pred = mcmc(gs.ave[keep,],start,end,thin)  
	for(a in 1:npoints) {
		pdf(paste(SaveLeafDir, runName, "_plot.gs.pred", a, ".pdf",sep=""))
		plot(mcmc.gs.pred[,a], main=paste("gs.pred[",a,"]",sep=""))
		dev.off()
	}
} else {
	gs.pred.mcmc = NULL
}

## Get summary of all variables
summary.Vcmax = summary(mcmc.Vcmax)
summary.Jmax = summary(mcmc.Jmax)
summary.R = summary(mcmc.R)
summary.Gstar = summary(mcmc.Gstar)
summary.alpha = summary(mcmc.alpha)
summary.m = summary(mcmc.m)
summary.g0 = summary(mcmc.g0)
summary.tauF = summary(mcmc.tauF)
summary.tauBB = summary(mcmc.tauBB)
summary.An.pred = summary(mcmc.An.pred)
summary.gs.pred = summary(mcmc.gs.pred)

## Compute means & intervals for all variables
mean.Vcmax = summary.Vcmax$statistics[1]
mean.Jmax = summary.Jmax$statistics[1]
mean.R = summary.R$statistics[1]
mean.Gstar = summary.Gstar$statistics[1]
mean.alpha = summary.alpha$statistics[1]
mean.m = summary.m$statistics[1]
mean.g0 = summary.g0$statistics[1]
mean.tauF = summary.tauF$statistics[1]
mean.tauBB = summary.tauBB$statistics[1]

sd.Vcmax = summary.Vcmax$statistics[2]
sd.Jmax = summary.Jmax$statistics[2]
sd.R = summary.R$statistics[2]
sd.Gstar = summary.Gstar$statistics[2]
sd.alpha = summary.alpha$statistics[2]
sd.m = summary.m$statistics[2]
sd.g0 = summary.g0$statistics[2]
sd.tauF = summary.tauF$statistics[2]
sd.tauBB = summary.tauBB$statistics[2]

q25.Vcmax = summary.Vcmax$quantiles[1]
q25.Jmax = summary.Jmax$quantiles[1]
q25.R = summary.R$quantiles[1]
q25.Gstar = summary.Gstar$quantiles[1]
q25.alpha = summary.alpha$quantiles[1]
q25.m = summary.m$quantiles[1]
q25.g0 = summary.g0$quantiles[1]
q25.tauF = summary.tauF$quantiles[1]
q25.tauBB = summary.tauBB$quantiles[1]


q975.Vcmax = summary.Vcmax$quantiles[5]
q975.Jmax = summary.Jmax$quantiles[5]
q975.R = summary.R$quantiles[5]
q975.Gstar = summary.Gstar$quantiles[5]
q975.alpha = summary.alpha$quantiles[5]
q975.m = summary.m$quantiles[5]
q975.g0 = summary.g0$quantiles[5]
q975.tauF = summary.tauF$quantiles[5]
q975.tauBB = summary.tauBB$quantiles[5]

mean.pred.An = numeric()
mean.pred.gs = numeric()
sd.pred.An = numeric()
sd.pred.gs = numeric()
q25.pred.An = numeric()
q25.pred.gs = numeric()
q975.pred.An = numeric()
q975.pred.gs = numeric()

for(p in 1:npoints){
	mean.pred.An[p] = summary.An.pred$statistics[p,1]
	mean.pred.gs[p] = summary.gs.pred$statistics[p,1]
	sd.pred.An[p] = summary.An.pred$statistics[p,2]
	sd.pred.gs[p] = summary.gs.pred$statistics[p,2]
	q25.pred.An[p] = summary.An.pred$quantiles[p,1]
	q25.pred.gs[p] = summary.gs.pred$quantiles[p,1]
	q975.pred.An[p] = summary.An.pred$quantiles[p,5]
	q975.pred.gs[p] = summary.gs.pred$quantiles[p,5]
}



## predictions vs measurements
axismin = min(min(An.obs),min(mean.pred.An))
axismax = max(max(An.obs),max(mean.pred.An))
pdf(paste(SaveLeafDir, runName, "_An_Pred-vs-Meas.pdf",sep=""))
plot(An.obs,
	mean.pred.An, 
	ylim = c(axismin,axismax),
	xlim = c(axismin,axismax), 
	pch = 19,
	main = "Predicted An vs Measured An", 
	xlab = "Measured An (umol m-2 s-1)", 
	ylab = "Predicted An (umol m-2 s-1)",
	cex.main = 1.6,
	cex.lab = 1.4)	
	abline(0, 1, col = "dark green", lwd = 3)
dev.off()



axismin = min(min(gs.obs),min(mean.pred.gs))
axismax = max(max(gs.obs),max(mean.pred.gs))
pdf(paste(SaveLeafDir, runName, "_gs_Pred-vs-Meas.pdf",sep=""))
plot(gs.obs,
	mean.pred.gs, 
	ylim = c(axismin,axismax),
	xlim = c(axismin,axismax), 
	pch = 19,
	main = "Predicted gs vs Measured gs", 
	xlab = "Measured gs (mol m-2 s-1)", 
	ylab = "Predicted gs (mol m-2 s-1)",
	cex.main = 1.6,
	cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
dev.off()



## autocorrelation plots of each leaf
pdf(paste(SaveLeafDir, runName, "_Autocorr_Vcmax.pdf",sep=""))
	autocorr.plot(mcmc.Vcmax, main="Vcmax Autocorrelation")
dev.off()

pdf(paste(SaveLeafDir, runName, "_Autocorr_Jmax.pdf",sep=""))
	autocorr.plot(mcmc.Jmax, main="Jmax Autocorrelation")
dev.off()



## Farquhar CI and PI
if(exists('An.pred.mcmc') && !is.null(An.pred.mcmc)) { 
	pdf(paste(SaveLeafDir, runName, "_model_Farquhar.pdf",sep=""))
	ci.pred = Ca-mean.pred.An/mean.pred.gs
		cimax = max(q975.pred.An)
		cimin = min(q25.pred.An)
	pi.An <- apply(pA.mcmc,2,quantile,c(0.025,0.5,0.975))
		pimax = max(pi.An[3,])
		pimin = min(pi.An[1,])
	ylim 	= min((sign(pimin)*0.75*abs(pimin)),(sign(cimin)*0.75*abs(cimin)))
	ylim[2]	= max((sign(pimax)*1.25*abs(pimax)),(sign(cimax)*1.25*abs(cimax)))
	xlim 	= 0
	xlim[2] = max(Ci)+100
	
	plot(Ci,
		An.obs,
		ylim = ylim,
		xlim = xlim, 
		pch = 19,
		main = "Farquhar Coupled Model Fit", 
		xlab = "Intracellular [CO2] (Ci) (ppm)", 
		ylab = "Net Photosynthesis (An) (umol m-2 s-1)",
		cex.main = 1.6,
		cex.lab = 1.4)
	lines(ci.pred[index],mean.pred.An[index],col=3,lwd=2)
	lines(ci.pred[index],q25.pred.An[index],col=3,lty=2)
	lines(ci.pred[index],q975.pred.An[index],col=3,lty=2)
	lines(ci.pred[index],pi.An[1,index],col=4,lty=2)
	lines(ci.pred[index],pi.An[3,index],col=4,lty=2)	
	
	legend(700, (ylim[1]+10), 
		c("MCMC Fit", "95% Credible Interval", "95% Predictive Interval"), 
		col = c(3,3,4),
     	lty = c(1, 2, 2))
     	
	dev.off()
}



## Ball-Berry CI and PI
if(exists('gs.pred.mcmc') && !is.null(gs.pred.mcmc)) { 
	pdf(paste(SaveLeafDir, runName, "_model_BallBerry.pdf",sep=""))
	BBx = mean.pred.An*H/Ca
		cimax = max(q975.pred.gs)
		cimin = min(q25.pred.gs)
	pi.gs <- apply(pgs.mcmc,2,quantile,c(0.025,0.5,0.975))
		pimax = max(pi.gs[3,])
		pimin = min(pi.gs[1,])
	ylim 	= min((sign(pimin)*0.75*abs(pimin)),(sign(cimin)*0.75*abs(cimin)))
	ylim[2]	= max((sign(pimax)*1.25*abs(pimax)),(sign(cimax)*1.25*abs(cimax)))
	xlim 	= min(An.obs*H/Ca) - 0.005
	xlim[2]	= max(An.obs*H/Ca) + 0.005
	
	plot(An.obs*H/Ca,
		gs.obs,
		ylim = ylim,
		xlim = xlim, 
		pch = 19,
		main = "Ball-Berry Coupled Model Fit", 
		xlab = "A*H/Ca", 
		ylab = "Stomatal Conductance (gs) (mol m-2 s-1)",
		cex.main = 1.6,
		cex.lab = 1.4)
		
	sorted = sort.int(An.obs*H/Ca,index.return=TRUE)
	BBx.index = sorted$ix
	
	lines(BBx[BBx.index],mean.pred.gs[BBx.index],col=3,lwd=2)
	lines(BBx[BBx.index],q25.pred.gs[BBx.index],col=3,lty=2)
	lines(BBx[BBx.index],q975.pred.gs[BBx.index],col=3,lty=2)
	lines(BBx[BBx.index],pi.gs[1, BBx.index],col=4,lty=2)
	lines(BBx[BBx.index],pi.gs[3, BBx.index],col=4,lty=2)	
	
	legend(0, ylim[2], 
		c("MCMC Fit", "95% Credible Interval", "95% Predictive Interval"), 
		col = c(3,3,4),
     	lty = c(1, 2, 2))
     	
	dev.off()
}



## DIC calculation
if(exists('DA.mcmc')) 		{ 
	An.pred.mean = colMeans(mcmc.An.pred,1)
	tauF.mean = mean(tauF.mcmc)
	
	DthetaBar 	= -2*llik.A(An.pred.mean,tauF.mean)
	Dbar		= mean(DA.mcmc[keep,])
	DIC.An 		= 2*Dbar - DthetaBar
} else { 	
	DA.mcmc=NULL	
}

if(exists('Dgs.mcmc')) 		{ 
	gs.pred.mean = colMeans(mcmc.gs.pred,1)
	tauBB.mean = mean(tauBB.mcmc)
	
	DthetaBar 	= -2*llik.gs(gs.pred.mean,tauBB.mean)
	Dbar		= mean(Dgs.mcmc[keep,])
	DIC.gs 		= 2*Dbar - DthetaBar
} else { 	
	Dgs.mcmc=NULL	
}



## parameter pairs plot
df = data.frame(Vcmax.mcmc[keep,1], Jmax.mcmc[keep,1], R.mcmc[keep,1], Gstar.mcmc[keep,1], alpha.mcmc[keep,1], m.mcmc[keep,1], g0.mcmc[keep,1], tauF.mcmc[keep,1], tauBB.mcmc[keep,1])
jpeg(paste(SaveLeafDir, runName, "_Params_Corr_Plot.jpeg",sep=""))
	pairs(df,c("Vcmax","Jmax","R","Gamma*","alpha","m","g0","F Var","BB Var"))
dev.off() 



## set remaining calculated values to NULL if they weren't tracked
if(!exists('pA.mcmc')) 	{ pA.mcmc=NULL }
if(!exists('pgs.mcmc')) 	{ pgs.mcmc=NULL }

## Save output data
save(Vcmax.mcmc,Jmax.mcmc,R.mcmc,Gstar.mcmc,alpha.mcmc,m.mcmc,g0.mcmc,tauF.mcmc,tauBB.mcmc,An.pred.mcmc,gs.pred.mcmc,dat,niter,DIC.An,DIC.gs, file=saveFileDat)
