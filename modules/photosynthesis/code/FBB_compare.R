## FBB_compare
library(plotrix)
filePath 	= "/Users/wolzy4u/Desktop/Biomath/" #"/home/wolz1/Biomath/"
saveDirec	= "FBB_soyface_output/" # "FBB_output/"
compareFile	= "param_compare.csv"
saveDirec	= paste(filePath, saveDirec, sep="")
compare 	= read.csv(paste(filePath,compareFile,sep=""), header=T) 

compare.coupled.alone 	= TRUE
compare.coupled.manual 	= TRUE
compare.alone.manual 	= TRUE


## Coupled FBB Model Mean & SD
Vcmax.leaf.mean = compare$coupled.Vcmax
Jmax.leaf.mean = compare$coupled.Jmax
alpha.leaf.mean = compare$coupled.alpha
R.leaf.mean = compare$coupled.R
Gstar.leaf.mean = compare$coupled.Gstar
tauF.leaf.mean = compare$coupled.tauF
m.leaf.mean = compare$coupled.m
g0.leaf.mean = compare$coupled.g0
tauBB.leaf.mean = compare$coupled.tauBB
DIC.F.leaf.mean = compare$coupled.DIC.F
DIC.BB.leaf.mean = compare$coupled.DIC.BB 

Vcmax.leaf.sd = compare$coupled.Vcmax.sd
Jmax.leaf.sd = compare$coupled.Jmax.sd
alpha.leaf.sd = compare$coupled.alpha.sd
R.leaf.sd = compare$coupled.R.sd
Gstar.leaf.sd = compare$coupled.Gstar.sd
tauF.leaf.sd = compare$coupled.tauF.sd
m.leaf.sd = compare$coupled.m.sd
g0.leaf.sd = compare$coupled.g0.sd
tauBB.leaf.sd = compare$coupled.tauBB.sd

		
## Uncoupled FBB Model Mean & SD
compare.V.alone = compare$alone.Vcmax 
compare.J.alone = compare$alone.Jmax
compare.alpha.alone = compare$alone.alpha 
compare.R.alone = compare$alone.R
compare.Gstar.alone = compare$alone.Gstar
compare.tauF.alone = compare$alone.tauF
compare.DIC.F.alone = compare$alone.DIC.F
compare.g0.alone = compare$alone.g0
compare.m.alone = compare$alone.m
compare.tauBB.alone = compare$alone.tauBB
compare.DIC.BB.alone = compare$alone.DIC.BB
		
sd.V.alone = compare$alone.Vcmax.sd 
sd.J.alone = compare$alone.Jmax.sd
sd.alpha.alone = compare$alone.alpha.sd 
sd.R.alone = compare$alone.R.sd
sd.Gstar.alone = compare$alone.Gstar.sd 
sd.tauF.alone = compare$alone.tauF.sd	
sd.g0.alone = compare$alone.g0.sd
sd.m.alone = compare$alone.m.sd
sd.tauBB.alone = compare$alone.tauBB.sd


## Manual Fit Mean
compare.V.man = compare$Katie.Vcmax 
compare.J.man = compare$Katie.Jmax
compare.g0.MLE = compare$g0.nobound 
compare.m.MLE = compare$m.nobound


if(compare.coupled.alone) { 
	## compare coupled mcmc means to uncoupled means for Vcmax
	lim = min(compare.V.alone-sd.V.alone,Vcmax.leaf.mean-Vcmax.leaf.sd)-10
	lim[2] = max(compare.V.alone+sd.V.alone,Vcmax.leaf.mean+Vcmax.leaf.sd)+10
	pdf(paste(saveDirec, "Coupled_vs_Alone_Comparison-Vcmax.pdf",sep=""))
	plot(compare.V.alone, 
		Vcmax.leaf.mean,
		ylim = lim,
		xlim = lim, 
		pch = 19,
		main = "Coupled MCMC vs Non-Coupled MCMC Vcmax Means", 
		xlab = "Non-Coupled MCMC Vcmax (umol m-2 s-1)", 
		ylab = "Coupled MCMC Vcmax (umol m-2 s-1)",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(compare.V.alone,Vcmax.leaf.mean,Vcmax.leaf.sd,add=TRUE,lwd=1)
	plotCI(compare.V.alone,Vcmax.leaf.mean,sd.V.alone,err="x",add=TRUE,lwd=1)
	dev.off()

	## compare coupled mcmc means to manual means for Jmax
	lim = min(compare.J.alone-sd.J.alone,Jmax.leaf.mean-Jmax.leaf.sd)-10
	lim[2] = max(compare.J.alone+sd.J.alone,Jmax.leaf.mean+Jmax.leaf.sd)+10
	pdf(paste(saveDirec, "Coupled_vs_Alone_Comparison-Jmax.pdf",sep=""))
	plot(compare.J.alone, 
		Jmax.leaf.mean,
		ylim = lim,
		xlim = lim, 
		pch = 19,
		main = "Coupled MCMC vs Non-Coupled MCMC Jmax Means", 
		xlab = "Non-Coupled MCMC Jmax (umol m-2 s-1)", 
		ylab = "Coupled MCMC Jmax (umol m-2 s-1)",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(compare.J.alone,Jmax.leaf.mean,Jmax.leaf.sd,add=TRUE,lwd=1)
	plotCI(compare.J.alone,Jmax.leaf.mean,sd.J.alone,err="x",add=TRUE,lwd=1)
	dev.off()
	
	## compare coupled mcmc means to manual means for alpha
	lim = min(compare.alpha.alone-sd.alpha.alone,alpha.leaf.mean-alpha.leaf.sd)-0.1
	lim[2] = max(compare.alpha.alone+sd.alpha.alone,alpha.leaf.mean+alpha.leaf.sd)+0.1
	pdf(paste(saveDirec, "Coupled_vs_Alone_Comparison-alpha.pdf",sep=""))
	plot(compare.alpha.alone, 
		alpha.leaf.mean,
		ylim = lim,
		xlim = lim, 
		pch = 19,
		main = "Coupled MCMC vs Non-Coupled MCMC alpha Means", 
		xlab = "Non-Coupled MCMC alpha", 
		ylab = "Coupled MCMC alpha",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(compare.alpha.alone,alpha.leaf.mean,alpha.leaf.sd,add=TRUE,lwd=1)
	plotCI(compare.alpha.alone,alpha.leaf.mean,
	sd.alpha.alone,err="x",add=TRUE,lwd=1)
	dev.off()

	## compare coupled mcmc means to manual means for R
	lim = min(compare.R.alone-sd.R.alone,R.leaf.mean-R.leaf.sd)-0.1
	lim[2] = max(compare.R.alone+sd.R.alone,R.leaf.mean+R.leaf.sd)+0.1
	pdf(paste(saveDirec, "Coupled_vs_Alone_Comparison-R.pdf",sep=""))
	plot(compare.R.alone, 
		R.leaf.mean,
		ylim = lim,
		xlim = lim, 
		pch = 19,
		main = "Coupled MCMC vs Non-Coupled MCMC R Means", 
		xlab = "Non-Coupled MCMC R (umol m-2 s-1)", 
		ylab = "Coupled MCMC R (umol m-2 s-1)",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(compare.R.alone,R.leaf.mean,R.leaf.sd,add=TRUE,lwd=1)
	plotCI(compare.R.alone,R.leaf.mean,sd.R.alone,err="x",add=TRUE,lwd=1)
	dev.off()
	
	## compare coupled mcmc means to manual means for Gstar
	lim = min(compare.Gstar.alone-sd.Gstar.alone,Gstar.leaf.mean-Gstar.leaf.sd)-5
	lim[2] = max(compare.Gstar.alone+ sd.Gstar.alone,Gstar.leaf.mean+ Gstar.leaf.sd)+5
	pdf(paste(saveDirec, "Coupled_vs_Alone_Comparison-Gstar.pdf",sep=""))
	plot(compare.Gstar.alone, 
		Gstar.leaf.mean,
		ylim = lim,
		xlim = lim, 
		pch = 19,
		main = "Coupled MCMC vs Non-Coupled MCMC Gstar Means", 
		xlab = "Non-Coupled MCMC Gstar (umol mol-1)", 
		ylab = "Coupled MCMC Gstar (umol mol-1)",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(compare.Gstar.alone,Gstar.leaf.mean,Gstar.leaf.sd,add=TRUE,lwd=1)	
	plotCI(compare.Gstar.alone,Gstar.leaf.mean,
	sd.Gstar.alone,err="x",add=TRUE,lwd=1)
	dev.off()
	
	## compare coupled mcmc means to manual means for tauF
	lim = min(compare.tauF.alone-sd.tauF.alone,tauF.leaf.mean-tauF.leaf.sd)-5
	lim[2] = max(compare.tauF.alone+sd.tauF.alone,tauF.leaf.mean+tauF.leaf.sd)+5
	pdf(paste(saveDirec, "Coupled_vs_Alone_Comparison-tauF.pdf",sep=""))
	plot(compare.tauF.alone, 
		tauF.leaf.mean,
		ylim = lim,
		xlim = lim, 
		pch = 19,
		main = "Coupled MCMC vs Non-Coupled MCMC tauF Means", 
		xlab = "Non-Coupled MCMC tauF", 
		ylab = "Coupled MCMC tauF",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(compare.tauF.alone,tauF.leaf.mean,tauF.leaf.sd,add=TRUE,lwd=1)	
	plotCI(compare.tauF.alone,tauF.leaf.mean,
	sd.tauF.alone,err="x",add=TRUE,lwd=1)
	dev.off()
	
	## compare coupled mcmc means to manual means for DIC.F
	lim = min(compare.DIC.F.alone,DIC.F.leaf.mean)-5
	lim[2] = max(compare.DIC.F.alone,DIC.F.leaf.mean)+5
	pdf(paste(saveDirec, "Coupled_vs_Alone_Comparison-DIC.F.pdf",sep=""))
	plot(compare.DIC.F.alone, 
		DIC.F.leaf.mean,
		ylim = lim,
		xlim = lim, 
		pch = 19,
		main = "Coupled MCMC vs Non-Coupled MCMC Farquhar DIC", 
		xlab = "Non-Coupled MCMC DIC", 
		ylab = "Coupled MCMC DIC",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	dev.off()
	
	## compare coupled mcmc means to MLE means for g0
	lim = min(compare.g0.alone-sd.g0.alone,g0.leaf.mean-g0.leaf.sd)-0.1
	lim[2] = max(compare.g0.alone+sd.g0.alone,g0.leaf.mean+g0.leaf.sd)+0.1
	pdf(paste(saveDirec, "Coupled_vs_Alone_Comparison-g0.pdf",sep=""))
	plot(compare.g0.alone, 
		g0.leaf.mean,
		ylim = lim,
		xlim = lim, 
		pch = 19,
		main = "Coupled MCMC vs Non-Coupled MCMC g0 Means", 
		xlab = "Non-Coupled MCMC g0 (mol m-2 s-1)", 
		ylab = "Coupled MCMC g0 (mol m-2 s-1)",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(compare.g0.alone,g0.leaf.mean,g0.leaf.sd,add=TRUE,lwd=1)
	plotCI(compare.g0.alone,g0.leaf.mean,sd.g0.alone,err="x",add=TRUE,lwd=1)
	dev.off()

	## compare coupled mcmc means to MLE means for m
	lim = min(compare.m.alone-sd.m.alone,m.leaf.mean-m.leaf.sd)-1
	lim[2] = max(compare.m.alone+sd.m.alone,m.leaf.mean+m.leaf.sd)+1
	pdf(paste(saveDirec, "Coupled_vs_Alone_Comparison-m.pdf",sep=""))
	plot(compare.m.alone, 
		m.leaf.mean,
		ylim = lim,
		xlim = lim, 
		pch = 19,
		main = "Coupled MCMC vs Non-Coupled MCMC m Means", 
		xlab = "Non-Coupled MCMC m", 
		ylab = "Coupled MCMC m",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(compare.m.alone,m.leaf.mean,m.leaf.sd,add=TRUE,lwd=1)
	plotCI(compare.m.alone,m.leaf.mean,sd.m.alone,err="x",add=TRUE,lwd=1)
	dev.off()
	
	## compare coupled mcmc means to manual means for tauF
	lim = min(compare.tauBB.alone-sd.tauBB.alone,tauBB.leaf.mean-tauBB.leaf.sd)-0.0001
	lim[2] = max(compare.tauBB.alone+sd.tauBB.alone,tauBB.leaf.mean+tauBB.leaf.sd)+0.0001
	pdf(paste(saveDirec, "Coupled_vs_Alone_Comparison-tauBB.pdf",sep=""))
	plot(compare.tauBB.alone, 
		tauBB.leaf.mean,
		ylim = lim,
		xlim = lim, 
		pch = 19,
		main = "Coupled MCMC vs Non-Coupled MCMC tauBB Means", 
		xlab = "Non-Coupled MCMC tauBB", 
		ylab = "Coupled MCMC tauBB",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(compare.tauBB.alone,tauBB.leaf.mean,tauBB.leaf.sd,add=TRUE,lwd=1)	
	plotCI(compare.tauBB.alone,tauBB.leaf.mean,
	sd.tauBB.alone,err="x",add=TRUE,lwd=1)
	dev.off()
	
	## compare coupled mcmc means to manual means for DIC.BB
	lim = min(compare.DIC.BB.alone,DIC.BB.leaf.mean)-5
	lim[2] = max(compare.DIC.BB.alone,DIC.BB.leaf.mean)+5
	pdf(paste(saveDirec, "Coupled_vs_Alone_Comparison-DIC.BB.pdf",sep=""))
	plot(compare.DIC.BB.alone, 
		DIC.BB.leaf.mean,
		ylim = lim,
		xlim = lim, 
		pch = 19,
		main = "Coupled MCMC vs Non-Coupled MCMC Ball-Berry DIC", 
		xlab = "Non-Coupled MCMC DIC", 
		ylab = "Coupled MCMC DIC",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	dev.off()
}


if(compare.coupled.manual) { 
	## compare coupled mcmc to manual fit for Vcmax
	lim = min(compare.V.man,Vcmax.leaf.mean-Vcmax.leaf.sd)-10
	lim[2] = max(compare.V.man,Vcmax.leaf.mean+Vcmax.leaf.sd)+10
	pdf(paste(saveDirec, "Coupled_vs_Manual_Comparison-Vcmax.pdf",sep=""))	
	plot(compare.V.man, 
		Vcmax.leaf.mean,
		ylim = lim,
		xlim = lim, 
		pch = 19,
		main = "Coupled MCMC vs Manually Fit Vcmax", 
		xlab = "Manual Vcmax Fit (umol m-2 s-1)", 
		ylab = "Coupled MCMC Vcmax (umol m-2 s-1)",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(compare.V.man,Vcmax.leaf.mean,Vcmax.leaf.sd,add=TRUE,lwd=1)
	dev.off()

	## compare coupled mcmc to manual fit for Jmax
	lim = min(compare.J.man,Jmax.leaf.mean-Jmax.leaf.sd)-10
	lim[2] = max(compare.J.man,Jmax.leaf.mean+Jmax.leaf.sd)+10
	pdf(paste(saveDirec, "Coupled_vs_Manual_Comparison-Jmax.pdf",sep=""))
	plot(compare.J.man, 
		Jmax.leaf.mean,
		ylim = lim,
		xlim = lim, 
		pch = 19,
		main = "Coupled MCMC vs Manually Fit Jmax", 
		xlab = "Manual Jmax Fit (umol m-2 s-1)", 
		ylab = "Coupled MCMC Jmax (umol m-2 s-1)",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(compare.J.man,Jmax.leaf.mean,Jmax.leaf.sd,add=TRUE,lwd=1)
	dev.off()

	## compare coupled mcmc to MLE fit for g0
	lim = min(compare.g0.MLE,g0.leaf.mean-g0.leaf.sd)-0.1
	lim[2] = max(compare.g0.MLE,g0.leaf.mean+g0.leaf.sd)+0.1
	pdf(paste(saveDirec, "Coupled_vs_MLE_Comparison-g0.pdf",sep=""))
	plot(compare.g0.MLE, 
		g0.leaf.mean,
		ylim = lim,
		xlim = lim, 
		pch = 19,
		main = "Coupled MCMC vs MLE g0 Means", 
		xlab = "MLE g0 (mol m-2 s-1)", 
		ylab = "Coupled MCMC g0 (mol m-2 s-1)",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(compare.g0.MLE,g0.leaf.mean,g0.leaf.sd,add=TRUE,lwd=1)
	dev.off()

	## compare coupled mcmc to MLE fit for m
	lim = min(compare.m.MLE,m.leaf.mean-m.leaf.sd)-1
	lim[2] = max(compare.m.MLE,m.leaf.mean+m.leaf.sd)+1
	pdf(paste(saveDirec, "Coupled_vs_MLE_Comparison-m.pdf",sep=""))
	plot(compare.m.MLE, 
		m.leaf.mean,
		ylim = lim,
		xlim = lim, 
		pch = 19,
		main = "Coupled MCMC vs MLE m Means", 
		xlab = "MLE m", 
		ylab = "Coupled MCMC m",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(compare.m.MLE,m.leaf.mean,m.leaf.sd,add=TRUE,lwd=1)
	dev.off()
}


if(compare.alone.manual) {
		## compare decoupled mcmc to manual fit for Vcmax
	lim = min(compare.V.man,compare.V.alone-sd.V.alone)-10
	lim[2] = max(compare.V.man,compare.V.alone+sd.V.alone)+10
	pdf(paste(saveDirec, "DEcoupled_vs_Manual_Comparison-Vcmax.pdf",sep=""))	
	plot(compare.V.man, 
		compare.V.alone,
		ylim = lim,
		xlim = lim, 
		pch = 19,
		main = "DEcoupled MCMC vs Manually Fit Vcmax", 
		xlab = "Manual Vcmax Fit (umol m-2 s-1)", 
		ylab = "DEcoupled MCMC Vcmax (umol m-2 s-1)",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(compare.V.man,compare.V.alone,sd.V.alone,add=TRUE,lwd=1)
	dev.off()

	## compare decoupled mcmc to manual fit for Jmax
	lim = min(compare.J.man,compare.J.alone-sd.J.alone)-10
	lim[2] = max(compare.J.man,compare.J.alone+sd.J.alone)+10
	pdf(paste(saveDirec, "DEcoupled_vs_Manual_Comparison-Jmax.pdf",sep=""))
	plot(compare.J.man, 
		compare.J.alone,
		ylim = lim,
		xlim = lim, 
		pch = 19,
		main = "DEcoupled MCMC vs Manually Fit Jmax", 
		xlab = "Manual Jmax Fit (umol m-2 s-1)", 
		ylab = "DEcoupled MCMC Jmax (umol m-2 s-1)",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(compare.J.man,compare.J.alone,sd.J.alone,add=TRUE,lwd=1)
	dev.off()

	## compare decoupled mcmc to MLE fit for g0
	lim = min(compare.g0.MLE,compare.g0.alone-sd.g0.alone)-0.1
	lim[2] = max(compare.g0.MLE,compare.g0.alone+sd.g0.alone)+0.1
	pdf(paste(saveDirec, "DEcoupled_vs_MLE_Comparison-g0.pdf",sep=""))
	plot(compare.g0.MLE, 
		compare.g0.alone,
		ylim = lim,
		xlim = lim, 
		pch = 19,
		main = "DEcoupled MCMC vs MLE g0 Means", 
		xlab = "MLE g0 (mol m-2 s-1)", 
		ylab = "DEcoupled MCMC g0 (mol m-2 s-1)",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(compare.g0.MLE,compare.g0.alone,sd.g0.alone,add=TRUE,lwd=1)
	dev.off()

	## compare decoupled mcmc to MLE fit for m
	lim = min(compare.m.MLE,compare.m.alone-sd.m.alone)-1
	lim[2] = max(compare.m.MLE,compare.m.alone+sd.m.alone)+1
	pdf(paste(saveDirec, "DEcoupled_vs_MLE_Comparison-m.pdf",sep=""))
	plot(compare.m.MLE, 
		compare.m.alone,
		ylim = lim,
		xlim = lim, 
		pch = 19,
		main = "DEcoupled MCMC vs MLE m Means", 
		xlab = "MLE m", 
		ylab = "DEcoupled MCMC m",
		cex.main = 1.6,
		cex.lab = 1.4)
	abline(0, 1, col = "dark green", lwd = 3)
	plotCI(compare.m.MLE,compare.m.alone,sd.m.alone,add=TRUE,lwd=1)
	dev.off()
} 
