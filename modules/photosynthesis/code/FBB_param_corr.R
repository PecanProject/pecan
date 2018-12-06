## FBB_param_corr

## parameter correlation plots of each leaf
if(plot.corr){
pdf(paste(SaveLeafDir, runName, "_param_corr_Vcmax_Jmax.pdf",sep=""))
	plot(Vcmax.mcmc[keep,1],
		Jmax.mcmc[keep,1],
		pch = 19,
		main = "Vcmax & Jmax Correlation", 
		xlab = "Vcmax", 
		ylab = "Jmax",
             xlim = c(50,100),
		cex.main = 1.6,
		cex.lab = 1.4)
	dev.off()
	
pdf(paste(SaveLeafDir, runName, "_param_corr_m_Vcmax.pdf",sep=""))
	plot(Vcmax.mcmc[keep,1],
		m.mcmc[keep,1],
		pch = 19,
		main = "Vcmax & m Correlation", 
		xlab = "Vcmax", 
		ylab = "m",
             xlim = c(50,100),
		cex.main = 1.6,
		cex.lab = 1.4)
	dev.off()	

pdf(paste(SaveLeafDir, runName, "_param_corr_m_Jmax.pdf",sep=""))
	plot(Jmax.mcmc[keep,1],
		m.mcmc[keep,1],
		pch = 19,
		main = "Jmax & m Correlation", 
		xlab = "Jmax", 
		ylab = "m",
		cex.main = 1.6,
		cex.lab = 1.4)
	dev.off()
		
pdf(paste(SaveLeafDir, runName, "_param_corr_m_g0.pdf",sep=""))
	plot(g0.mcmc[keep,1],
		m.mcmc[keep,1],
		pch = 19,
		main = "g0 & m Correlation", 
		xlab = "g0", 
		ylab = "m",
		cex.main = 1.6,
		cex.lab = 1.4)
	dev.off()		
}
