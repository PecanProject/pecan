##############################################
# Example allometry Workflow Script 
# Christy Rollinson, crollinson@gmail.com	
# 25 March 2015
##############################################

##########################################################################
# Set Directories, load libraries
##########################################################################
#setwd("~/Desktop/pecan/modules/allometry/R")

library(ggplot2)


# Script Querying allometries
setwd("~/Desktop/pecan/modules/allometry/R")

#outdir <- "~/Dropbox/PalEON CR/Tree Rings/Tree-Rings-and-Biomass/Uncertainty_analysis/AllomFiles/Size_tests" # CR Office
outdir <- "~/Desktop/PalEON CR/Tree Rings/Tree-Rings-and-Biomass/Uncertainty_analysis/Pecan_Size_Testing/PecanAllom" # CR Home

source("AllomAve.R")
source("query.allom.data.R")
source("allom.BayesFit.R")
source("read.allom.data.R")


##########################################################################
# Query Allom
#    Balsam Fir (12, ABBA) -- 4 equations, 1 with baby trees
#    Doug-Fir (202, PSME) -- 6 equations, 4 max at 25, 1 maxes at 6
#    Red Maple (316, ACRU),
#    Sugar Maple (318, ACSA3)
#    White Oak (802, QUAL)
#    Northern Red Oak (833, QURU)
##########################################################################
spp.list = list(ABBA = data.frame(spcd=12,acronym="ABBA"),
				PSME = data.frame(spcd=202,acronym="PSME"),
				ACRU = data.frame(spcd=316,acronym="ACRU"),
				ACSA = data.frame(spcd=318,acronym="ACSA"),
				QUAL = data.frame(spcd=802,acronym="QUAL"),
				QURU = data.frame(spcd=833,acronym="QURU"))

# Querying full range
AllomAve(spp.list,2,outdir=file.path(outdir, "0.1-500"),parm="../data/Table3_GTR-NE-319.v2.csv",ngibbs=5000, dmin=0.1, dmax=500)

# Querying double-truncated range
AllomAve(spp.list,2,outdir=file.path(outdir, "10-50"),parm="../data/Table3_GTR-NE-319.v2.csv",ngibbs=5000, dmin=10, dmax=50)

# Querying left-truncated range
AllomAve(spp.list,2,outdir=file.path(outdir, "10-500"),parm="../data/Table3_GTR-NE-319.v2.csv",ngibbs=5000, dmin=10, dmax=500)

# Querying right-truncated range
AllomAve(spp.list,2,outdir=file.path(outdir, "0.1-50"),parm="../data/Table3_GTR-NE-319.v2.csv",ngibbs=5000, dmin=0.1, dmax=50)


##########################################################################
# Plotting & running some diagnostics on the allometry with different size cutoffs
##########################################################################
#setwd("~/Desktop/PalEON CR/Tree Rings/Tree-Rings-and-Biomass/Uncertainty_analysis/Pecan_Size_Testing")
setwd("~/Dropbox/PalEON CR/Tree Rings/Tree-Rings-and-Biomass/Uncertainty_analysis/Pecan_Size_Testing")
#allom.dir <- "~/Desktop/PalEON CR/Tree Rings/Tree-Rings-and-Biomass/Uncertainty_analysis/Pecan_Size_Testing/PecanAllom/"
allom.dir <- "~/Dropbox/PalEON CR/Tree Rings/Tree-Rings-and-Biomass/Uncertainty_analysis/Pecan_Size_Testing/PecanAllom/"

allom.eq <- function(mu0, mu1, DBH) { exp(mu0 + mu1 * log(DBH) )}



n.samp <- 500 # This number of samples will be pulled from each mc chain
# ----------------------------------------------------
# Sampling the different distributions
# ----------------------------------------------------
allom.full <- list()
for(s in names(spp.list)){
	load(paste0(allom.dir, "0.1-500/Allom.", s, ".2.Rdata"))
	samp.temp <- array()
	for(i in 1:length(mc)){
		samp.temp <- rbind(samp.temp, mc[[i]][sample(1:nrow(mc[[i]]), size=n.samp, replace=T),])
	}
	allom.full[[s]] <- samp.temp
}

allom.left <- list()
for(s in names(spp.list)){
	load(paste0(allom.dir, "10-500/Allom.", s, ".2.Rdata"))
	samp.temp <- array()
	for(i in 1:length(mc)){
		samp.temp <- rbind(samp.temp, mc[[i]][sample(1:nrow(mc[[i]]), size=n.samp, replace=T),])
	}
	allom.left[[s]] <- samp.temp
}

allom.right <- list()
for(s in names(spp.list)){
	load(paste0(allom.dir, "0.1-50/Allom.", s, ".2.Rdata"))
	samp.temp <- array()
	for(i in 1:length(mc)){
		samp.temp <- rbind(samp.temp, mc[[i]][sample(1:nrow(mc[[i]]), size=n.samp, replace=T),])
	}
	allom.right[[s]] <- samp.temp
}

allom.both <- list()
for(s in names(spp.list)){
	load(paste0(allom.dir, "10-50/Allom.", s, ".2.Rdata"))
	samp.temp <- array()
	for(i in 1:length(mc)){
		samp.temp <- rbind(samp.temp, mc[[i]][sample(1:nrow(mc[[i]]), size=n.samp, replace=T),])
	}
	allom.both[[s]] <- samp.temp
}
# ----------------------------------------------------

dbh.range <- 1:100

temp.full <- temp.left <- temp.right <- temp.both <- list()

# ------------------------------------------------
# Creating a distribution of Biomass estimations for 1 example tree
# ------------------------------------------------
for(s in names(spp.list)){
	temp.full[[s]] <- array(NA, dim=c(length(1:length(dbh.range)), nrow(allom.full[[1]])))
	temp.left[[s]] <- array(NA, dim=c(length(1:length(dbh.range)), nrow(allom.left[[1]])))
	temp.right[[s]] <- array(NA, dim=c(length(1:length(dbh.range)), nrow(allom.right[[1]])))
	temp.both[[s]] <- array(NA, dim=c(length(1:length(dbh.range)), nrow(allom.both[[1]])))
	
	# ------------------------------------
	# Getting MCMC iteration estimations
	#	Note: the ifelse statement was originally designed to pull the hierarchical 
	#   Coefficients by default.  Because there is no hierarchical model when there's
	#   only 1 equation being used, those scenarios must use the global coeffcients.
	#
	#	As of 25 March 2015, there's some weirdness going on with the hierarchical
	#	coefficients and it's safer to just pull the global coefficients instead
	# ------------------------------------
	for(i in 1:nrow(allom.full[[1]])){
		# mu0 = ifelse(!(allom.full[[s]][i,"mu0"]==0 &  allom.full[[s]][i,"mu1"]==0), allom.full[[s]][i,"mu0"],  allom.full[[s]][i,"Bg0"])
		# mu1 = ifelse(!(allom.full[[s]][i,"mu0"]==0 &  allom.full[[s]][i,"mu1"]==0), allom.full[[s]][i,"mu1"],  allom.full[[s]][i,"Bg1"])
		mu0 = allom.full[[s]][i,"Bg0"]
		mu1 = allom.full[[s]][i,"Bg1"]
		temp.full[[s]][,i] <- allom.eq(mu0=mu0, mu1=mu1, DBH=dbh.range)
	}

	for(i in 1:nrow(allom.left[[1]])){
		# mu0 = ifelse(!(allom.left[[s]][i,"mu0"]==0 &  allom.left[[s]][i,"mu1"]==0), allom.left[[s]][i,"mu0"],  allom.left[[s]][i,"Bg0"])
		# mu1 = ifelse(!(allom.left[[s]][i,"mu0"]==0 &  allom.left[[s]][i,"mu1"]==0), allom.left[[s]][i,"mu1"],  allom.left[[s]][i,"Bg1"])
		mu0 = allom.left[[s]][i,"Bg0"]
		mu1 = allom.left[[s]][i,"Bg1"]
		temp.left[[s]][,i] <- allom.eq(mu0=mu0, mu1=mu1, DBH=dbh.range)
	}
	
	for(i in 1:nrow(allom.right[[1]])){
		# mu0 = ifelse(!(allom.right[[s]][i,"mu0"]==0 &  allom.right[[s]][i,"mu1"]==0), allom.right[[s]][i,"mu0"],  allom.right[[s]][i,"Bg0"])
		# mu1 = ifelse(!(allom.right[[s]][i,"mu0"]==0 &  allom.right[[s]][i,"mu1"]==0), allom.right[[s]][i,"mu1"],  allom.right[[s]][i,"Bg1"])
		mu0 = allom.right[[s]][i,"Bg0"]
		mu1 = allom.right[[s]][i,"Bg1"]
		temp.right[[s]][,i] <- allom.eq(mu0=mu0, mu1=mu1, DBH=dbh.range)
	}

	for(i in 1:nrow(allom.both[[1]])){
		# mu0 = ifelse(!(allom.both[[s]][i,"mu0"]==0 &  allom.both[[s]][i,"mu1"]==0), allom.both[[s]][i,"mu0"],  allom.both[[s]][i,"Bg0"])
		# mu1 = ifelse(!(allom.both[[s]][i,"mu0"]==0 &  allom.both[[s]][i,"mu1"]==0), allom.both[[s]][i,"mu1"],  allom.both[[s]][i,"Bg1"])
		mu0 = allom.both[[s]][i,"Bg0"]
		mu1 = allom.both[[s]][i,"Bg1"]
		temp.both[[s]][,i] <- allom.eq(mu0=mu0, mu1=mu1, DBH=dbh.range)
	}	
	# End iterations loops
	# ------------------------------------
} # end species loop
# ------------------------------------------------
plot(temp.both[[1]][,2])

# ------------------------------------------------
# Condensing the Distributions into ranges & 95% CI
# ------------------------------------------------
full.final <- left.final <- right.final <- both.final <- allom.final.list <- list()
allom.final <- data.frame()


for(s in names(spp.list)){
	full.mean <- apply(temp.full[[s]], 1, mean, na.rm=T)
	full.ci <- apply(temp.full[[s]], 1, quantile, c(0.025, 0.975), na.rm=T) 	
	full.final[[s]] <- data.frame(DBH=dbh.range, Mean=full.mean, LB=full.ci[1,], UB=full.ci[2,], Allom="Full Distribution", Species=s)

	left.mean <- apply(temp.left[[s]], 1, mean, na.rm=T)
	left.ci <- apply(temp.left[[s]], 1, quantile, c(0.025, 0.975), na.rm=T) 	
	left.final[[s]] <- data.frame(DBH=dbh.range, Mean=left.mean, LB=left.ci[1,], UB=left.ci[2,], Allom="Left-Truncated", Species=s)

	right.mean <- apply(temp.right[[s]], 1, mean, na.rm=T)
	right.ci <- apply(temp.right[[s]], 1, quantile, c(0.025, 0.975), na.rm=T) 	
	right.final[[s]] <- data.frame(DBH=dbh.range, Mean=right.mean, LB=right.ci[1,], UB=right.ci[2,], Allom="Right-Truncated", Species=s)

	both.mean <- apply(temp.both[[s]], 1, mean, na.rm=T)
	both.ci <- apply(temp.both[[s]], 1, quantile, c(0.025, 0.975), na.rm=T) 	
	both.final[[s]] <- data.frame(DBH=dbh.range, Mean=both.mean, LB=both.ci[1,], UB=both.ci[2,], Allom="Double-Truncated", Species=s)
	
	allom.final <- rbind(allom.final, full.final[[s]], left.final[[s]], right.final[[s]], both.final[[s]])
}
# ------------------------------------------------

summary(allom.final)

q.blank <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12), axis.title.x=element_text(face="bold", size=14),  axis.title.y=element_text(face="bold", size=14))
	

ggplot(data=allom.final) + facet_wrap(~Species) +
	geom_ribbon(aes(x=DBH, ymin=LB, ymax=UB, fill=Allom), alpha=0.25) +
	geom_line(aes(x=DBH, y=Mean, color=Allom, linetype=Allom), size=2) + 
	scale_y_continuous(name="Biomass (Mg/tree)") +
	scale_x_continuous(name="DBH (cm)") +
	q.blank +
	theme(legend.position=c(0.15, 0.8))
	
