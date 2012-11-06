## Retreive & Analyze FBBmodel Results
##syntax: nohup env SPECIES=2 LEAF=4 R --vanilla < FBB_cluster_processing.R > log2-4 &

#****************** USER PARAMETERS ***********************************
species.id 			<- as.numeric(system("echo $SPECIES",intern=TRUE))
leaf 				<- as.numeric(system("echo $LEAF",intern=TRUE))
nchains				<- 3 # as.numeric(system("echo $NCHAINS",intern=TRUE))

niter		 		= 100000	# number of MCMC iterations to compute 

start				= 10000		# number of burn-in iterations
end					= niter		# last iteration
thin				= 50		# number of iterations to thin

## File parameters
filePath 			= "/home/wolz1/Biomath/" # "/Users/wolzy4u/Desktop/Biomath/"
datFile  			= "Kdata_Project.csv" # "Biomath_Processed_Data.csv"
saveDirec			= "FBB_soyface_output/" # "FBB_output/"
saveDirDat			= "FBB_soyface_output_dat/" # "FBB_output_dat/" 

setupFile			= "FBB_setup.R"
funcFile 			= "FBB_functions.R"
multianalysisFile 	= "FBB_multi_chain_analysis.R"
comparisonFile 		= "FBB_compare.R"
paramcorrFile 		= "FBB_param_corr.R"
summaryFile 		= "FBB_summary.R"

datFile  			= paste(filePath, datFile, sep="")
saveDirec			= paste(filePath, saveDirec, sep="")
saveDirDat			= paste(filePath, saveDirDat, sep="")
setupFile			= paste(filePath, setupFile,sep="")
funcFile 			= paste(filePath, funcFile, sep="")
multianalysisFile 	= paste(filePath, multianalysisFile, sep="")
comparisonFile 		= paste(filePath, comparisonFile, sep="")
paramcorrFile 		= paste(filePath, paramcorrFile, sep="")
summaryFile 		= paste(filePath, summaryFile, sep="")

## Toggles and parameters
plot.corr			= TRUE  # Whether or not to plot parameter correlations

## Constants
O = 210         # [02] in ppt (millimol/mol)
Kc = 275		# Michaelis-Menton constant of RuBisCO for C02
Ko = 400		# Michaelis-Menton constant of RuBisCO for O
phi = 0.85		# maximum dark-adapted quantum yield of PSII
beta = 0.5		# fraction of absorbed quanta that reasches PSII
theta = 0.7		# empirical curvature factor

## Run setup file
source(setupFile)

## Load functions (in separate file for convenience)
source(funcFile)

Vcmax.allchains  	= matrix(NA, nrow=niter, ncol=nchains) 
Jmax.allchains 		= matrix(NA, nrow=niter, ncol=nchains) 
R.allchains  		= matrix(NA, nrow=niter, ncol=nchains) 
Gstar.allchains  	= matrix(NA, nrow=niter, ncol=nchains) 
alpha.allchains  	= matrix(NA, nrow=niter, ncol=nchains) 
m.allchains   		= matrix(NA, nrow=niter, ncol=nchains) 
g0.allchains   		= matrix(NA, nrow=niter, ncol=nchains) 
tauBB.allchains  	= matrix(NA, nrow=niter, ncol=nchains) 
tauF.allchains  	= matrix(NA, nrow=niter, ncol=nchains) 
An.pred.allchains 	= matrix(NA, nrow=niter*nchains, ncol=npoints) 
gs.pred.allchains 	= matrix(NA, nrow=niter*nchains, ncol=npoints) 
DA.allchains 		= matrix(NA, nrow=niter, ncol=nchains) 
Dgs.allchains 		= matrix(NA, nrow=niter, ncol=nchains) 
pA.allchains  		= matrix(NA, nrow=niter*nchains, ncol=npoints) 
pgs.allchains  		= matrix(NA, nrow=niter*nchains, ncol=npoints)

#**********************************************************************
## EXTRACT DATA FOR EACH CHAIN AND COMPILE INTO ONE MATRIX PER VARIABLE
for(chain in 1:nchains){
oldrunName = paste(species.id, leaf, chain, sep="-")
load(paste(saveDirDat,oldrunName,".Rdata",sep=""))

Vcmax.allchains[,chain] = Vcmax.mcmc[,chain]	
Jmax.allchains[,chain] 	= Jmax.mcmc[,chain]	
R.allchains[,chain] 	= R.mcmc[,chain]
	
if(exists('Gstar.mcmc') && !is.null(Gstar.mcmc)) { Gstar.allchains[,chain] = Gstar.mcmc[,chain]	} else {
	Gstar.allchains = NULL
}

alpha.allchains[,chain] = alpha.mcmc[,chain]
m.allchains[,chain] 	= m.mcmc[,chain]
g0.allchains[,chain] 	= g0.mcmc[,chain]
tauBB.allchains[,chain] = tauBB.mcmc[,chain]
tauF.allchains[,chain] 	= tauF.mcmc[,chain]
DA.allchains[,chain] 	= DA.mcmc[,chain]
Dgs.allchains[,chain] 	= Dgs.mcmc[,chain]

An.pred.allchains[((1+niter*(chain-1)):(niter+niter*(chain-1))),] = An.pred.mcmc[((1+niter*(chain-1)):(niter+niter*(chain-1))),]
gs.pred.allchains[((1+niter*(chain-1)):(niter+niter*(chain-1))),] = gs.pred.mcmc[((1+niter*(chain-1)):(niter+niter*(chain-1))),]

pA.allchains[((1+niter*(chain-1)):(niter+niter*(chain-1))),] = pA.mcmc[((1+niter*(chain-1)):(niter+niter*(chain-1))),]
pgs.allchains[((1+niter*(chain-1)):(niter+niter*(chain-1))),] = pgs.mcmc[((1+niter*(chain-1)):(niter+niter*(chain-1))),]
} ## end of chain

#**********************************************************************
runName = paste(species.id, leaf, sep="-")
SaveLeafDir		= paste(saveDirec, runName, "/", sep="")
saveFileDat		= paste(SaveLeafDir, runName, ".Rdata", sep="")
saveFileSum		= paste(SaveLeafDir, runName, "_summary.txt", sep="")
dir.create(SaveLeafDir,showWarnings=FALSE,recursive=TRUE)

Vcmax.mcmc 	= Vcmax.allchains
Jmax.mcmc 	= Jmax.allchains
R.mcmc 		= R.allchains
Gstar.mcmc 	= Gstar.allchains
alpha.mcmc 	= alpha.allchains
m.mcmc 		= m.allchains
g0.mcmc 	= g0.allchains
tauBB.mcmc 	= tauBB.allchains
tauF.mcmc 	= tauF.allchains
DA.mcmc 	= DA.allchains
Dgs.mcmc 	= Dgs.allchains
An.pred.mcmc = An.pred.allchains
gs.pred.mcmc = gs.pred.allchains
pA.mcmc 	= pA.allchains
pgs.mcmc 	= pgs.allchains

## Run analysis
source(multianalysisFile,print.eval=TRUE)

## Run parameter correlation
source(paramcorrFile,print.eval=TRUE)

## Run summary creation
source(summaryFile,print.eval=TRUE)
