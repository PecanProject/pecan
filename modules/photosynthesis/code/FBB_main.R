## Setup and Run FBBmodel
##syntax: nohup env SPECIES=1 LEAF=31 CHAIN=1 R --vanilla < FBB_main.R > log1-31-1 &
startTime = proc.time()[3] # Start timer

#****************** USER PARAMETERS ***********************************
species.id 			<- as.numeric(system("echo $SPECIES",intern=TRUE))
leaf 				<- as.numeric(system("echo $LEAF",intern=TRUE))
chain 				<- as.numeric(system("echo $CHAIN",intern=TRUE))
niter		 		= 100000	# number of MCMC iterations to compute 
nchains				= 3			# number of chains that will be used
progressEvery 		= 10000 	# How frequent to print progress

## File parameters
filePath 			= "/home/wolz1/Biomath/" # "/Users/wolzy4u/Desktop/Biomath/"  
datFile  			= "Kdata_Project.csv" # "Biomath_Processed_Data.csv"
saveDirDat			= "FBB_soyface_output_dat/" # "FBB_output_dat/" 
setupFile			= "FBB_setup.R"
funcFile 			= "FBB_functions.R"
mcmcFile			= "FBB_mcmc.R"

datFile  			= paste(filePath, datFile, sep="")
saveDirDat			= paste(filePath, saveDirDat, sep="")
setupFile			= paste(filePath, setupFile,sep="")
funcFile 			= paste(filePath, funcFile, sep="")
mcmcFile 			= paste(filePath, mcmcFile, sep="")

dir.create(saveDirDat,showWarnings=FALSE,recursive=TRUE)

## Toggles and parameters
compute.pA 			= TRUE	# Whether or not to compute pA
compute.pgs 		= TRUE	# Whether or not to compute pgs
compute.DA			= TRUE	# Whether or not to compute DA
compute.Dgs			= TRUE	# Whether or not to compute Dgs
track.An.pred		= TRUE 	# Whether or not to track An.pred (TRUE = yes)
track.gs.pred		= TRUE 	# Whether or not to track gs.pred (TRUE = yes)

sample.Vcmax		= TRUE 	# Whether or not to sample Vcmax (TRUE = yes)
	prior.mu.Vcmax 	= 4.61
	prior.sd.Vcmax 	= 0.32
sample.Jmax			= TRUE	# Whether or not to sample Jmax (TRUE = yes)
	prior.mu.Jmax 	= 5.16
	prior.sd.Jmax 	= 0.32
sample.R	 		= TRUE 	# Whether or not to sample R (TRUE = yes)
	prior.mu.R 		= -0.69
	prior.sd.R 		= 1
	R.lb = 0 				# lower bound on R
	R.ub = 10				# upper bound on R
sample.Gstar		= TRUE 	# Whether or not to sample Gstar (TRUE = yes)
	prior.mu.Gstar	= 3.75
	prior.sd.Gstar	= 1
sample.alpha		= TRUE 	# Whether or not to sample alpha (TRUE = yes)	
	prior.mu.alpha	= -0.15
	prior.sd.alpha	= 0.1
	alpha.lb 		= 0 	# lower bound on alpha
	alpha.ub		= 1		# upper bound on alpha
sample.m			= TRUE 	# Whether or not to sample alpha (TRUE = yes)	
	prior.mu.m		= 10
	prior.sd.m		= 10
sample.g0			= TRUE 	# Whether or not to sample alpha (TRUE = yes)	
	prior.mu.g0		= 0
	prior.sd.g0		= 0.1
sample.tauF			= TRUE 	# Whether or not to sample tauF (TRUE = yes)	
	prior.s1.tauF	= 0.1
	prior.s2.tauF	= 0.1
	tauF.lb = 0 			# lower bound on tauF
	tauF.ub = 100			# upper bound on tauF
sample.tauBB		= TRUE 	# Whether or not to sample tauBB (TRUE = yes)	
	prior.s1.tauBB	= 0.1
	prior.s2.tauBB	= 0.00001
	tauBB.lb = 0 			# lower bound on tauBB
	tauBB.ub = 100			# upper bound on tauBB

## Initial conditions
Vcmax.ic 	= c(88.7,200,30)	
Jmax.ic		= c(144.8,300,50)	
R.ic		= c(0.6,2,0.01)	
Gstar.ic	= c(29.8,60,5)
alpha.ic	= c(0.85,0.99,0.3)
m.ic       	= c(12.8,20,1)	
g0.ic		= c(0,1,-1)		
tauF.ic		= c(0.4,1,0.01)		
tauBB.ic	= c(0.033,0.05,0.001)

## Jump SD
jumpSD.Vcmax 	= 5
jumpSD.Jmax 	= 10
jumpSD.R 		= 1
jumpSD.Gstar 	= 5
jumpSD.alpha 	= 0.1
jumpSD.m 		= 3
jumpSD.g0 		= 0.05
jumpSD.tauF 	= 0.01
jumpSD.tauBB 	= 0.01

## Constants
O = 210         # [02] in ppt (millimol/mol)
Kc = 275		# Michaelis-Menton constant of RuBisCO for C02
Ko = 400		# Michaelis-Menton constant of RuBisCO for O
phi = 0.85		# maximum dark-adapted quantum yield of PSII
beta = 0.5		# fraction of absorbed quanta that reasches PSII
theta = 0.7		# empirical curvature factor

#**********************************************************************
## FOR EACH LEAF - CHAIN
runName = paste(species.id, leaf, chain, sep="-")

## Run setup file
source(setupFile)

## Load functions (in separate file for convenience)
source(funcFile)

## Initial Conditions
Vcmax 	= Vcmax.ic[chain]	# max velocity of carboxylation (micromol/m^2/s)
Jmax	= Jmax.ic[chain]	# max rate of electron transport (micromol/m^2/s)
R		= R.ic[chain]		# day respiration (micromol/m^2/s)
Gstar	= Gstar.ic[chain]	# CO2 compensation pt in the absense of dark resp
alpha	= alpha.ic[chain]	# absorbance of leaves (3.1)
m       = m.ic[chain]		# slope of Ball-Berry model
g0		= g0.ic[chain]		# y-intercept of Ball-Berry model
tauF	= tauF.ic[chain]	# variance of Farquhar model
tauBB	= tauBB.ic[chain]	# variance of Ball-Berry model

## Run MCMC
source(mcmcFile,print.eval=TRUE)

save.image(paste(saveDirDat, runName, ".Rdata", sep=""))
#**********************************************************************

elapsedTime = proc.time()[3] - startTime 
print(elapsedTime)
efficiency = elapsedTime/niter
print(paste('Seconds/Iteration:', efficiency))
