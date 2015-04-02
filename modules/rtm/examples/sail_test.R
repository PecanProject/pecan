## SAIL test
library(PEcAnRTM)
corn <- c(1.48, 50, 0.011, 0.005)
full.refl <- prospect(4, corn, FALSE)
full.trans <- prospect(4, corn, TRUE)

## SAIL parameters
LAI <- 0.5
LIDFa <- -0.65
LIDFb <- -0.15
hot <- 0.05
f_brown <- 0
diss <- 0.8
soil_rso <- 0.6
soil_rdo <- 0.6
soil_rsd <- 0.6
soil_rdd <- 0.6
crown_cover <- 0.7
zeta <- 3.5
rb <- 0
tb <- 0
sol_zenith <- 0
view_zenith <- 0
azimuth <- 0

sail.reflectance <- function(rg, tg){
	sail.params <- c(LAI, LIDFa, LIDFb, hot, f_brown, diss,
					 soil_rsd, soil_rdo, soil_rsd, soil_rdd,
					 crown_cover, zeta, rg, rb, tg, tb, 
					 sol_zenith, view_zenith, azimuth)
	out.full <- fs(sail.params)
	return(out.full)
}

rng <- 1:2101

full.run <- function(rng){
	print(system.time(corn.sail <- mapply(sail.reflectance, full.refl[rng], full.trans[rng])))
	par(mfrow=c(2,2))
	for(i in 1:4) plot(corn.sail[i,], type='l', ylim=c(0,1))
	return(corn.sail)
}

cs <- full.run(rng)
