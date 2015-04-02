## SAIL test
library(PEcAnRTM)
corn <- c(1.48, 50, 0.011, 0.005)
full.refl <- prospect(4, corn, FALSE)
full.trans <- prospect(4, corn, TRUE)

data(dataSpec_ps5B)
soil.dry <- dataSpec_ps5B$dry_soil
soil.wet <- dataSpec_ps5B$wet_soil

## SAIL parameters
LAI <- 1.5
LIDFa <- -0.65
LIDFb <- -0.15
hot <- 0.05
f_brown <- 0
diss <- 0.8
soil.moisture <- 0
crown_cover <- 0.7
zeta <- 3.5
rb <- 0
tb <- 0
sol_zenith <- 28
view_zenith <- 0
azimuth <- 0

soil <- soil.moisture * soil.wet + (1 - soil.moisture) * soil.dry

sail.reflectance <- function(rg, tg, soil){
	sail.params <- c(LAI, LIDFa, LIDFb, hot, f_brown, diss,
					 soil, soil, soil, soil,
					 crown_cover, zeta, rg, rb, tg, tb, 
					 sol_zenith, view_zenith, azimuth)
	out.full <- fs(sail.params)
	return(out.full)
}

rng <- 1:2101

full.run <- function(rng){
	print(system.time(corn.sail <- mapply(sail.reflectance,
										  full.refl[rng],
										  full.trans[rng],
										  soil[rng])))
	par(mfrow=c(2,2))
	for(i in 1:4) plot(corn.sail[i,], type='l', ylim=c(0,0.5))
	return(corn.sail)
}

cs <- full.run(rng)
