## PEcAN RTM Package Vignette
## Author: Alexey Shiklomanov

# Load package
library(PEcAnRTM)

# Run RTMs in forward mode
wl <- 400:2500
p1 <- prospect(4, c(1.4, 30, 0.01, 0.001))
plot(wl, p1, type='l') 

p2.params <- c(1.4, 30, 10, 0.01, 0.001)
p2 <- prospect(5, p2.params)   			             ## Reflectance
p2t <- prospect(5, p2.params, reflectance = FALSE)   ## Transmittance
plot(wl, p2, type='l', ylim=c(0,1))
lines(wl, 1-p2t, col=2)

# Load test spectra (in package)
data(testspec)
testspec_ACRU[1:5,1:5]
plot(400:2500, testspec_ACRU[,1], type='l')
lines(400:2500, testspec_ACRU[,2], col=2)
lines(400:2500, testspec_ACRU[,3], col=3)

# Run inversion on single spectrum
#		'ngibbs' controls the number of iterations. Default is 15000, but you 
#		can usually get decent values with as few as 1500.
system.time(inv.p <- invert_prospect(testspec_ACRU[,1], ngibbs=3000))

# Trace plots
par(mfrow=c(2,2))
plot(inv.p[,1], type='l', main="N")
plot(inv.p[,2], type='l', main="Cab")
plot(inv.p[,3], type='l', main="Cw")
plot(inv.p[,4], type='l', main="Cm")

# Burn-in : Remove first 500 values during which model is trying to converge
inv.pb <- inv.p[-500:0,]

# Trace plots after burn-in
plot(inv.pb[,1], type='l', main="N")
plot(inv.pb[,2], type='l', main="Cab")
plot(inv.pb[,3], type='l', main="Cw")
plot(inv.pb[,4], type='l', main="Cm")

# Probability density plots of parameters (after burn-in)
plot(density(inv.pb[,1]), main="N")
plot(density(inv.pb[,2]), main="Cab")
plot(density(inv.pb[,3]), main="Cw")
plot(density(inv.pb[,4]), main="Cm")

# Mean parameter values
colMeans(inv.pb)

# Standard deviation
colMeans(inv.pb)


## Inversion for multiple spectra -- returns single set of paramters that fits
##		the full set of spectra. These take longer because the matrix operation
##		is bigger, so for demonstrative purposes, I've reduced ngibbs.
system.time(inv.2 <- invert_prospect(testspec_ACRU[,1:5], ngibbs = 3000))
plot(inv.2[,1], type='l', main="N")
plot(inv.2[,2], type='l', main="Cab")
plot(inv.2[,3], type='l', main="Cw")
plot(inv.2[,4], type='l', main="Cm")

inv.2b <- inv.2[-500:0,]
plot(density(inv.2b[,1]), main="N")
plot(density(inv.2b[,2]), main="Cab")
plot(density(inv.2b[,3]), main="Cw")
plot(density(inv.2b[,4]), main="Cm")