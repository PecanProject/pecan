###########################################################
## Demo: Convolution of normal distributions   
###########################################################
require(distr)
options("newDevice"=TRUE)

## initialize two normal distributions
A <- Norm(mean=1, sd=2)
B <- Norm(mean=4, sd=3) 

## convolution via addition of moments
AB <- A+B

## casting of A,B as absolutely continuous distributions
## that is, ``forget'' that A,B are normal distributions
A1 <- as(A, "AbscontDistribution")
B1 <- as(B, "AbscontDistribution")

## for higher precision we change the global variable
## "TruncQuantile" from 1e-5 to 1e-8
oldeps <- getdistrOption("TruncQuantile")
eps <- 1e-8
distroptions("TruncQuantile" = eps)
## support of A1+B1 for FFT convolution is
## [q(A1)(TruncQuantile), 
##  q(B1)(TruncQuantile, lower.tail = FALSE)]

## convolution via FFT
AB1 <- A1+B1

#############################
## plots of the results
#############################
par(mfrow=c(1,3))
low <- q(AB)(1e-15)
upp <- q(AB)(1e-15, lower.tail = FALSE)
x <- seq(from = low, to = upp, length = 10000)

## densities
plot(x, d(AB)(x), type = "l", lwd = 5)
lines(x , d(AB1)(x), col = "orange", lwd = 1)
title("Densities")
legend("topleft", legend=c("exact", "FFT"), 
        fill=c("black", "orange"))

## cdfs
plot(x, p(AB)(x), type = "l", lwd = 5)
lines(x , p(AB1)(x), col = "orange", lwd = 1)
title("Cumulative distribution functions")
legend("topleft", legend=c("exact", "FFT"), 
        fill=c("black", "orange"))

## quantile functions
x <- seq(from = eps, to = 1-eps, length = 1000)
plot(x, q(AB)(x), type = "l", lwd = 5) 
lines(x , q(AB1)(x), col = "orange", lwd = 1) 
title("Quantile functions")
legend(0, q(AB)(eps, lower.tail = FALSE), 
       legend = c("exact", "FFT"), 
        fill = c("black", "orange"))

## Since the plots of the results show no 
## recognizable differencies, we also compute 
## the total variation distance of the densities 
## and the Kolmogorov distance of the cdfs

## total variation distance of densities
total.var <- function(z, N1, N2){
    0.5*abs(d(N1)(z) - d(N2)(z))
}
dv <- integrate(f = total.var, lower = -Inf, 
                upper = Inf, rel.tol = 1e-8, 
                N1 = AB, N2 = AB1)
cat("Total variation distance of densities:\t")
print(dv) # 4.25e-07

### meanwhile realized in package "distrEx" 
### as TotalVarDist(N1,N2)

## Kolmogorov distance of cdfs 
## the distance is evaluated on a random grid
z <- r(Unif(Min=low, Max=upp))(1e5)
dk <- max(abs(p(AB)(z)-p(AB1)(z)))
cat("Kolmogorov distance of cdfs:\t", dk, "\n") 
# 2.03e-07

### meanwhile realized in package "distrEx" 
### as KolmogorovDist(N1,N2)

## old distroptions
distroptions("TruncQuantile" = oldeps)
