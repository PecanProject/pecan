#####################################################
## Demo: Stationary Regressor Distribution of an AR(1)    
##       Process
#####################################################
require(distr)
options("newDevice"=TRUE)

## Approximation of the stationary regressor 
## distribution of an AR(1) process 
##       X_t = phi X_{t-1} + V_t 
## where V_t i.i.d N(0,1) and phi\in(0,1)
## We obtain 
##    X_t = \sum_{j=1}^\infty phi^j V_{t-j}
## i.e., X_t \sim N(0,1/(1-phi^2))
phi <- 0.5

## casting of V as absolutely continuous distributions
## that is, ``forget'' that V is a normal distribution
V <- as(Norm(), "AbscontDistribution")

## for higher precision we change the global variable
## "TruncQuantile" from 1e-5 to 1e-8
oldeps <- getdistrOption("TruncQuantile")
eps <- 1e-8
distroptions("TruncQuantile" = eps)

## Computation of the approximation 
##      H=\sum_{j=1}^n phi^j V_{t-j}
## of the stationary regressor distribution 
## (via convolution using FFT)
H <- V
n <- 15 
## may take some time
for(i in 1:n){Vi <- phi^i*V; H <- H + Vi } 

## the stationary regressor distribution (exact)
X <- Norm(sd=sqrt(1/(1-phi^2)))

#############################
## plots of the results
#############################
par(mfrow=c(1,3))
low <- q(X)(1e-15)
upp <- q(X)(1e-15, lower.tail = FALSE)
x <- seq(from = low, to = upp, length = 10000)

## densities
plot(x, d(X)(x),type = "l", lwd = 5)
lines(x , d(H)(x), col = "orange", lwd = 1)
title("Densities")
legend("topleft", legend=c("exact", "FFT"), 
        fill=c("black", "orange"))

## cdfs
plot(x, p(X)(x),type = "l", lwd = 5)
lines(x , p(H)(x), col = "orange", lwd = 1)
title("Cumulative distribution functions")
legend("topleft", legend=c("exact", "FFT"), 
        fill=c("black", "orange"))

## quantile functions
x <- seq(from = eps, to = 1-eps, length = 1000)
plot(x, q(X)(x),type = "l", lwd = 5)
lines(x , q(H)(x), col = "orange", lwd = 1)
title("Quantile functions")
legend( "topleft", 
        legend=c("exact", "FFT"), 
        fill=c("black", "orange"))

## Since the plots of the results show no 
## recognizable differencies, we also compute 
## the total variation distance of the densities 
## and the Kolmogorov distance of the cdfs

## total variation distance of densities
total.var <- function(z, N1, N2){
    0.5*abs(d(N1)(z) - d(N2)(z))
}
dv <- integrate(f = total.var, lower = -Inf, 
                upper = Inf, rel.tol = 1e-5, 
                N1=X, N2=H)
cat("Total variation distance of densities:\t")
print(dv) # 8.0e-06

### meanwhile realized in package "distrEx" 
### as TotalVarDist(N1,N2)


## Kolmogorov distance of cdfs 
## the distance is evaluated on a random grid
z <- r(Unif(Min=low, Max=upp))(1e5)
dk <- max(abs(p(X)(z)-p(H)(z)))
cat("Kolmogorov distance of cdfs:\t", dk, "\n") 
# 3.7e-05

### meanwhile realized in package "distrEx" 
### as KolmogorovDist(N1,N2)


## old distroptions
distroptions("TruncQuantile" = oldeps)
