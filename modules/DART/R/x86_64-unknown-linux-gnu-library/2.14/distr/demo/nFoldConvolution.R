##########################################################
## Demo: n-fold convolution of absolutely continuous            
##       probability distributions
##########################################################
require(distr)
options("newDevice"=TRUE)

### from version 1.9 of distr on available in the package
### already

if(!isGeneric("convpow")) 
    setGeneric("convpow", 
    function(D1, ...) standardGeneric("convpow"))

##########################################################
## Function for n-fold convolution
## -- absolute continuous distribution -- 
##########################################################

##implentation of Algorithm 3.4. of 
# Kohl, M., Ruckdeschel, P., Stabla, T. (2005): 
#   General purpose convolution algorithm for distributions 
#   in S4-Classes by means of FFT.
# Technical report, Feb. 2005. Also available in
# http://www.uni-bayreuth.de/departments/math/org/mathe7/
#       /RUCKDESCHEL/pubs/comp.pdf


setMethod("convpow",
          signature(D1 = "AbscontDistribution"),
          function(D1, N){
            if((N<1)||(abs(floor(N)-N)>.Machine$double.eps))
              stop("N has to be a natural greater than 0")
            
            m <- getdistrOption("DefaultNrFFTGridPointsExponent")

    ##STEP 1

            lower <- ifelse((q(D1)(0) > - Inf), q(D1)(0), 
                             q(D1)(getdistrOption("TruncQuantile"))
                           ) 
            upper <- ifelse((q(D1)(1) < Inf), q(D1)(1), 
                             q(D1)(getdistrOption("TruncQuantile"),
                                   lower.tail = FALSE)
                           )

    ##STEP 2

            M <- 2^m
            h <- (upper-lower)/M
            if(h > 0.01)
              warning(paste("Grid for approxfun too wide,", 
                      "increase DefaultNrFFTGridPointsExponent"))
            x <- seq(from = lower, to = upper, by = h)
            p1 <- p(D1)(x)

    ##STEP 3

            p1 <- p1[2:(M + 1)] - p1[1:M]

    ##STEP 4
    
            ## computation of DFT
            pn <- c(p1, numeric((N-1)*M))
            fftpn <- fft(pn)

    ##STEP 5

            ## convolution theorem for DFTs
            pn <- Re(fft(fftpn^N, inverse = TRUE)) / (N*M)
            pn <- (abs(pn) >= .Machine$double.eps)*pn
            i.max <- N*M-(N-2)
            pn <- c(0,pn[1:i.max])
            dn <- pn / h
            pn <- cumsum(pn)

    ##STEP 6(density)

            ## density 
            x <- seq(from = N*lower+N/2*h, 
                     to = N*upper-N/2*h, by=h)            
            x <- c(x[1]-h,x[1],x+h)
            dnfun1 <- approxfun(x = x, y = dn, yleft = 0, yright = 0)

    ##STEP 7(density)
 
            standardizer <- sum(dn) - (dn[1]+dn[i.max+1]) / 2
            dnfun2 <- function(x) dnfun1(x) / standardizer

    ##STEP 6(cdf)
    
            ## cdf with continuity correction h/2
            pnfun1 <- approxfun(x = x+0.5*h, y = pn, 
                                yleft = 0, yright = pn[i.max+1])

    ##STEP 7(cdf)
   
            pnfun2 <- function(x) pnfun1(x) / pn[i.max+1]


            ## quantile with continuity correction h/2
            yleft <- ifelse(((q(D1)(0) == -Inf)|
                             (q(D1)(0) == -Inf)), -Inf, N*lower)
            yright <- ifelse(((q(D1)(1) == Inf)|
                              (q(D1)(1) == Inf)), Inf, N*upper)    
            w0 <- options("warn")
            options(warn = -1)
            qnfun1 <- approxfun(x = pnfun2(x+0.5*h), y = x+0.5*h, 
                                yleft = yleft, yright = yright)
            qnfun2 <- function(x){ 
            ind1 <- (x == 0)*(1:length(x))
            ind2 <- (x == 1)*(1:length(x))
            y <- qnfun1(x)
            y <- replace(y, ind1[ind1 != 0], yleft)
            y <- replace(y, ind2[ind2 != 0], yright)
            return(y)
            }
            options(w0)

            rnew = function(N) apply(matrix(r(e1)(n*N), ncol=N), 1, sum)

            return(new("AbscontDistribution", r = rnew, d = dnfun1, p = pnfun2, 
                        q = qnfun2, .withArith = TRUE,
                     .withSim = FALSE))
})

## initialize a normal distribution
A <- Norm(mean=0, sd=1)

## convolution power
N <- 10 

## convolution via FFT 
AN <- convpow(as(A,"AbscontDistribution"), N)
##  ... for the normal distribution , 'convpow' has an "exact"
##      method by version 1.9 so the as(.,.)  is needed to
##      see how the algorithm above works

## convolution exact
AN1 <- Norm(mean=0, sd=sqrt(N))

## plots of the results
eps <- getdistrOption("TruncQuantile")
par(mfrow=c(1,3))
low <- q(AN1)(eps)
upp <- q(AN1)(eps, lower.tail = FALSE)
x <- seq(from = low, to = upp, length = 10000)

## densities
plot(x, d(AN1)(x), type = "l", lwd = 5)
lines(x , d(AN)(x), col = "orange", lwd = 1)
title("Densities")
legend("topleft", legend=c("exact", "FFT"), 
        fill=c("black", "orange"))

## cdfs
plot(x, p(AN1)(x), type = "l", lwd = 5)
lines(x , p(AN)(x), col = "orange", lwd = 1)
title("Cumulative distribution functions")
legend("topleft", legend=c("exact", "FFT"), 
        fill=c("black", "orange"))

## quantile functions
x <- seq(from = eps, to = 1-eps, length = 1000)
plot(x, q(AN1)(x), type = "l", lwd = 5)
lines(x , q(AN)(x), col = "orange", lwd = 1) 
title("Quantile functions")
legend("topleft",
       legend = c("exact", "FFT"), 
        fill = c("black", "orange"))
