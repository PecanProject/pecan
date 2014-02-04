### R code from vignette source 'distrMod.Rnw'

###################################################
### code chunk number 1: library
###################################################
library(distrMod)
distrModoptions(show.details="minimal")
options(prompt = "R> ", continue = "+  ", width = 70, 
        useFancyQuotes = FALSE)


###################################################
### code chunk number 2: locNorm
###################################################
(N <- NormLocationFamily(mean = 3))
x <- r(N)(20)


###################################################
### code chunk number 3: locNorm1
###################################################
MLEstimator(x,N)
MDEstimator(x,N,distance=CvMDist,
            asvar.fct = distrMod:::.CvMMDCovariance)


###################################################
### code chunk number 4: exam1
###################################################
library(distr)
N <- Norm(mean = 2, sd = 1.3)
P <- Pois(lambda = 1.2)
Z <- 2*N + 3 + P
Z
plot(Z, cex.inner = 0.9)


###################################################
### code chunk number 5: exam11
###################################################
p(Z)(0.4)
q(Z)(0.3)
r(Z)(5)


###################################################
### code chunk number 6: expectation
###################################################
library(distrEx)
E(N)
E(P)
E(Z)
E(Z, fun=function(x) sin(x))


###################################################
### code chunk number 7: centralD
###################################################
myD <- AbscontDistribution(d = function(x) exp(-abs(x)^3), 
                           withS = TRUE)


###################################################
### code chunk number 8: locscFam
###################################################
(myFam <- L2LocationScaleFamily(loc = 3, scale = 2,
                          name = "location and scale family",
                          centraldistribution = myD))


###################################################
### code chunk number 9: GamFam
###################################################
(G <- GammaFamily(scale = 1, shape = 2))


###################################################
### code chunk number 10: CensPois
###################################################
CensoredPoisFamily <- function(lambda = 1, trunc.pt = 2){
    name <- "Censored Poisson family"
    distribution <- Truncate(Pois(lambda = lambda), 
                             lower = trunc.pt)
    param0 <- lambda
    names(param0) <- "lambda"
    param <- ParamFamParameter(name = "positive mean",
                               main = param0, 
                               fixed = c(trunc.pt=trunc.pt))
    modifyParam <- function(theta){
                      Truncate(Pois(lambda = theta), 
                               lower = trunc.pt)}
    startPar <- function(x,...) c(.Machine$double.eps,max(x))
    makeOKPar <- function(param){
        if(param<=0) return(.Machine$double.eps)
        return(param)
    }
    L2deriv.fct <- function(param){
        lambda <- main(param)
        fct <- function(x){}
        body(fct) <- substitute({
                       x/lambda-ppois(trunc.pt-1, 
                                      lambda = lambda,
                                      lower.tail=FALSE)/
                                ppois(trunc.pt, 
                                      lambda = lambda,
                                      lower.tail=FALSE)},
                                list(lambda = lambda))
        return(fct)
    }
    res <- L2ParamFamily(name = name, 
                         distribution = distribution,
                         param = param, 
                         modifyParam = modifyParam,
                         L2deriv.fct = L2deriv.fct,
                         startPar = startPar, 
                         makeOKPar = makeOKPar)
    res@fam.call <- substitute(CensoredPoisFamily(lambda = l, 
                                                  trunc.pt = t),
                               list(l = lambda, t = trunc.pt))
    return(res)
}
(CP <- CensoredPoisFamily(3,2))


###################################################
### code chunk number 11: plotFam
###################################################
layout(matrix(c(1,1,2,2,3,3,4,4,4,5,5,5), nrow = 2,
       byrow = TRUE))
plot(myFam, mfColRow = FALSE, cex.inner = 1,
     inner = c("density", "cdf", "quantile function",
               "location part", "scale part"))


###################################################
### code chunk number 12: GammaFamilyModify (eval = FALSE)
###################################################
## setMethod("modifyModel", 
##           signature(model = "GammaFamily", 
##                     param = "ParamFamParameter"),
##     function(model, param, ...){
##         M <- modifyModel(as(model, "L2ParamFamily"), 
##                          param = param, .withCall = FALSE)
##         M@L2derivSymm <- FunSymmList(OddSymmetric(SymmCenter = 
##                                             prod(main(param))),
##                                      NonSymmetric())
##         class(M) <- class(model)
##         return(M)
##     })


###################################################
### code chunk number 13: ParaTrafex
###################################################
mtrafo <- function(x){
     nms0 <- c("scale","shape")
     nms <- c("shape","rate")
     fval0 <- c(x[2], 1/x[1])
     names(fval0) <- nms
     mat0 <- matrix(c(0, -1/x[1]^2, 1, 0), nrow = 2, 
                     dimnames = list(nms,nms0))
     list(fval = fval0, mat = mat0)
}
(G.MASS <- GammaFamily(scale = 1, shape = 2, trafo = mtrafo))


###################################################
### code chunk number 14: mad-const
###################################################
(mad.const <- 1/q(myD)(0.75))


###################################################
### code chunk number 15: MLE-MASS
###################################################
set.seed(19)
x <- r(distribution(myFam))(50)
mydf <- function(x, loc, scale){
    y <- (x-loc)/scale; exp(-abs(y)^3)/scale
}
Med <- median(x)
MAD <- mad(x, constant = mad.const)
c(Med, MAD)
fitdistr(x, mydf, start = list("loc" = Med, "scale" = MAD))


###################################################
### code chunk number 16: MLE-stats4
###################################################
ll <- function(loc,scale){-sum(log(mydf(x,loc,scale)))}
mle(ll, start = list("loc" = Med, "scale" = MAD))


###################################################
### code chunk number 17: MCEstimator
###################################################
negLoglikelihood <- function(x, Distribution){
    res <- -sum(log(Distribution@d(x)))
    names(res) <- "Negative Log-Likelihood"
    return(res)
}
MCEstimator(x = x, ParamFamily = myFam, 
            criterion = negLoglikelihood)


###################################################
### code chunk number 18: PoisFamilyDef (eval = FALSE)
###################################################
## setClass("PoisFamily", contains = "L2ParamFamily")


###################################################
### code chunk number 19: NormLocationFamily (eval = FALSE)
###################################################
## setClass("NormLocationFamily", contains = "L2LocationFamily")


###################################################
### code chunk number 20: L2ScaleFamily (eval = FALSE)
###################################################
## setMethod("validParameter", 
##            signature(object = "L2ScaleFamily"),
##     function(object, param, tol=.Machine$double.eps){
##         if(is(param,"ParamFamParameter"))
##             param <- main(param)
##         if(!all(is.finite(param))) return(FALSE)
##         if(length(param)!=1) return(FALSE)
##         return(param > tol)
##     })


###################################################
### code chunk number 21: mlecalcEx (eval = FALSE)
###################################################
## setMethod("mleCalc", signature(x = "numeric", 
##                               PFam = "NormLocationScaleFamily"),  
##           function(x, PFam){ 
##               n <- length(x)
##               c(mean(x), sqrt((n-1)/n)*sd(x)) 
##           })


###################################################
### code chunk number 22: MLEstimator
###################################################
MLEstimator(x = x, ParamFamily = myFam)


###################################################
### code chunk number 23: MDEstimatorK
###################################################
MDEstimator(x = x, ParamFamily = myFam, 
            distance = KolmogorovDist)


###################################################
### code chunk number 24: NormScaleFam (eval = FALSE)
###################################################
## setMethod("mleCalc", signature(x = "numeric", 
##                                PFam = "NormScaleFamily"),
##     function(x, PFam, ...){
##         n <- length(x)
##         theta <- sqrt((n - 1)/n) * sd(x)
##         mn <- mean(distribution(PFam))
##         ll <- -sum(dnorm(x, mean = mn, sd = theta, log = TRUE))
##         names(ll) <- "neg.Loglikelihood"
##         crit.fct <- function(sd) -sum(dnorm(x, mean = mn, 
##                                         sd = sd, log = TRUE))  
##         param <- ParamFamParameter(name = "scale parameter", 
##                                    main = c("sd" = theta))
##         if(!hasArg(Infos)) Infos <- NULL
##         return(meRes(x, theta, ll, param, crit.fct, 
##                      Infos = Infos))
##     })


###################################################
### code chunk number 25: CIex
###################################################
set.seed(19)
y <- rgamma(50, scale = 3, shape = 2)
(MDest <- MDEstimator(x = y, ParamFamily = G.MASS, 
                     distance = CvMDist))
fitdistr(x = y, densfun = "gamma", 
         start = list("shape" = estimate(MDest)[1], 
                      "rate" = estimate(MDest)[2]))
(res <- MLEstimator(x = y, ParamFamily = G.MASS))
(ci <- confint(res))


###################################################
### code chunk number 26: profile
###################################################
par(mfrow=c(2,1))
plot(profile(res))


###################################################
### code chunk number 27: cleanup
###################################################
options("prompt"=">")


