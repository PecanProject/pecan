require(distrMod)
options("newDevice"=TRUE)

CensoredPoisFamily <- function(lambda = 1, trunc.pt = 2){
    ## name
    name <- "Censored Poisson family"
    ## central distribution
    distribution <- Truncate(Pois(lambda = lambda), lower= trunc.pt )
    param0 <- lambda
    names(param0) <- "lambda"
    ## parameter definition
    param <- ParamFamParameter(name = "positive mean",
                               main = param0)

    ## mapping theta -> P_theta
    modifyParam <- function(theta){
                      Truncate(Pois(lambda = theta), lower = trunc.pt)}

    ## search interval for reasonable parameters
    startPar <- function(x,...) c(.Machine$double.eps,max(x))

    ## what to do in case of leaving the parameter domain
    makeOKPar <- function(param) {if(param<=0) return(.Machine$double.eps)
                                  return(param)}

    ## mapping theta -> Lambda_theta
    L2deriv.fct <- function(param){
                   lambda <- main(param)
                   fct <- function(x){}
                   body(fct) <- substitute({
                                 x/lambda-ppois(trunc.pt-1, lambda = lambda,
                                                lower.tail=FALSE)/
                                          ppois(trunc.pt, lambda = lambda,
                                                lower.tail=FALSE)},
                                list(lambda = lambda))
                   return(fct)}

    res <- L2ParamFamily(name = name, distribution = distribution,
                         param = param, modifyParam = modifyParam,
                         L2deriv.fct = L2deriv.fct,
                         startPar = startPar, makeOKPar = makeOKPar)

    ## a simplified call
    res@fam.call <- substitute(CensoredPoisFamily(lambda = l, trunc.pt = t),
                               list(l = lambda, t = trunc.pt))
    return(res)
}
CP <- CensoredPoisFamily(3,2)
CP.data <- r(CP)(40)
(m<- MLEstimator(CP.data, CP))
confint(m)
plot(profile(m))
(md.kolm<- MDEstimator(CP.data, CP))
(md.CvM<-  MDEstimator(CP.data, CP, distance = CvMDist,
           asvar.fct = distrMod:::.CvMMDCovariance))
confint(md.CvM)

