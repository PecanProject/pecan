require(distr)
options("newDevice"=TRUE)

## argument names conformal to use in distrEx

if(!isGeneric("Range"))
    setGeneric("Range",
    function(object, ...) standardGeneric("Range"))
    

setMethod("Range",
          signature(object = "AbscontDistribution",
          e2 = "numeric"),
          function(object, e2){
            if ((e2 <= 0) || !isTRUE(all.equal(e2,floor(e2))))
               stop("second argument needs to be a positive natural")

            ## new random number function
            rnew <- function(n){
              rn1 <- matrix(r(object)(n*e2),n,e2)
              m <- apply(rn1,1,min)
              M <- apply(rn1,1,max)
              M-m
            }

            fnt <- function(u0,s) d(object)(s)*(p(object)(s+u0)-p(object)(s))^(e2-1)*e2
            fu <- function(u) integrate(fnt, lower=-Inf, upper=Inf, u0=u)$value*(u>0)
            fnt0 <- function(u0,s) d(object)(s)*d(object)(s+u0)*(p(object)(s+u0)-p(object)(s))^(e2-2)*e2*(e2-1)
            fu0 <- function(u) integrate(fnt0, lower=-Inf, upper=Inf, u0=u)$value*(u>0)
            xgrid <- seq(0,
                         q(object)(1e-6, lower.tail = FALSE)-q(object)(1e-6),
                         length = getdistrOption("DefaultNrGridPoints")/10)
            fx <- sapply(xgrid, fu)
            pnew <- approxfun(xgrid, fx, yleft = 0, yright = 1)
            fx0 <- sapply(xgrid, fu0)
            dnew <- approxfun(xgrid, fx0, yleft = 0, yright = 0)

            ## new quantile function
            lower <- q(object)(0)
            upper <- q(object)(1)

            maxquantile = q(object)(1e-6, lower.tail = FALSE)
            minquantile = q(object)(1e-6)

            qfun1 <- function(x){
              if(x == 0) return(lower)
              if(x == 1) return(upper)
              fun <- function(t) pnew(t) - x
              uniroot(f = fun,
                  interval = c(0,
                               maxquantile-minquantile))$root
            }
            qfun2 <- function(x)
              sapply(x, qfun1)

            return(new("AbscontDistribution", r = rnew,
                   d = dnew, p = pnew, q = qfun2))
          })

N <- Norm(mean = 0, sd = 1)

R <- Range(N, 5)
plot(R)

cat("Hit <enter> to continue...")
readline()

setMethod("Range",
          signature(object = "DiscreteDistribution",
          e2 = "numeric"),
          function(object, e2){
            if ((e2 <= 0) || !isTRUE(all.equal(e2,floor(e2))))
               stop("second argument needs to be a positive natural")

            supp <- support(object)
            suppnew <- sort(unique(as.vector(outer(supp,supp,"-"))))
            suppnew <- suppnew[suppnew>=0]
            print(suppnew)
            
            ## new random number function
            rnew <- function(n){
              rn1 <- matrix(r(object)(n*e2),n,e2)
              m <- apply(rn1,1,min)
              M <- apply(rn1,1,max)
              M-m
            }

            fnt <- function(u0,s) (p(object)(s+u0)-p(object)(s)+d(object)(s))^e2 -
                                  (p(object)(s+u0)-p(object)(s))^e2
            pnew <- function(x) sapply(x, function(u) sum(fnt(u, s = supp)))*(x>=0)
            dnew <- function(x){
             (pnew(x)-pnew(x-getdistrOption("DistrResolution")*100))*(d(object)(x)>0)*(x>=0)
              }

            cumprob <- pnew(suppnew)
            qnew <- function(x){ suppnew[sum(cumprob<x)+1] }

            return(new("DiscreteDistribution", r = rnew, d = dnew, p = pnew,
                   q = qnew, support = suppnew))

          })

B1 <- Binom(6, 0.5)
R <- Range(B1, 2)
plot(R)
