require(distr)
options("newDevice"=TRUE)

if(!isGeneric("E")) 
   setGeneric("E", 
         function(object, fun) standardGeneric("E"))

setMethod("E",
        signature(object = "AbscontDistribution", 
                    fun = "function"),
        function(object, fun){
            integrand <- function(x) fun(x) * d(object)(x)
            return(integrate(f = integrand,
                             lower = q(object)(0),
                             upper = q(object)(1))$value)
          })

setMethod("E",
          signature(object = "DiscreteDistribution", 
                    fun = "function"),
          function(object, fun){
            supp = support(object)
            return(sum(fun(supp) * d(object)(supp)))
          })



# Example
id <- function(x) x
sq <- function(x) x^2

# Expectation and Variance of Binom(6,0.5)
B <- Binom(6, 0.5)
E(B, id)
E(B, sq) - E(B, id)^2

# Expectation and Variance of Norm(1,1)
N <- Norm(1, 1)
E(N, id)
E(N, sq) - E(N, id)^2
