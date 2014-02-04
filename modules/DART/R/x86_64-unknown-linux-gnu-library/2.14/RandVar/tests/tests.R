library(RandVar)

###############################################################################
## start of tests
###############################################################################

## RandVariable
(R1 <- new("RandVariable"))
(R1 <- RandVariable())
Map(R1)
Domain(R1)
Range(R1)
Map(R1) <- list(function(x){ceiling(x)}, function(x){floor(x)})
Domain(R1) <- Reals()
Range(R1) <- Naturals()
R1
Map(R1)
length(R1)

R2 <- R1
Domain(R2) <- Naturals()
compatibleDomains(R1, R2)
Domain(R2) <- NULL
compatibleDomains(R1, R2)
Domain(R2) <- EuclideanSpace(dimension = 1)
compatibleDomains(R1, R2)
Domain(R2) <- EuclideanSpace(dimension = 2)
compatibleDomains(R1, R2)


## list of functions
L1 <- list(function(x){x}, function(x){x^2}, function(x){x^3}, function(x){x^4})
L2 <- list(function(x){x}, function(x){x^2}, function(x){x^3}, function(x){x^4}, 
           function(x){x^5}, function(x){x^6})
L3 <- list(function(x){x}, function(x){x^2}, function(x){x^3}, function(x){x^4}, 
           function(x){x^5}, function(x){x^6}, function(x){x^7}, function(x){x^8})
L4 <- list(function(x){exp(x)}, function(x){abs(x)}, 
           function(x){sin(x)}, function(x){floor(x)})

## EuclRandVariable
(R3 <- new("EuclRandVariable", Map = L4, Domain = Reals(), Range = Reals()))
(R3 <- EuclRandVariable(L1, Domain = Reals(), dimension = 1))
Map(R3)
Range(R3) <- Reals()
R3[2]
Map(R3[3])
Map(R3[c(1,2,4)])
Map(R3[2:4])
evalRandVar(R3, rnorm(1))
x <- as.matrix(rnorm(10))
res.R3 <- evalRandVar(R3, x)
res.R3[2,,] # results for Map(R3)[[2]](x)
res.R3[2,1,] # results for Map(R3)[[2]](x[1,])
# assuming a probability space with 
# distribution Exp()
res.R31 <- evalRandVar(R3, x, Gammad())
res.R31[2,,] # results for Map(R3)[[2]](x)
res.R31[2,1,] # results for Map(R3)[[2]](x[1,])
dimension(R3)

R4 <- EuclRandVariable(L4, Domain = Reals(), dimension = 1)
DL1 <- imageDistr(R4, Norm())
plot(DL1)

Domain(R4) <- EuclideanSpace(dimension = 2)
Range(R4) <- EuclideanSpace(dimension = 2)
(X <- matrix(c(x, rnorm(10)), ncol = 2))
res2.R4 <- evalRandVar(R4, X)
res2.R4[3,,1] # results for Map(R4)[[3]](X[,1])
dimension(R4)


## EuclRandMatrix
(R5 <- as(R4, "EuclRandMatrix"))
dimension(R5)
Domain(R5) <- Reals()
Range(R5) <- Reals()
(DL2 <- imageDistr(R5, Norm())) # list of distributions
plot(DL2) # vgl. DL1

Domain(R5) <- EuclideanSpace(dimension = 2)
Range(R5) <- EuclideanSpace(dimension = 2)
#res1.R5 <- evalRandVar(R5, rnorm(2))
#res1.R5[1,1,] # result for map of R5[1,1]

res2.R5 <- evalRandVar(R5, X)
res2.R5[,,1,2] 
res2.R5[,1,2,1:2] 
res2.R5[1,1,1:2,2]

new("EuclRandMatrix", Map = L2, Dim = as.integer(c(3,2)), Domain = Reals(), Range = Reals())
(R6 <- EuclRandMatrix(Map = L2, ncol = 2, Domain = Reals(), Range = Reals()))
R6[1:2, 2]
R6[1:2, 1:2]
Map(R6[1,2])
Map(t(R6)[2,1])
dimension(R6)

R7 <- EuclRandMatrix(Map = L4, ncol = 2, Domain = Reals(), dimension = 1)
dimension(R7)
(DL3 <- imageDistr(R7, Norm()))
plot(DL3) # vgl. DL1, DL2


## EuclRandVarList
new("EuclRandVarList")
(RL1 <- EuclRandVarList(R3, R6, R7))
dimension(RL1)
as(R4, "EuclRandVarList")
as(R6, "EuclRandVarList")
Domain(R5) <- Reals()
Range(R5) <- Reals()
(RL2 <- EuclRandVarList(R5, R7))
(DL4 <- imageDistr(RL2, Norm()))
plot(DL4)


## "Math" group
system.time(Map(log(abs(R4))), gcFirst = TRUE)
system.time(Map(gamma(R7)), gcFirst = TRUE)
system.time(Map(exp(RL1)[[1]]), gcFirst = TRUE)


## "Arith" group
system.time(Map(3 + R3), gcFirst = TRUE)
Map(c(1,3,5) * R3)
try(1:5 * R3) # error
Map(1:2 * R4)
Map(2/R6)
Map(c(1,3,5) %% R6)
Map(R4 - 5)
Map(R6 %/% 2)
Map(R3 ^ R3)
Map(R7 * R7)
Map((1 + RL1)[[1]])
Map((RL1 * 2)[[2]])
system.time(Map((RL1 %% RL1)[[3]]), gcFirst = TRUE)


## "%*%"
M1 <- matrix(1:16, ncol = 8)
(R8 <- M1 %*% R4)
Map(R4)
M1[1,]
Map(R8)[[1]]
M1[2,]
Map(R8)[[2]]
M2 <- matrix(1:2, ncol = 2)
(R9 <- M2 %*% R7)
Map(R7)
Map(R9)
Map(1:4 %*% R3) # inner product
Map(1:2 %*% R7) # corresponds to Map(t(1:2) %*% R7)
Map(R4 %*% 1:8) # inner product
Map(R9 %*% 3:4)
Map(R9 %*% matrix(1:4, nrow = 2))
(R10 <- R3 %*% matrix(1:16, ncol = 4))
Map(R10)
R3 %*% R3 # inner product
R3 %*% R10
system.time(R9 %*% R7, gcFirst = TRUE)
(RL3 <- diag(dimension(RL1)) %*% RL1)


## %m% "matrix multiplication" for 'EuclRandVarList'
system.time(RL4 <- EuclRandVarList(t(R3[1:2]), R7) %m% EuclRandVarList(R6, R9), gcFirst = TRUE)

## integration
MVD <- DiscreteMVDistribution(supp = matrix(c(r(Pois(5))(10), r(Pois(5))(10)), ncol = 2))
support(MVD)
E(MVD)
E(as(MVD, "MultivariateDistribution"))
E(MVD, function(x){x})
E(MVD, function(x){x}, useApply = FALSE)
E(as(MVD, "MultivariateDistribution"), function(x){x})
E(as(MVD, "MultivariateDistribution"), function(x){x}, useApply = FALSE)
E(MVD, function(x){x^2})
E(as(MVD, "MultivariateDistribution"), function(x){x^2}, useApply = FALSE)
E(MVD, function(x){x %*% t(x)})
E(as(MVD, "MultivariateDistribution"), function(x){x %*% t(x)})

R1 <- RealRandVariable(list(function(x){x}, function(x){x^2}), Domain = Reals())
R2 <- EuclRandMatrix(list(function(x){x}, function(x){x^2}), ncol = 1, Domain = Reals(), dimension = 1)
E(Norm(), R1)
E(Norm(), R1, useApply = FALSE)
E(Norm(), R2)
E(Norm(), R2, useApply = FALSE)
R3 <- EuclRandVarList(R1, R2)
E(Norm(), R3)
E(Norm(), R3, useApply = FALSE)

R1 <- EuclRandVariable(list(function(x){x}, function(x){x^2}), Domain = EuclideanSpace(2), dimension = 2)
R2 <- EuclRandMatrix(list(function(x){x}, function(x){x^2}), ncol = 1, Domain = EuclideanSpace(2), dimension = 2)
E(MVD, R1)
E(MVD, R1, useApply = FALSE)
E(MVD, R2)[1:2,,]
E(MVD, R2, useApply = FALSE)[1:2,,]
R3 <- EuclRandVarList(R1, R2)
E1 <- E(MVD, R3)
E1[[1]]
E1[[2]][1:2,,]
E(MVD, R3, useApply = FALSE)

CD <- LMCondDistribution(theta = 1)
E(CD, cond = 2)
E(CD, cond = 2, useApply = FALSE)
E(CD, function(x){x}, cond = 2)
E(CD, function(x){x}, cond = 2, useApply = FALSE)
E(CD, function(x, cond){2*x}, cond = 2, withCond = FALSE)
E(CD, function(x, cond){2*x}, cond = 2, withCond = TRUE, useApply = FALSE)
E(CD, function(x){x^2}, cond = 2)
E(CD, function(x){x^2}, cond = 2, useApply = FALSE)
E(CD, function(x, cond){x^2*cond}, cond = 2, withCond = TRUE)
E(CD, function(x, cond){x^2*cond}, cond = 2, withCond = TRUE, useApply = FALSE)
Range(R1) <- Reals()
Domain(R1) <- Reals()
E(CD, R1, cond = 2)
E(CD, R1, cond = 2, useApply = FALSE)
R3 <- EuclRandVariable(list(function(x){x[2]*x[1]}, function(x){x[2]*x[1]^2}), 
                       Domain = EuclideanSpace(2), dimension = 1)
E(CD, R3, cond = 2, withCond = TRUE)
Range(R2) <- Reals()
Domain(R2) <- Reals()
E(CD, R2, cond = 2)
E(CD, R2, cond = 2, useApply = FALSE)
R4 <- EuclRandMatrix(list(function(x){x[2]*x[1]}, function(x){x[2]*x[1]^2}), 
                     ncol = 1, Domain = EuclideanSpace(2), dimension = 1)
E(CD, R4, cond = 2, withCond = TRUE)
R5 <- EuclRandVarList(R1, R2)
E(CD, R5, cond = 2)
E(CD, R5, cond = 2, useApply = FALSE)
R6 <- EuclRandVarList(R3, R4)
E(CD, R6, cond = 2, withCond = TRUE)


###############################################################################
## end of tests
###############################################################################

q("no")
