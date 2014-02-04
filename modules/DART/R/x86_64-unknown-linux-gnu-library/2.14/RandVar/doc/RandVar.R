### R code from vignette source 'RandVar.Rnw'

###################################################
### code chunk number 1: exa1
###################################################
library(RandVar)
distroptions("withSweave" = TRUE) ## only for use in Sweave - document; set to FALSE else!
(X <- RealRandVariable(Map = list(function(x){x}, function(x){x^2}), Domain = Reals(), Range = Reals()))
Map(X)
evalRandVar(X, 2)
evalRandVar(X, as.matrix(seq(2, 10, 2)))
R1 <- exp(X-1)
Map(R1)
R2 <- exp(X-1:2)
Map(R2)
(Y <- RealRandVariable(Map = list(function(x){sin(x)}, function(x){cos(x)}), Domain = Reals(), Range = Reals()))
Map(Y)
R3 <- X %*% Y
dimension(R3)
#evalRandVar(R3, 2)
2*sin(2) + 2^2*cos(2)
(R4 <- X %*% t(Y))
dimension(R4)
#evalRandVar(R4, 2)
(M <- matrix(c(2*sin(2), 2^2*sin(2), 2*cos(2), 2^2*cos(2)), ncol = 2))
(R5 <- M %*% R4)


###################################################
### code chunk number 2: exa2
###################################################
D <- Norm()
E(object = D, fun = X)
E(D)
var(D)
(CD <- LMCondDistribution(theta = 1))
E(object = CD, fun = X, cond = 2)
E(Norm(mean = 2))
E(Norm(mean = 2), fun = function(x){x^2})


