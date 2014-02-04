library(distrEx)

###############################################################################
## some simple tests
###############################################################################

## DiscreteDistribution
D0 <- DiscreteDistribution(c(1:10, 2, 3, 4))
support(D0)
E(D0)
E(D0, function(x){x^3})
E(D0, function(x){x^3}, useApply = FALSE)


## Gumbel distribution
G1 <- new("Gumbel", loc = 1, scale = 2)
(G2 <- Gumbel(loc = 2, scale = 0.1))
plot(G1)
loc(G1)
scale(G1)
loc(G1) <- 0
scale(G1) <- 1
plot(G1)
E(G1) # Euler's constant
E(G2, function(x){x^2})
E(G2, function(x){x^2}, useApply = FALSE)


## MultivariateDistribution, DiscreteMVDistribution
(D1 <- new("MultivariateDistribution")) # Dirac measure in (0,0)
r(D1)(5)

(D2 <- DiscreteMVDistribution(supp = matrix(c(1:5, rep(3, 5)), ncol=2, byrow=TRUE)))
support(D2)
r(D2)(10)
d(D2)(support(D2))
p(D2)(lower = c(1,1), upper = c(3,3))
q(D2)
param(D2)
img(D2)

(e11 <- E(D2)) # expectation
(e12 <- E(as(D2, "MultivariateDistribution"))) # crude Monte-Carlo
(e13 <- E(D2, function(x){x}, useApply = FALSE)) # expectation
(e14 <- E(as(D2, "MultivariateDistribution"), function(x){x}, useApply = FALSE)) # crude Monte-Carlo
E(D2, function(x, e1){ (x-e1)%*%t(x-e1) }, e1 = e11) # covariance
E(as(D2, "MultivariateDistribution"), function(x, e1){ (x-e1)%*%t(x-e1) }, e1 = e12) # crude Monte-Carlo


## UnivariateCondDistribution, AbscontCondDistribution, 
## DiscreteCondDistribution
(D3 <- new("UnivariateCondDistribution"))
r(D3)
d(D3)
p(D3)
q(D3)
img(D3) # bug in methods package in R Version R 2.0.1 patched (2004-12-09)?
param(D3)
cond(D3)

(D4 <- LMCondDistribution(theta = 1)) # corresponds to Norm(cond, 1)
plot(D4)
r(D4)
d(D4)
p(D4)
q(D4)
param(D4)
cond(D4)
img(D4)

x <- seq(from=-3, to = 3, length=10)
d(D4)(x, cond = 1)
d(Norm(mean=1))(x)

N <- Norm(mean = 2)
E(D4, cond = 1)
E(D4, cond = 1, useApply = FALSE)
E(as(D4, "UnivariateCondDistribution"), cond = 1)
E(as(D4, "UnivariateCondDistribution"), cond = 1, useApply = FALSE)
E(D4, function(x){x^2}, cond = 2)
E(D4, function(x){x^2}, cond = 2, useApply = FALSE)
E(N, function(x){x^2})
E(as(N, "UnivariateDistribution"), function(x){x^2}, useApply = FALSE) # crude Monte-Carlo
E(D4, function(x, cond){cond*x^2}, cond = 2, withCond = TRUE)
E(D4, function(x, cond){cond*x^2}, cond = 2, withCond = TRUE, useApply = FALSE)
E(N, function(x){2*x^2})
E(as(N, "UnivariateDistribution"), function(x){2*x^2}, useApply = FALSE) # crude Monte-Carlo

(D5 <- new("DiscreteCondDistribution"))
r(D5)
img(D5) # bug in methods package in R Version R 2.0.1 patched (2004-12-09)?

## DistrList, UnivarDistrList
(L1 <- new("DistrList", list(D1, D2, Norm())))
#plot(L1) not yet implemented
(L2 <- DistrList(D1, D2, D2))
#plot(L2) not yet implemented

(L3 <- new("UnivarDistrList", list(Binom(size=10), Td(df=10), Fd(df1 = 10, df2=10))))
plot(L3) 
try(L4 <- UnivarDistrList(D4, Exp(rate = 0.5), Pois(lambda = 10)))
D4

## Reals, Naturals, EuclideanSpace
Reals()
Naturals()
EuclideanSpace(dimension = 2)


## convex contamination
(D6 <- ConvexContamination(Norm(), Norm(mean=5), size = 0.1))
plot(D6)
ContaminationSize(Norm(), D6)
ContaminationSize(Norm(mean = 5), D6)

(D7 <- ConvexContamination(Binom(size=10), Pois(lambda=5), size = 0.2))
plot(D7)
ContaminationSize(Binom(size=10), D7)
ContaminationSize(Pois(lambda = 5), D7)

(D8 <- ConvexContamination(Gumbel(), Nbinom(size=10), size = 0.1))
plot(p(D8), from = -2, to = 5)
plot(q(D8), from = 1e-5, to = 1-1e-5)


## TotalVarDist, KolmogorovDist, HellingerDist
TotalVarDist(Norm(), Gumbel())
KolmogorovDist(Norm(), Gumbel())
HellingerDist(Norm(), Gumbel())

TotalVarDist(Norm(), Td(10))
KolmogorovDist(Norm(), Td(10))
HellingerDist(Norm(), Td(10))

TotalVarDist(Norm(mean = 50, sd = sqrt(25)), Binom(size = 100)) # mutually singular
KolmogorovDist(Norm(mean = 50, sd = sqrt(25)), Binom(size = 100))
HellingerDist(Norm(mean = 50, sd = sqrt(25)), Binom(size = 100)) # mutually singular

TotalVarDist(Pois(10), Binom(size = 20))
KolmogorovDist(Pois(10), Binom(size = 20))
HellingerDist(Pois(10), Binom(size = 20))


## clipped moments
N <- Norm(mean = 3, sd = 2)
m1df(N, upper = 1.5)
m1df(as(N, "AbscontDistribution"), upper = 1.5)
m1df(as(N, "UnivariateDistribution"), upper = 1.5) # crude Monte-Carlo
m2df(N, upper = 1.5)
m2df(as(N, "AbscontDistribution"), upper = 1.5)
m2df(as(N, "UnivariateDistribution"), upper = 1.5) # crude Monte-Carlo

E <- Exp(3.3)
m1df(E, upper = 2)
m1df(as(E, "AbscontDistribution"), upper = 2)
m1df(as(E, "UnivariateDistribution"), upper = 2) # crude Monte-Carlo
m2df(E, upper = 2)
m2df(as(E, "AbscontDistribution"), upper = 2)
m2df(as(E, "UnivariateDistribution"), upper = 2) # crude Monte-Carlo

Ch <- Chisq(df = 2)
m1df(Ch, upper = 2)
m1df(as(Ch, "AbscontDistribution"), upper = 2)
m1df(as(Ch, "UnivariateDistribution"), upper = 2) # crude Monte-Carlo
m2df(Ch, upper = 2)
m2df(as(Ch, "AbscontDistribution"), upper = 2)
m2df(as(Ch, "UnivariateDistribution"), upper = 2) # crude Monte-Carlo

B <- Binom(size = 25, prob = 0.25)
m1df(B, upper = 15)
m1df(as(B, "DiscreteDistribution"), upper = 15)
m1df(as(B, "UnivariateDistribution"), upper = 15) # crude Monte-Carlo
m2df(B, upper = 15)
m2df(as(B, "DiscreteDistribution"), upper = 15)
m2df(as(B, "UnivariateDistribution"), upper = 15) # crude Monte-Carlo

P <- Pois(20)
m1df(P, upper = 15)
m1df(as(P, "DiscreteDistribution"), upper = 15)
m1df(as(P, "UnivariateDistribution"), upper = 15) # crude Monte-Carlo
m2df(P, upper = 15)
m2df(as(P, "DiscreteDistribution"), upper = 15)
m2df(as(P, "UnivariateDistribution"), upper = 15) # crude Monte-Carlo


## distrExOptions
distrExOptions()
getdistrExOption("m1dfRelativeTolerance")
distrExOptions("m1dfRelativeTolerance" = 1e-5)
getdistrExOption("m1dfRelativeTolerance")
distrExOptions(m2dfRelativeTolerance = 1e-6)
distrExOptions()
distrExOptions(m1dfRelativeTolerance = .Machine$double.eps^0.25) #default
distrExOptions("m2dfRelativeTolerance" = .Machine$double.eps^0.25) # default
distrExOptions()


###############################################################################
## end of tests
###############################################################################

q("no")
