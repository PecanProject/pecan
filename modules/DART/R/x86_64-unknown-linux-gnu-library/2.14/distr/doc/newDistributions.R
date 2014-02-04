### R code from vignette source 'newDistributions.Rnw'

###################################################
### code chunk number 1: SweaveListingsPreparations
###################################################
require(SweaveListingUtils)
SweaveListingPreparations()
setToBeDefinedPkgs(pkgs = c("distr","distrEx"),
                   keywordstyles = "\\bf\\color{distrCol}")


###################################################
### code chunk number 2: Prepa
###################################################
## preparation: set option withSweave to true
require(distr)
distroptions(withSweave = TRUE)
options(width=70)


###################################################
### code chunk number 3: exam1
###################################################
require(distr)
N <- Norm(mean = 2, sd = 1.3)
P <- Pois(lambda = 1.2)
Z <- 2*N + 3 + P
Z
plot(Z, panel.first = grid(), lwd=2)
p(Z)(0.4)
q(Z)(0.3)
Zs <- r(Z)(50)
Zs


###################################################
### code chunk number 4: DiscrDist
###################################################
D <- DiscreteDistribution(supp = c(1,5,7,21), prob = c(0.1,0.1,0.6,0.2))
D
plot(D, panel.first = grid(), lwd = 2)


###################################################
### code chunk number 5: AbscDist
###################################################
AC <- AbscontDistribution(d = function(x) exp(-abs(x)^3), withStand = TRUE)
AC
plot(AC, panel.first = grid(), lwd = 2)


###################################################
### code chunk number 6: AllClass1
###################################################
lstinputSourceFromRForge("distr","R","AllClasses.R","distr",
                     "## Class: BinomParameter", "#-")


###################################################
### code chunk number 7: AllClass2
###################################################
lstinputSourceFromRForge("distr","R","AllClasses.R","distr",
                     from = "## Class: binomial distribution",
                     to = "contains = \"LatticeDistribution\"", 
                     offset.after = 1)


###################################################
### code chunk number 8: BinomDist1
###################################################
lstinputSourceFromRForge("distr","R","BinomialDistribution.R","distr",
                     from = c("## Access Methods", "## wrapped access methods"),
                     to = c("setReplaceMethod\\(\"prob\", \"BinomParameter\"",
                            "size = value\\)\\)") ,
                     offset.after = c(1,1))


###################################################
### code chunk number 9: AllGenerics
###################################################
lstinputSourceFromRForge("distr","R","AllGenerics.R","distr",
                     from = "if\\(!isGeneric\\(\"size\"",
                     to = "setGeneric\\(\"prob\"") 


###################################################
### code chunk number 10: BinomDist2
###################################################
lstinputSourceFromRForge("distr","R","BinomialDistribution.R","distr",
                     from = "setValidity\\(\"Binom",
                     to = "else return\\(TRUE\\)",
                     offset.after = 1)


###################################################
### code chunk number 11: BinomDist3
###################################################
lstinputSourceFromRForge("distr","R","BinomialDistribution.R","distr",
                     from = "Binom <- function",
                     to = "Binom <- function")


###################################################
### code chunk number 12: BinomDist4
###################################################
lstinputSourceFromRForge("distr","R","BinomialDistribution.R","distr",
                     from = "## Convolution for",
                     to = "\\}\\)")


###################################################
### code chunk number 13: BinomParam
###################################################
lstinputSourceFromRForge("distr","man","BinomParameter-class.Rd","distr")


###################################################
### code chunk number 14: Binomclass
###################################################
lstinputSourceFromRForge("distr","man","Binom-class.Rd","distr")


###################################################
### code chunk number 15: Prepa2
###################################################
## preparation: set option withSweave to true
require(distrEx)


###################################################
### code chunk number 16: Expect
###################################################
lstinputSourceFromRForge("distrEx","R","Expectation.R","distr",
                     from = "\"E\", signature\\(object = \"Binom\"",
                     to = "\\}\\)")


###################################################
### code chunk number 17: var
###################################################
lstinputSourceFromRForge("distrEx","R","Functionals.R","distr",
                     from = "\"var\", signature\\(x = \"Binom\"",
                     to = "\\}\\)")


###################################################
### code chunk number 18: skew
###################################################
lstinputSourceFromRForge("distrEx","R","Skewness.R","distr",
                     from = "\"skewness\", signature\\(x = \"Binom\"",
                     to = "\\}\\)")


###################################################
### code chunk number 19: kurt
###################################################
lstinputSourceFromRForge("distrEx","R","Kurtosis.R","distr",
                     from = "\"kurtosis\", signature\\(x = \"Binom\"",
                     to = "\\}\\)")


###################################################
### code chunk number 20: cleanup
###################################################
#unloadNamespace("SweaveListingUtils")


