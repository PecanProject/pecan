### R code from vignette source 'ExampleSweaveListingUtils.Rnw'

###################################################
### code chunk number 1: SweaveListingsPreparations
###################################################
require(SweaveListingUtils)
SweaveListingoptions(intermediate = FALSE)
SweaveListingPreparations()
setToBeDefinedPkgs(pkgs = c("SweaveListingUtils","distr"),
                   keywordstyles = c("\\bf\\color{blue}","\\bf\\color{red}"))


###################################################
### code chunk number 2: exam00
###################################################
x <- rnorm(3) # define random numbers -> insert a label <here> (*\label{comment}*)
print(round(x,2))
f <- function(x) sin(x) ## here is a ref: section(*~\ref{PrepSec}*)
a <- 2; b <- 3
# compute (*$\int_a^b f(x)\,dx$*)
integrate(f, lower=a, upper=b)$value


###################################################
### code chunk number 3: Prepa
###################################################
require(distr)


###################################################
### code chunk number 4: exam1
###################################################
require(distr)
N <- Norm(mean = 2, sd = 1.3)
P <- Pois(lambda = 1.2)
Z <- 2*N + 3 + P
Z
p(Z)(0.4)
q(Z)(0.3)


###################################################
### code chunk number 5: LookAtRset
###################################################
getSweaveListingOption("Rset")


###################################################
### code chunk number 6: LookAtRdset
###################################################
getSweaveListingOption("Rdset")


###################################################
### code chunk number 7: AllClass1
###################################################
lstinputSourceFromRForge("distr","R","AllClasses.R","distr",
                     "## Class: BinomParameter", "#-")


###################################################
### code chunk number 8: BinomParam
###################################################
lstinputSourceFromRForge("distr","man","BinomParameter-class.Rd","distr")


