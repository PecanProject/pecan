## Compare the FORTRAN and C++ versions of PROSPECT
library(microbenchmark)
library(ggplot2)
library(PEcAnRTM)
dyn.load("src/prosail.so")

pars <- c(1.5, 40, 8, 0, 0.01, 0.009)	  ## Default parameters

## Setup fortran
r <- matrix(0,nrow=2101,ncol=2)
p <- c(list("PROSPECT_5B"), as.list(pars), list(r))
p.f <- function(){
	do.call(.Fortran, p)
}

## Setup C
pdat <- prospect.datamatrix("prospect5b")
p.c <- function(){
	prospect5b_cpp(pars,pdat,1)
}

mb <- microbenchmark(p.f, p.c, times=1000)
autoplot(mb)
