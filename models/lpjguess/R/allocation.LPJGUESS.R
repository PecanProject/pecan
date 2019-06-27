##' @useDynLib PEcAn.LPJGUESS
##' @importFrom Rcpp sourceCpp
NULL

# compile the LPJ-GUESS allocation function using Rcpp
sourceCpp("src/allocation.LPJGUESS.cpp")