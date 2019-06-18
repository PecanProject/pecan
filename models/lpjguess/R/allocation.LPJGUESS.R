#' @importFrom Rcpp sourceCpp
#' @export

# compile the LPJ-GUESS allocation function using Rcpp
sourceCpp("~/Projects/PalEON/LPJ-GUESS/allocation.cpp")