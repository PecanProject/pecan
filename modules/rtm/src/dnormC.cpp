#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector dnormC (NumericVector x, double mu, double sigma) {
  return dnorm(x, mu, sigma, true);
}

