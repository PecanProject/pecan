#include <Rcpp.h>
#include <Rmath.h>
using namespace Rcpp;

double rtnorm(double mu, double sd, double MIN, double MAX){
    double x = rnorm(1, mu, sd)[0];
    while(x < MIN or x > MAX) x = rnorm(1, mu, sd)[0];
    return x;
}

double dtnorm(double X, double mu, double sd, double MIN, double MAX){
    return R::dnorm(X, mu, sd, 1) -
        R::pnorm(MAX, mu, sd, 1, 1) +
        R::pnorm(MAX, mu, sd, 1 ,1);
}

