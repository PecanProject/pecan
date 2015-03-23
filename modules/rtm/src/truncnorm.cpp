#include <Rcpp.h>
#include <Rmath.h>
using namespace Rcpp;

double rtnorm(double mu, double sd, double MIN){
    double x = rnorm(1, mu, sd)[0];
    if(x < MIN){
        x = R::qnorm(runif(1, R::pnorm(MIN, mu, sd, 1, 0), 1)[0], mu, sd, 1, 0);
    }
    return x;
}

double dtnorm(double X, double mu, double sd, double MIN){
    if(X < MIN){
        return -1e15;
    }
    else {
        return R::dnorm(X, mu, sd, 1) - log(1 - R::pnorm(MIN, mu, sd, 1, 0));
    }
}

