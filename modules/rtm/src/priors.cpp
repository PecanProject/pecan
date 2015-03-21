#include <Rcpp.h>
#include <Rmath.h>
using namespace Rcpp;

// N is halfnormal, rest are lognormal
double priorN(double N){
    double p =  R::dnorm(N-1, 0.916, 2.2, 1) + log(2);
    return p;
}

double priorCab(double Cab){
    double p = R::dnorm(log(Cab), 3.4, 0.9, 1);
    return p;
}

double priorCw(double Cw){
    double p= R::dnorm(log(Cw), -6.377, 0.5, 1);
    return p;
}

double priorCm(double Cm){
    double p = R::dnorm(log(Cm), -5.116, 0.9, 1);
    return p;
}
