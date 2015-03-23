// Define common functions for later use
#include <Rcpp.h>
using namespace Rcpp;

double exp_int(double k);

NumericVector gpm(NumericVector tao1,
        NumericVector tao2,
        NumericVector rho1,
        NumericVector rho2,
        NumericVector x,
        NumericVector y,
        NumericVector theta,
        double N,
        int return_refl);

NumericVector prospect4_cpp(double N,
        double Cab,
        double Cw,
        double Cm,
        NumericMatrix p4data,
        int refl_on);

double rtnorm(double mu, double sd, double MIN);
double dtnorm(double X, double mu, double sd, double MIN);

// Priors
double priorN(double N);
double priorCab(double Cab);
double priorCw(double Cw);
double priorCm(double Cm);
