// Define common functions for later use
#include <Rcpp.h>
using namespace Rcpp;

double exp_int(double k);

NumericVector gpm(
        double N,
        NumericVector theta,
        NumericVector tao1,
        NumericVector tao2,
        NumericVector rho1,
        NumericVector rho2,
        NumericVector x,
        NumericVector y,
        int return_refl);

NumericVector prospect4_model(NumericVector param, NumericMatrix p4data);

// Truncated normal distribution functions
double rtnorm(double mu, double sd, double MIN);
double dtnorm(double X, double mu, double sd, double MIN);
double rtnorm_c(double mu, double sd, double MIN);
double dtnorm_c(double X, double mu, double sd, double MIN);

// Priors
double prospect4_priors(int param, double value);
