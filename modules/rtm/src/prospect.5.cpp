#include <stdio.h>
#include "common_funcs.h"
#include <Rcpp.h>
using namespace Rcpp;

// PROSPECT5 model
//      Based on 'refl_on', returns reflectance(1) or transmittance(0)
// [[Rcpp::export]]
NumericVector prospect5_cpp(
        NumericVector param,
        NumericMatrix p5data,
        int refl_on)
{
    int wl = p5data.nrow();
    double N = param[0], Cab = param[1], Car = param[2], Cw = param[3], Cm = param[4];
    NumericVector k(wl), theta(wl), Spec(wl);

    NumericMatrix::Column Cab_abs = p5data(_,0);
    NumericMatrix::Column Car_abs = p5data(_,1);
    NumericMatrix::Column Cw_abs = p5data(_,2);
    NumericMatrix::Column Cm_abs = p5data(_,3);
    NumericMatrix::Column tao1 = p5data(_,4);
    NumericMatrix::Column tao2 = p5data(_,5);
    NumericMatrix::Column rho1 = p5data(_,6);
    NumericMatrix::Column rho2 = p5data(_,7);
    NumericMatrix::Column x = p5data(_,8);
    NumericMatrix::Column y = p5data(_,9);

    k = (1.0/N) * (Cab * Cab_abs + 
            Car * Car_abs +
            Cw * Cw_abs + 
            Cm * Cm_abs);

    for(int i=0; i<wl; i++) theta[i] = exp_int(k[i]);

    Spec = gpm(N, theta, tao1, tao2, rho1, rho2, x, y, refl_on);
    return Spec;
}

// Shortcut function to get PROSPECT4 reflectance only (for inversion)
NumericVector prospect5_def(
    NumericVector param,
    NumericMatrix p5data)
{
        return prospect5_cpp(param, p5data, 1);
}
