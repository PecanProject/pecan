#include <stdio.h>
#include "common_funcs.h"
#include <Rcpp.h>
using namespace Rcpp;

// PROSPECT4 model
//      Based on 'refl_on', returns reflectance(1) or transmittance(0)
// [[Rcpp::export]]
NumericVector prospect4_cpp(
        NumericVector param,
        NumericMatrix p4data,
        int refl_on)
{
    int wl = p4data.nrow();
    double N = param[0], Cab = param[1], Cw = param[2], Cm = param[3];
    NumericVector k(wl), theta(wl), Spec(wl);

    NumericMatrix::Column Cab_abs = p4data(_,0);
    NumericMatrix::Column Cw_abs = p4data(_,1);
    NumericMatrix::Column Cm_abs = p4data(_,2);
    NumericMatrix::Column tao1 = p4data(_,3);
    NumericMatrix::Column tao2 = p4data(_,4);
    NumericMatrix::Column rho1 = p4data(_,5);
    NumericMatrix::Column rho2 = p4data(_,6);
    NumericMatrix::Column x = p4data(_,7);
    NumericMatrix::Column y = p4data(_,8);

    k = (1.0/N) * (Cab * Cab_abs + Cw * Cw_abs + Cm * Cm_abs);

    for(int i=0; i<wl; i++) theta[i] = exp_int(k[i]);

    Spec = gpm(N, theta, tao1, tao2, rho1, rho2, x, y, refl_on);
    return Spec;
}

// Shortcut function to get PROSPECT4 reflectance only (for inversion)
NumericVector prospect4_def(
    NumericVector param,
    NumericMatrix p4data)
{
        return prospect4_cpp(param, p4data, 1);
}
