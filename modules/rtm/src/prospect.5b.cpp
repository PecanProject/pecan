#include <stdio.h>
#include "common_funcs.h"
#include <Rcpp.h>
using namespace Rcpp;

// PROSPECT5 model
//      Based on 'refl_on', returns reflectance(1) or transmittance(0)
// [[Rcpp::export]]
NumericVector prospect5b_cpp(
        NumericVector param,
        NumericMatrix p5bdata,
        int refl_on)
{
    int wl = p5bdata.nrow();
    double N = param[0], Cab = param[1], Car = param[2], 
           Cbrown = param[3], Cw = param[4], Cm = param[5];
    NumericVector k(wl), theta(wl), Spec(wl);

    NumericMatrix::Column Cab_abs = p5bdata(_,0);
    NumericMatrix::Column Car_abs = p5bdata(_,1);
    NumericMatrix::Column Cbrown_abs = p5bdata(_,2);
    NumericMatrix::Column Cw_abs = p5bdata(_,3);
    NumericMatrix::Column Cm_abs = p5bdata(_,4);
    NumericMatrix::Column tao1 = p5bdata(_,5);
    NumericMatrix::Column tao2 = p5bdata(_,6);
    NumericMatrix::Column rho1 = p5bdata(_,7);
    NumericMatrix::Column rho2 = p5bdata(_,8);
    NumericMatrix::Column x = p5bdata(_,9);
    NumericMatrix::Column y = p5bdata(_,10);

    k = (1.0/N) * (Cab * Cab_abs + 
            Car * Car_abs +
            Cw * Cw_abs + 
            Cm * Cm_abs);

    for(int i=0; i<wl; i++) theta[i] = exp_int(k[i]);

    Spec = gpm(N, theta, tao1, tao2, rho1, rho2, x, y, refl_on);
    return Spec;
}

// Shortcut function to get PROSPECT4 reflectance only (for inversion)
NumericVector prospect5b_def(
    NumericVector param,
    NumericMatrix p5bdata)
{
        return prospect5b_cpp(param, p5bdata, 1);
}
