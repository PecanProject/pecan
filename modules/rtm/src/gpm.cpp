#include <Rcpp.h>
using namespace Rcpp;

// NAME: gpm
// TITLE: Generalized plate model
// DESCRIPTION: Returns reflectance of a leaf with "N" layers.
NumericVector gpm(
        double N,
        NumericVector theta,
        NumericVector tao1,
        NumericVector tao2,
        NumericVector rho1,
        NumericVector rho2,
        NumericVector x,
        NumericVector y,
        int return_refl)
{
    int wl = tao1.size();
    NumericVector rhoa, taoa, rho90, tao90;
    NumericVector d90, a90, b90, dmRT;
    NumericVector rho90s, tao90s, trdif;
    NumericVector b90p, b90pinv, b90dif;
    NumericVector out(wl);

    // Reflectance and transmittance of first layer (N=1)
    rhoa = rho1 + (tao1 * tao2 * rho2 * theta*theta) / (1 - rho2*rho2 * theta*theta);
    taoa = tao1 * tao2 * theta / (1 - rho2*rho2 * theta*theta);
    rho90 = (rhoa - y) / x;
    tao90 = taoa / x;
    rho90s = rho90*rho90;
    tao90s = tao90*tao90;
    trdif = tao90s - rho90s;

    // Reflectance and transmittance of N layers (Stokes coefficients)
    d90 = sqrt((trdif - 1.0)*(trdif - 1.0) - 4.0*rho90s);
    a90 = (1.0 - trdif + d90) / (2.0*rho90);
    b90 = (1.0 + trdif + d90) / (2.0*tao90);

    b90p = pow(b90, (N-1.0));
    b90pinv = pow(b90, (1.0-N));
    b90dif = b90p - b90pinv;

    dmRT = a90 * b90p - b90pinv / a90 - rho90 * b90dif;
    if(return_refl){
    	NumericVector nmR;
    	nmR = taoa * tao90 * b90dif;
    	out = rhoa + nmR / dmRT;
    	return out;
    } else {
    	NumericVector nmT;
    	nmT = taoa * (a90 - 1/a90);
    	out = nmT / dmRT;
    }
    return out;
}


