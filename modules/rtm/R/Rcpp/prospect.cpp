// Quick and dirty implementation of the 'Sample alphaN' code block in C. I have taken the C++ code
// from prospect_c.cpp to get direct access to the prospect4 function. This required minor changes
// to the supporting functions to make them C compatible, and more substatial modification to the prospect4
// function to cut out the Rcpp package types.

#include <Rcpp.h>
using namespace Rcpp;
// Set up environments and constants
static Environment GE = Environment::global_env();
static int wl = GE["wl"];
static NumericVector Cab_abs = GE["Cab_abs"];
static NumericVector Cw_abs = GE["Cw_abs"];
static NumericVector Cm_abs = GE["Cm_abs"];
static NumericVector tao1 = GE["tao1"];
static NumericVector tao2 = GE["tao2"];
static NumericVector rho1 = GE["rho1"];
static NumericVector rho2 = GE["rho2"];
static NumericVector x = GE["x"];
static NumericVector y = GE["y"];

double exp_int(double k){
  double tau;
  double xx, yy;
  if(k <= 0.0){
    tau = 1;
  }
  else if (k > 0.0 and k <= 4.0){
    xx = 0.5 * k - 1.0;
    yy=(((((((((((((((-3.60311230482612224e-13
    *xx+3.46348526554087424e-12)*xx-2.99627399604128973e-11)
    *xx+2.57747807106988589e-10)*xx-2.09330568435488303e-9)
    *xx+1.59501329936987818e-8)*xx-1.13717900285428895e-7)
    *xx+7.55292885309152956e-7)*xx-4.64980751480619431e-6)
    *xx+2.63830365675408129e-5)*xx-1.37089870978830576e-4)
    *xx+6.47686503728103400e-4)*xx-2.76060141343627983e-3)
    *xx+1.05306034687449505e-2)*xx-3.57191348753631956e-2)
    *xx+1.07774527938978692e-1)*xx-2.96997075145080963e-1;
    yy=(yy*xx+8.64664716763387311e-1)*xx+7.42047691268006429e-1;
    yy=yy-log(k);
    tau=(1.0-k)*exp(-k)+k*k*yy;
  } else if (k > 4.0 and k <= 85.0){
    xx=14.5/(k+3.25)-1.0;
    yy=(((((((((((((((-1.62806570868460749e-12
    *xx-8.95400579318284288e-13)*xx-4.08352702838151578e-12)
    *xx-1.45132988248537498e-11)*xx-8.35086918940757852e-11)
    *xx-2.13638678953766289e-10)*xx-1.10302431467069770e-9)
    *xx-3.67128915633455484e-9)*xx-1.66980544304104726e-8)
    *xx-6.11774386401295125e-8)*xx-2.70306163610271497e-7)
    *xx-1.05565006992891261e-6)*xx-4.72090467203711484e-6)
    *xx-1.95076375089955937e-5)*xx-9.16450482931221453e-5)
    *xx-4.05892130452128677e-4)*xx-2.14213055000334718e-3;
    yy=((yy*xx-1.06374875116569657e-2)*xx-8.50699154984571871e-2)*xx+9.23755307807784058e-1;
    yy=exp(-k)*yy/k;
    tau=(1.0-k)*exp(-k)+k*k*yy;
  } else if (k > 85.0){
    tau=0;
  }
  return tau;
}

double gpm(double tao1,
        double tao2,
        double rho1,
        double rho2,
        double x,
        double y,
        double theta,
        double N)
{
    double rhoa, taoa, rho90, tao90;
    double d90, a90, b90, nmR, nmT, dmRT;
    double RNa;
    // Reflectance and transmittance of first layer (N=1)
    rhoa = rho1 + (tao1 * tao2 * rho2 * theta*theta) / (1 - rho2*rho2 * theta*theta);
    taoa = tao1 * tao2 * theta / (1 - rho2*rho2 * theta*theta);
    rho90 = (rhoa - y) / x;
    tao90 = taoa / x;
    // Reflectance and transmittance of N layers (Stokes coefficients)
    d90 = sqrt((tao90*tao90 - rho90*rho90 - 1.0)*(tao90*tao90 - rho90*rho90 - 1.0) - 4.0*rho90*rho90);
    a90 = (1.0 + rho90*rho90 - tao90*tao90 + d90) / (2.0*rho90);
    b90 = (1.0 - rho90*rho90 + tao90*tao90 + d90) / (2.0*tao90);
    nmR = taoa * tao90 * (pow(b90,(N-1.0)) - pow(b90,(1.0-N)));
    //nmT = taoa * (a90 - 1/a90);
    dmRT = a90*pow(b90, (N-1.0)) - pow(b90, (1.0-N))/a90 - rho90 * (pow(b90, (N-1.0)) - pow(b90,(1.0-N)));
    RNa = rhoa + nmR / dmRT;
    //TNa = nmT / dmRT;
    return RNa;
}

// PROSPECT 4 model
// [[Rcpp::export]]
NumericVector prospect4(float N, float Cab, float Cw, float Cm){
    NumericVector k(wl);
    NumericVector theta(wl);
    NumericVector Refl(wl);
    int i;

    for(i=0; i<wl; i++){
        k[i] = (1.0/N) * (Cab * Cab_abs[i] +
                Cw * Cw_abs[i] +
                Cm * Cm_abs[i]);
		theta[i] = exp_int(k[i]);
        Refl[i] = gpm(tao1[i], tao2[i], rho1[i], rho2[i],
                x[i], y[i], theta[i], N);
    }
    return Refl;
}
