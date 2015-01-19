// TITLE: Prospect4 Model
//
// DESCRIPTION: Estimates leaf hemispherical-directional reflectance given 
// effective number of leaf layers (N), chlorophyll content (Cab), water
// content (Cw), and dry matter (Cm). Returns vector of reflectance from 
// 400 to 2500 nm.
//
// AUTHOR: Alexey Shiklomanov
//

#include <Rcpp.h>
using namespace Rcpp;

// NAME: exp_int
// TITLE: Numerical approximation of exponential integral
// DESCRIPTION: Integral from k to infinity of exp(-x)/x.
// 
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

// NAME: tavf
// TITLE: Transmittance of elementary layer
//
double tavf(float alpha2, double n)
{
  // Variable declarations
  double alpha = alpha2 * PI/180.0;
  
  double np, nm, a, k, sa;  
  double b1, b2, b;
  double ts, tp1, tp2, tp3, tp4, tp5, tp;
  double out;
  
  // Process
  sa = sin(alpha);
  np = n*n + 1;
  nm = n*n - 1;
  a = (n + 1)*(n+1) / 2;
  k = (-((n*n - 1)*(n*n - 1))) / 4;
    
  if(alpha < 1e-10)
  {
    out = 4 * n / ((n+1)*(n+1));
    return out;
  }
  
  else if(alpha == PI/2)
  {
    b1 = 0;
  }
  
  else
  {
    b1 = sqrt((sa*sa - np/2) * (sa*sa - np/2) + k);
  }
  
  b2 = sa*sa - np/2;
  b = b1 - b2;
  
  ts = (k*k/(6.0*b*b*b) + k/b - b/2.0) - (k*k/(6.0*a*a*a) + k/a - a/2.0);
  tp1 = -2.0*n*n * (b - a) / (np*np);
  tp2 = -2.0*n*n * np * log(b/a) / (nm*nm);
  tp3 = n*n * (1.0/b - 1.0/a) / 2.0;
  tp4 = 16.0*n*n*n*n * (n*n*n*n + 1.0) * log((2.0*np*b - nm*nm)/(2.0*np*a - nm*nm)) / (np*np*np * nm*nm);
  tp5 = 16.0*n*n*n*n*n*n * (1.0/(2.0*np*b - nm*nm) - 1.0/(2.0*np*a - nm*nm)) / (np*np*np);
  tp = tp1 + tp2 + tp3 + tp4 + tp5;
    
  out = (ts + tp) / (2.0*sa*sa);  

  return out;
}

// NAME: gpm
// TITLE: Generalized plate model
// DESCRIPTION: Returns reflectance of a leaf with "N" layers.
// [[Rcpp::export]]
double gpm(float alpha, float n, float theta, float N)
{
  double t90, tav, x, y;
  double tao1, tao2, rho1, rho2, rhoa, taoa, rho90, tao90;
  double d90, a90, b90, nmR, nmT, dmRT;
  double RNa;
  
  
  
  t90 = tavf(90.0, n);
  tav = tavf(alpha, n);
  
  // "x" and "y" simplifications from original PROSPECT model (Jacquemoud & Baret 1990) 
  x = tav / t90;
  y = x * (t90 - 1.0) + 1.0 - tav;
  
  // Reflectance and transmittance of first layer (N=1)
  tao1 = tav;
  tao2 = t90 / (n*n);
  rho1 = 1 - tao1;
  rho2 = 1 - tao2;
  
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

// NAME: prospect4
// TITLE: PROSPECT4 model
// DESCRIPTION: Returns full reflectance spectrum of a leaf given its biophysical parameters
//[[Rcpp::export]]
NumericVector prospect4(float N, float Cab, float Cw, float Cm,
  NumericVector n, NumericVector Cab_abs, 
  NumericVector Cw_abs, NumericVector Cm_abs
  ){
    
    int wl = n.size();
    NumericVector k(wl);
    NumericVector theta(wl);
    NumericVector Refl(wl);
    float ALPHA = 40.0;
    
    for(int i=0; i<wl; i++){
      k[i] = (1.0/N) * (Cab * Cab_abs[i] + Cw * Cw_abs[i] + Cm * Cm_abs[i]);
      theta[i] = exp_int(k[i]);
      Refl[i] = gpm(ALPHA, n[i], theta[i], N);
    }
    
    return Refl;
  }
      
    
