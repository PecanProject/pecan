#include <Rcpp.h>
using namespace Rcpp;

NumericVector ladgen(double a, double b);
void volscatt(double tts, double tto, double psi, double ttl,
        double &chi_s, double &chi_o, double &frho, double &ftau);

void refl_trans( double rho, double tau, double LAI, 
        double ks, double ko, double sdb, double sdf, double dob,
        double dof, double sob, double sof, double ddb, double ddf,
        double &tau_ss, double &tau_oo, double &w, 
        double &tdd, double &rdd, 
        double &tsd, double &rsd, 
        double &tdo, double &rdo, 
        double &rsod
        );

double min(double a, double b);
double Jfunc1(double k, double l, double t);
double Jfunc2(double k, double l, double t);
double Jfunc3(double m, double t);

extern double rd;
