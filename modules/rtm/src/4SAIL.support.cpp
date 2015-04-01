#include <Rcpp.h>
using namespace Rcpp;

//4SAIL model support functions

double min(double a, double b){
    if(a < b) {
        return a;
    } else {
        return b;
    }
}

// J1 function with avoidance of singularity problem
double Jfunc1(double k, double l, double t){
    double del = (k-l)*t;
    if (abs(del) > 1e-3){ 
        return (exp(-l*t)-exp(-k*t))/(k-l);
    } else {
        return 0.5*t*(exp(-k*t)+exp(-l*t))*(1.-del*del/12.);
    }
}

// J2 function
double Jfunc2(double k, double l, double t){
    return (1.-exp(-(k+l)*t))/(k+l);
}

// J3 function for treating (near) conservative scattering
double Jfunc3(double m, double t){
    double del, e;
    del=m*t;
    if (del > 1e-3){
        e=exp(-del);
        return (1-e)/(m*(1+e));
    } else {
        return 0.5*t*(1.-del*del/12.);
    }
}

