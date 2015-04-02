#include "sail_common.h"

double cum(double a, double b, double t){

//    double rd = M_PI/180;
    double eps, delx, x, p, y, dx;

    eps = 1e-6;
    delx = 1;

    x = 2*rd*t;
    p = x;

    while (delx > eps) {
        y = a*sin(x) + 0.5*b*sin(2.0*x);
        dx = 0.5*(y-x+p);
        x = x+dx;
        delx = abs(dx);
    }

    return (2 * y + p) / M_PI;
}

NumericVector ladgen(double a, double b){
    NumericVector freq(13);
    double t;

    for(int i = 0; i<8; i++){
        t = i*10;
        freq[i] = cum(a,b,t);
    }

    for(int i = 8; i<12; i++){
        t = 80 + (i - 8) * 2;
        freq[i] = cum(a,b,t);
    }

    freq[12] = 1;

    for(int i = 12; i > 2; i--){
        freq[i] = freq[i] - freq[i-1];
    }

    return freq;
}

