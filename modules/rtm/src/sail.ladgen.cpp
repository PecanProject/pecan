#include "sail.common.h"

double cum(double a, double b, double t){

//    double rd = M_PI/180;
    double eps, delx, x, p, y, dx;

    double rd = M_PI/180.0;
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

    for(int i = 1; i<9; i++){
        t = i*10;
        freq[i-1] = cum(a,b,t);
    }

    for(int i = 9; i<13; i++){
        t = 80 + (i - 8) * 2;
        freq[i-1] = cum(a,b,t);
    }

    freq[12] = 1;

    for(int i = 12; i > 1; i--){
        freq[i] = freq[i] - freq[i-1];
    }

    return freq;
}

