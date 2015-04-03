#include "common_funcs.h"
#include <Rcpp.h>
using namespace Rcpp;

// NOTE: Overloaded function - Can take vector or matrix 'Model'
NumericMatrix SpecError(NumericVector Model, NumericMatrix Observed){
    int nspec = Observed.ncol();
    int wl = Observed.nrow();
    NumericMatrix E(wl, nspec);
    for (int i=0; i<nspec; i++) E(_,i) = Model - Observed(_,i);
    return E;
}

NumericMatrix SpecError(NumericMatrix Model, NumericMatrix Observed){
    int nspec = Observed.ncol();
    int wl = Observed.nrow();
    NumericMatrix E(wl, nspec);
    for (int i=0; i<nspec; i++) E(_,i) = Model(_,i) - Observed(_,i);
    return E;
}


// NOTE: Overloaded function - Can take matrix or vector as argument
double Likelihood(NumericMatrix Error, double rsd){
    return sum(dnorm(Error, 0, rsd, 1));
}

double Likelihood(NumericVector Error, double rsd){
    return sum(dnorm(Error, 0, rsd, 1));
}
