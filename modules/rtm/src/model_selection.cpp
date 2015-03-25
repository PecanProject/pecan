#include <stdio.h>
#include "common_funcs.h"
#include <Rmath.h>
#include <Rcpp.h>
using namespace Rcpp;

typedef NumericVector (*select_model)(NumericVector, NumericMatrix);
typedef double (*select_prior)(int, double);

// Select model
select_model MODEL(std::string RTM){
    if (RTM == "prospect4") {
        return prospect4_model;
    } // else {} <--- Other RTMs
}

// Select Prior
select_prior PRIOR(std::string RTM){
    if (RTM == "prospect4") {
        return prospect4_priors;
    } // else {} <--- Other RTMs
}

NumericVector PMIN(std::string RTM){
    if(RTM == "prospect4") {
        return NumericVector::create(1, 0, 0, 0);
    } // else {} <--- Other RTMs
}
