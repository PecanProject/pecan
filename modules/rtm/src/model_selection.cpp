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
        return prospect4_def;
    }
    else if (RTM == "prospect5") {
        return prospect5_def;
    }
    else if (RTM == "prospect5b"){
        return prospect5b_def;
    }
    // else {} <--- Other RTMs
}

// Select Prior
select_prior PRIOR(std::string RTM){
    if (RTM == "prospect4") {
        return prospect4_priors;
    }
    else if (RTM == "prospect5") {
        return prospect5_priors;
    }
    else if (RTM == "prospect5b"){
        return prospect5b_priors;
    }
    // else {} <--- Other RTMs
}

// Parameter constraints (minimum only)
NumericVector PMIN(std::string RTM){
    if(RTM == "prospect4") {
        return NumericVector::create(1, 0, 0, 0);
    } 
    else if (RTM == "prospect5") {
        return NumericVector::create(1, 0, 0, 0, 0);
    }
    else if (RTM == "prospect5b") {
        return NumericVector::create(1, 0, 0, 0, 0, 0);
    }
    // else {} <--- Other RTMs
}
