#include <stdio.h>
#include "common_funcs.h"
#include <Rmath.h>
#include <Rcpp.h>
using namespace Rcpp;

NumericMatrix SpecError(NumericVector Model, NumericMatrix Observed){
    int nspec = Observed.ncol();
    int wl = Observed.nrow();
    NumericMatrix E(wl, nspec);
    for (int i=0; i<nspec; i++) E(_,i) = Model - Observed(_,i);
    return E;
}

double Likelihood(NumericMatrix Error, double rsd){
    return sum(dnorm(Error, 0, rsd, 1));
}

// [[Rcpp::export]]
NumericMatrix invert_RTM(
        std::string RTM,
        NumericMatrix Observed,
        int ngibbs,
        float adapt,
        float adj_min,
        NumericVector inits,
        NumericMatrix func_data){

    // Pick model based on specified string
    if(RTM == "prospect4"){
        NumericVector (*Model)(NumericVector,
                NumericMatrix, NumericMatrix) = prospect4_model;
        double (*Prior)(int, double) = prospect4_priors;
        NumericVector pmin = NumericVector::create(1, 0, 0, 0);
    } 
//    else if(RTM == "prospect5"){
//        NumericVector (*Model)(NumericVector, NumericMatrix) = 
//            prospect5_model;
//        double (*Prior)(int, double) = prospect5_priors;
//        NumericVector pmin = NumericVector::create(1, 0, 0, 0, 0);
//    }
        
    int nspec = Observed.ncol();
    int nwl = Observed.nrow();
    int npars = inits.size(), p;
    NumericMatrix results(ngibbs, npars);

    double rp1 = 0.001 + nspec*nwl / 2, rp2, rinv, rsd;

    // Precalculate first model
    NumericVector PrevSpec = prospect4_cpp(N, Cab, Cw, Cm, p4data, 1);
    NumericMatrix PrevError = SpecError(PrevSpec, Observed);

    NumericVector Jump = inits * 0.05;

    double Tpar, JN, JD, a;
    NumericVector TVec = inits, TrySpec(wl);
    NumericMatrix TryError(wl, nspec);
    double TryPost, PrevPost;
    NumericVector ar(npars), adj(npars);
    float adapt_count = 0;

    for(int ng = 0; ng<ngibbs; ng++){
        // Adapt
        if(adapt - adapt_count < 1){
            adj = ar / adapt / 0.75;
            adj = ifelse(adj < adj_min, adj_min, adj);
            Jump = Jump * adj;
            ar = ar * 0;
            adapt_count = 0;
        }

        // Sample model parameters
        for(p = 0; p<npars; p++){
            Tvec[p] = rtnorm(inits[p], Jump[p], pmin[p]);
            TrySpec = Model(Tvec);
            TryError = SpecError(TrySpec, Observed);
            TryPost = Likelihood(TryError, rsd) + Prior(p, Tvec[p]);
            PrevPost = Likelihood(PrevError, rsd) + Prior(p, inits[p]);
            JN = dtnorm(Tvec[p], inits[p], Jump[p], pmin[p]);
            JD = dtnorm(inits[p], Tvec[p], Jump[p], pmin[p]);
            a = exp((TryPost - JN) - (PrevPost - JD));
            if(a > runif(1)[0]){
                inits[p] = Tvec[p];
                PrevError = TryError;
                ar[p] = ar[p] + 1;
            }
            results(ng, p) = inits[p];
        }

        // Sample residual SD
        rp2 = 0.001 + sum(PrevError * PrevError)/2;
        rinv = rgamma(1, rp1, 1/rp2)[0];
        rsd = 1/sqrt(rinv);
        results(ng, npars + 1) = rsd;

        adapt_count = adapt_count + 1;
    }
    printf("\n MCMC completed! \n");
    return results;
}

