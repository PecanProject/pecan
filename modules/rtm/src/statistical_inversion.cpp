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

    int nspec = Observed.ncol();
    int nwl = Observed.nrow();
    int npars = inits.size();
    printf("Parameters: %d \n", npars);
    NumericMatrix results(ngibbs, npars);
    
	// Pick model based on specified string
    NumericMatrix(*Model)(NumericVector, NumericMatrix);
    double (*Prior)(int, double);
    NumericVector pmin(npars);
    if(RTM == "prospect4"){
        Model = prospect4_model;
        Prior = prospect4_priors;
        pmin = NumericVector::create(1, 0, 0, 0);
    }
    
//    else if(RTM == "prospect5"){
//        NumericVector (*Model)(NumericVector, NumericMatrix) = 
//            prospect5_model;
//        double (*Prior)(int, double) = prospect5_priors;
//        NumericVector pmin = NumericVector::create(1, 0, 0, 0, 0);
//    }

    double rp1 = 0.001 + nspec*nwl / 2, rp2, rinv, rsd = 0.5;

    // Precalculate first model
    NumericVector PrevSpec = prospect4_model(inits, func_data);
    NumericMatrix PrevError = SpecError(PrevSpec, Observed);

    NumericVector Jump = inits * 0.05;

    double Tpar, JN, JD, a;
    NumericVector Tvec = inits;
    NumericVector TrySpec(nwl);
    NumericMatrix TryError(nwl, nspec);
    double TryPost, PrevPost;
    NumericVector ar(npars), adj(npars);
    float adapt_count = 0;

    printf("Begin MCMC \n");
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
        for(int p = 0; p<npars; p++){
        	printf("n = %d p = %d \n", ng, p);
            Tvec = inits;
            Tvec[p] = rtnorm(inits[p], Jump[p], pmin[p]);
            TrySpec = Model(Tvec, func_data)(_,0);
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
        results(ng, npars) = rsd;

        adapt_count = adapt_count + 1;
    }
    printf("\n MCMC completed! \n");
    return results;
}

