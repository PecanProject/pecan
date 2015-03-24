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
        std::string RTM,            // Name of model to invert
        NumericMatrix Observed,     // Matrix of observations
        int ngibbs,                 // Number of MCMC steps
        float adapt,                // How often to adapt Jump SD
        float adj_min,              // Minimum value by which to adapt Jump
        NumericVector inits,        // Vector of initial conditions
        NumericMatrix func_data)    // Input data for the RTM (e.g. absorption features)
{
    int nspec = Observed.ncol();
    int nwl = Observed.nrow();
    int npars = inits.size();
    NumericMatrix results(ngibbs, npars);   // Set up results matrix

    // Pick model based on specified string
    NumericVector(*Model_select)(NumericVector, NumericMatrix);
    double (*Prior)(int, double);           // 'int' is the indicator, 'double' is the value
    NumericVector pmin(npars);              // Vector of parameter constraints (minima)
    if(RTM == "prospect4"){
        Model_select = prospect4_model;
        Prior = prospect4_priors;
        pmin = NumericVector::create(1, 0, 0, 0);
    } // else {} <--- other RTMs go here 

    NumericVector Model(NumericVector param){   // Redefine model with input data
        return Model_select(param, func_data);
    }

    double rp1, rp2, rinv, rsd;
    rp1 = 0.001 + nspec*nwl;        // Gamma shape; this is a constant
    rsd = 0.5;                      // Initial condition for residual SD
    
    // Precalculate first model
    NumericVector PrevSpec = prospect4_model(inits, func_data);
    NumericMatrix PrevError = SpecError(PrevSpec, Observed);

    NumericVector Jump = inits * 0.05;  // Jump distribution vector - starts at 5% of initial conditions

    // Define sampler parameters
    double Tpar, JN, JD, a;              
    NumericVector Tvec = inits;
    NumericVector TrySpec(nwl);
    NumericMatrix TryError(nwl, nspec);
    double TryPost, PrevPost;

    // Define acceptance monitoring parameters
    NumericVector ar(npars), adj(npars);
    float adapt_count = 0;

    // MCMC loop
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
        // Sample model parameters - Basic Metropolis Hastings
        for(int p = 0; p<npars; p++){
            printf("n = %d p = %d \n", ng, p);
            Tvec = inits;
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
        results(ng, npars) = rsd;
        adapt_count = adapt_count + 1;
    }

    printf("\n MCMC completed! \n");
    return results;
}
