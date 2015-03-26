#include <stdio.h>
#include "common_funcs.h"
#include <Rmath.h>
#include <Rcpp.h>
using namespace Rcpp;

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
    NumericMatrix results(ngibbs, npars+1);   // Set up results matrix
    printf("Results matrix is %d rows by %d columns \n", results.nrow(), results.ncol());

    // Pick model based on specified string
    select_model Model = MODEL(RTM);
    select_prior Prior = PRIOR(RTM);
    NumericVector pmin = PMIN(RTM);

    double rp1, rp2, rinv, rsd;
    rp1 = 0.001 + nspec*nwl/2;        // Gamma shape; this is a constant
    rsd = 0.5;                      // Initial condition for residual SD

    // Precalculate first model
    NumericVector PrevSpec = Model(inits, func_data);
    NumericMatrix PrevError = SpecError(PrevSpec, Observed);

    NumericVector Jump = inits * 0.05;  // Jump distribution vector - starts at 5% of initial conditions

    // Define sampler parameters

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
        sampler_MH(inits, rsd, Jump, Observed, PrevError, ar, Model, Prior, pmin, func_data);
        for(int p = 0; p<npars; p++) results(ng, p) = inits[p];

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
