#include <stdio.h>
#include "common_funcs.h"
#include <Rmath.h>
#include <Rcpp.h>
using namespace Rcpp;

// Compute random_effects matrix
// Calculates individual spectrum for each leaf and returns spec matrix
NumericMatrix RE_model(
        NumericVector (*Model)(NumericVector, NumericMatrix),
        NumericVector param,
        NumericMatrix re,
        NumericMatrix p4data)
{
    int wl = p4data.nrow();
    int nre = re.ncol();
    NumericMatrix re_model(wl, nre);
    NumericVector pass_par(param.size());
    for(int i=0; i<nre; i++){
        re_model(_,i) = Model(param + re(_,i), p4data);
    }
    return re_model;
}

// [[Rcpp::export]]
NumericMatrix invert_RTM_re(
        std::string RTM,            // Name of model to invert
        NumericMatrix Observed,     // Matrix of observations
        int ngibbs,                 // Number of MCMC steps
        float adapt,                // How often to adapt Jump SD
        float adj_min,              // Minimum value by which to adapt Jump
        NumericVector values,       // Vector of parameter mean initial conditions
        NumericMatrix re_values,    // Matrix of random effects initial conditions (par x re)
        NumericMatrix func_data)    // Input data for the RTM (e.g. absorption features)
{
    int nspec = Observed.ncol();
    int nre = re_values.ncol();
    int nwl = Observed.nrow();
    int npars = values.size();
    NumericMatrix results(ngibbs, (npars+1)*nre);   // Set up results matrix
    printf("Results matrix is %d rows by %d columns \n", results.nrow(), results.ncol());

    // Pick RTM based on specified string
    select_model Model = MODEL(RTM);
    select_prior Prior = PRIOR(RTM);
    NumericVector pmin = PMIN(RTM);

    // Set up residual calculations
    double rp1, rp2, rinv, rsd;
    rp1 = 0.001 + nspec*nwl/2;        // Gamma shape; this is a constant
    rsd = 0.5;                      // Initial condition for residual SD
    double alpha_rp1;
    alpha_rp1 = 0.001 * nre / 2;
    NumericVector alpha_rp2(nre), tau(nre), tinv(nre);

    // Precalculate first model
    NumericVector PrevSpec = RE_model(Model, values, re_values, func_data);
    NumericMatrix PrevError = SpecError(PrevSpec, Observed);

    // Initialize Jump distribution
    NumericVector Jump = values * 0.05;  // Jump distribution vector - starts at 5% of initial conditions
    NumericMatrix alpha_Jump = re_values * 0.05;    // Jump matrix for random effects

    // Define sampler parameters
    double Tpar, JN, JD, a;
    NumericVector Tvec(npars);
    NumericVector Talpha(nre);
    NumericMatrix TrySpec(nwl, nre);
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
            Tvec = clone(values);
            Tvec[p] = rtnorm(values[p], Jump[p], pmin[p]);
            TrySpec = RE_model(Model, Tvec, re_values, func_data);
            TryError = SpecError(TrySpec, Observed);
            TryPost = Likelihood(TryError, rsd) + Prior(p, Tvec[p]);
            PrevPost = Likelihood(PrevError, rsd) + Prior(p, values[p]);
            JN = dtnorm(Tvec[p], values[p], Jump[p], pmin[p]);
            JD = dtnorm(values[p], Tvec[p], Jump[p], pmin[p]);
            a = exp((TryPost - JN) - (PrevPost - JD));
            printf("Tpar: %g  values: %g  Jump: %g \n TL: %g  PL: %g \n",
                Tvec[p],
                values[p],
                Jump[p],
                Likelihood(TryError, rsd),
                Likelihood(PrevError, rsd));
            if(a > runif(1)[0]){
                values[p] = Tvec[p];
                PrevError = TryError;
                ar[p] = ar[p] + 1;
            }
            results(ng, p) = values[p];

            // Sample model random effects
            for(int r = 0; r<nre; r++){
                // Sample
                Talpha = rtnorm_c(re_values(t,r), alpha_Jump(p), pmin(p)-values(p));
                TrySpec_alpha = Model(#VECTOR#, );

            }
        }

        // Sample residual SD
        rp2 = 0.001 + sum(PrevError * PrevError)/2;
        rinv = rgamma(1, rp1, 1/rp2)[0];
        rsd = 1/sqrt(rinv);
        printf(" RSD: %f \n =======================\n", rsd);
        results(ng, npars) = rsd;
        adapt_count = adapt_count + 1;
    }

    printf("\n MCMC completed! \n");
    return results;
}
