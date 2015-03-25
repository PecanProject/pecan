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
    int ng;
    NumericMatrix results(ngibbs, npars*(nre+2)+1);   // Set up results matrix
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
    NumericVector alpha_Jump = Jump * 0.1;    // Jump vector for random effects

    // Define sampler parameters
    double Tpar, Tmin, JN, JD, a;
    NumericVector Tvec(npars);
    NumericMatrix TrySpec(nwl, nre);
    NumericMatrix TryError(nwl, nspec);
    NumericVector TrySpec_alpha(nwl), TryError_alpha(nwl);
    double TryPost, PrevPost;

    // Define acceptance monitoring parameters
    NumericVector ar(npars), adj(npars);
    NumericVector alpha_ar(npars), alpha_adj(npars);
    float adapt_count = 0;

    // MCMC loop
    printf("Begin MCMC \n");
    for(ng = 0; ng<ngibbs; ng++){
        // Adapt
        if(adapt - adapt_count < 1){
            adj = ar / adapt / 0.75;
            adj = ifelse(adj < adj_min, adj_min, adj);
            Jump = Jump * adj;
            ar = ar * 0;

            alpha_adj = alpha_ar / nre / adapt / 0.75;
            alpha_adj = ifelse(alpha_adj < adj_min, adj_min, alpha_adj);
            alpha_Jump = alpha_Jump * alpha_adj;
            alpha_ar = alpha_ar * 0;
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
            if(a > runif(1)[0]){
                values[p] = Tvec[p];
                PrevError = TryError;
                ar[p] = ar[p] + 1;
            }
            results(ng, p*(nre+2)) = values[p];

            // Sample model random effects
            for(int r = 0; r<nre; r++){
                // Sample
                Tmin = pmin[p] - values[p];
                Tpar = rtnorm_c(re_values(p,r), alpha_Jump[p], Tmin);
                Tvec = values + re_values(_,r);
                Tvec[p] = values[p] + Tpar;
                TrySpec_alpha = Model(Tvec, func_data);
                TryError_alpha = TrySpec_alpha - Observed(_,r);
                TryPost = Likelihood(TryError_alpha, rsd) + dtnorm_c(Tpar, 0, tau[p], Tmin);
                PrevPost = Likelihood(PrevError(_,r), rsd) + dtnorm_c(re_values(p,r), 0, tau[p], Tmin);
                JN = dtnorm_c(Tpar, re_values(p,r), alpha_Jump[p], Tmin);
                JD = dtnorm_c(re_values(p,r), Tpar, alpha_Jump[p], Tmin);
                a = exp((TryPost - JN) - (PrevPost - JD));
                if(a > runif(1)[0]){
                    re_values(p,r) = Tpar;
                    PrevError(_,r) = TryError_alpha;
                    alpha_ar[p] = alpha_ar[p] + 1;
                }
                results(ng, p*(nre+2)+r+1) = re_values(p,r);
            }

            // Sample random effects SD
            alpha_rp2[p] = 0.001 + sum(re_values(p,_)*re_values(p,_))/2;
            tinv[p] = rgamma(1, alpha_rp1, 1/alpha_rp2[p])[0];
            tau[p] = 1/sqrt(tinv[p]);
            results(ng, (p+1)*(nre+2)-1) = tau[p];
            }
        
        // Sample residual SD
        rp2 = 0.001 + sum(PrevError * PrevError)/2;
        rinv = rgamma(1, rp1, 1/rp2)[0];
        rsd = 1/sqrt(rinv);
        results(ng, npars*(nre+2)) = rsd;
        adapt_count = adapt_count + 1;
    }

    printf("\n MCMC completed! \n");
    return results;
}
