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
NumericMatrix pinvbayes(int ngibbs,
        NumericMatrix Observed,
        float adapt, float adj_min,
        NumericMatrix p4data) {

    printf("%f", log(10));
    int nspec = Observed.ncol();
    int wl = Observed.nrow();
    NumericMatrix results(ngibbs,5);

    // Initial conditions
    double N = 1.4;
    double Cab = 30;
    double Cw = 0.01;
    double Cm = 0.01;

    double rp1 = 0.001 + nspec * wl / 2;

    // Precalculate first model
    NumericVector PrevSpec = prospect4(N, Cab, Cw, Cm, p4data);
    NumericMatrix PrevError = SpecError(PrevSpec, Observed);

    NumericVector Jump = NumericVector::create(
            _["N"] = 0.01,
            _["Cab"] = 0.1,
            _["Cw"] = 1e-4,
            _["Cm"] = 5e-5);

    double rsd, rinv, rp2;
    double TN, TCab, TCw, TCm;
    double JN, JD, a;
    NumericVector TrySpec(wl);
    NumericMatrix TryError(wl, nspec);
    double TryPost, PrevPost;
    NumericVector ar = NumericVector::create(
            _["N"] = 0,
            _["Cab"] = 0,
            _["Cw"] = 0,
            _["Cm"] = 0);
    NumericVector adj(4);
    float adapt_count = 0;

    printf("Beginning MCMC loop \n");
    // Commented lines are for 'debug by print'
    for(int ng = 0; ng<ngibbs; ng++){
//        printf("%4d ", ng);
        // Adapt
        if(adapt - adapt_count < 1){
//            printf("\n %f %f %f %f \n", N, Cab, Cw, Cm);
//            printf("%.1f %.1f %.1f %.1f \n", ar[0], ar[1], ar[2], ar[3]);
            adj = ar / adapt / 0.75;
            adj = ifelse(adj < adj_min, adj_min, adj);
            Jump = Jump * adj;
//            printf("%g  %g  %g  %g \n \n", Jump[0], Jump[1], Jump[2], Jump[3]);
            ar = ar * 0;
            adapt_count = 0;
        }

        // Sample N
        TN = rtnorm(N, Jump["N"], 1, 1e10);
        TrySpec = prospect4(TN, Cab, Cw, Cm, p4data);
        TryError = SpecError(TrySpec, Observed);
        TryPost = Likelihood(TryError, rsd) + priorN(TN);
        PrevPost = Likelihood(PrevError, rsd) + priorN(N);
        JN = dtnorm(TN, N, Jump["N"], 1, 1e10);
        JD = dtnorm(N, TN, Jump["N"], 1, 1e10);
        a = exp((TryPost - JN) - (PrevPost - JD));
        if(a > runif(1)[0]){
            N = TN;
            PrevError = TryError;
            ar["N"] = ar["N"] + 1;
        }

        // Sample Cab
        TCab = rtnorm(Cab, Jump["Cab"], 0, 1e10);
        TrySpec = prospect4(N, TCab, Cw, Cm, p4data);
        TryError = SpecError(TrySpec, Observed);
        TryPost = Likelihood(TryError, rsd) + priorCab(TCab);
        PrevPost = Likelihood(PrevError, rsd) + priorCab(Cab);
        JN = dtnorm(TCab, Cab, Jump["Cab"], 0, 1e10);
        JD = dtnorm(Cab, TCab, Jump["Cab"], 0, 1e10);
        a = exp((TryPost - JN) - (PrevPost - JD));
        if(a > runif(1)[0]){
            Cab = TCab;
            PrevError = TryError;
            ar["Cab"] = ar["Cab"] + 1;
        }

        // Sample Cw
        TCw = rtnorm(Cw, Jump["Cw"], 0, 1e10);
        TrySpec = prospect4(N, Cab, TCw, Cm, p4data);
        TryError = SpecError(TrySpec, Observed);
        TryPost = Likelihood(TryError, rsd) + priorCw(TCw);
        PrevPost = Likelihood(PrevError, rsd) + priorCw(Cw);
        JN = dtnorm(TCw, Cw, Jump["Cw"], 0, 1e10);
        JD = dtnorm(Cw, TCw, Jump["Cw"], 0, 1e10);
        a = exp((TryPost - JN) - (PrevPost - JD));
        if(a > runif(1)[0]){
            Cw = TCw;
            PrevError = TryError;
            ar["Cw"] = ar["Cw"] + 1;
        }

        // Sample Cm
        TCm = rtnorm(Cm, Jump["Cm"], 0, 1e10);
        TrySpec = prospect4(N, Cab, Cw, TCm, p4data);
        TryError = SpecError(TrySpec, Observed);
        TryPost = Likelihood(TryError, rsd) + priorCm(TCm);
        PrevPost = Likelihood(PrevError, rsd) + priorCm(Cm);
        JN = dtnorm(TCm, Cm, Jump["Cm"], 0, 1e10);
        JD = dtnorm(Cm, TCm, Jump["Cm"], 0, 1e10);
        a = exp((TryPost - JN) - (PrevPost - JD));
        if(a > runif(1)[0]){
            Cm = TCm;
            PrevError = TryError;
            ar["Cm"] = ar["Cm"] + 1;
        }

        // Sample rsd
        rp2 = 0.001 + sum(PrevError * PrevError)/2;
        rinv = rgamma(1, rp1, 1/rp2)[0];
        rsd = 1/sqrt(rinv);

        results(ng,0) = N;
        results(ng,1) = Cab;
        results(ng,2) = Cw;
        results(ng,3) = Cm;
        results(ng,4) = rsd;

        adapt_count = adapt_count + 1;
    }
    printf("\n MCMC completed! \n");
   return results;
}


