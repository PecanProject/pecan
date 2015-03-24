#include <stdio.h>
#include "common_funcs.h"
#include <Rmath.h>
#include <Rcpp.h>
using namespace Rcpp;

NumericMatrix RE_model(NumericVector N,
        NumericVector Cab,
        NumericVector Cw,
        NumericVector Cm,
        NumericMatrix p4data){
    int nspec = N.size(), wl = p4data.nrow();
    NumericMatrix model(wl, nspec);
    for(int i=0; i<nspec; i++){
        model(_,i) = prospect4_cpp(N[i], Cab[i], Cw[i], Cm[i], p4data, 1);
    }
    return model;
}

NumericMatrix SpecError_re(NumericMatrix Model, NumericMatrix Observed){
    int nspec = Observed.ncol();
    int wl = Observed.nrow();
    NumericMatrix E(wl, nspec);
    for (int i=0; i<nspec; i++) E(_,i) = Model(_,i) - Observed(_,i);
    return E;
}

double Likelihood_re(NumericMatrix Error, double rsd){
    return sum(dnorm(Error, 0, rsd, 1));
}

double Likelihood_v(NumericVector Error, double rsd){
    return sum(dnorm(Error, 0, rsd, 1));
}

// [[Rcpp::export]]
NumericMatrix pinvbayes_re(int ngibbs,
        NumericMatrix Observed,
        float adapt,
        float adj_min,
        NumericMatrix p4data) {

    int nspec = Observed.ncol();
    int wl = Observed.nrow();
    NumericMatrix results(ngibbs, 5*(nspec + 1));
    // Results format:
    // 		First 5 are N, Cab, Cw, Cm, rsd;
    // 		Then tau_N, tau_Cab, tau_Cw, tau_Cm
    // 		Then a_N[1], a_Cab[1], a_Cw[1], a_Cm[1], a_N[2] ...

    // Initial conditions - global mean
    double N = 1.4;
    double Cab = 30;
    double Cw = 0.01;
    double Cm = 0.01;

    NumericVector alpha_N(nspec, 0.0);
    NumericVector alpha_Cab(nspec, 0.0);
    NumericVector alpha_Cw(nspec, 0.0);
    NumericVector alpha_Cm(nspec, 0.0);

    NumericVector VN = N + alpha_N;
    NumericVector VCab = Cab + alpha_Cab;
    NumericVector VCw = Cw + alpha_Cw;
    NumericVector VCm = Cm + alpha_Cm;

    double rp1 = 0.001 + nspec * wl / 2;
    double alpha_rp1 = 0.001 + nspec / 2;

    // Precalculate first model
    NumericMatrix PrevSpec = RE_model(VN, VCab, VCw, VCm, p4data);
    NumericMatrix PrevError = SpecError_re(PrevSpec, Observed);

    NumericVector Jump = NumericVector::create(
            _["N"] = 0.01,
            _["Cab"] = 0.1,
            _["Cw"] = 1e-4,
            _["Cm"] = 5e-5);

    NumericVector alpha_Jump = NumericVector::create(
            _["N"] = 0.0005,
            _["Cab"] = 0.1,
            _["Cw"] = 1e-5,
            _["Cm"] = 5e-3);

    double rsd, rinv, rp2;
    double TN, TCab, TCw, TCm;
    NumericVector TVN(nspec), TVCab(nspec), TVCw(nspec), TVCm(nspec);
    double Talpha_N, Talpha_Cab, Talpha_Cw, Talpha_Cm;
    double alpha_rp2_N, alpha_rp2_Cab, alpha_rp2_Cw, alpha_rp2_Cm;
    double tau_N, tau_Cab, tau_Cw, tau_Cm;
    double tinv_N, tinv_Cab, tinv_Cw, tinv_Cm;
    double JN, JD, a;
    NumericMatrix TrySpec(wl, nspec);
    NumericMatrix TryError(wl, nspec);
    NumericVector TrySpec_alpha(wl), TryError_alpha(wl);
    double TryPost, PrevPost;
    NumericVector ar = NumericVector::create(
            _["N"] = 0,
            _["Cab"] = 0,
            _["Cw"] = 0,
            _["Cm"] = 0);

    NumericVector alpha_ar = NumericVector::create(
            _["N"] = 0,
            _["Cab"] = 0,
            _["Cw"] = 0,
            _["Cm"] = 0);
    NumericVector adj(4);
    NumericVector alpha_adj(4);
    float adapt_count = 0;

    printf("Beginning MCMC loop \n");
    // Commented lines are for 'debug by print'
    for(int ng = 0; ng<ngibbs; ng++){
        printf("%4d ", ng);
        // Adapt
        if(adapt - adapt_count < 1){
            printf("\n %f %f %f %f \n", N, Cab, Cw, Cm);
            printf("%.1f %.1f %.1f %.1f \n", ar[0], ar[1], ar[2], ar[3]);
            adj = ar / adapt / 0.75;
            adj = ifelse(adj < adj_min, adj_min, adj);
            Jump = Jump * adj;
            printf("%g  %g  %g  %g \n \n", Jump[0], Jump[1], Jump[2], Jump[3]);
            ar = ar * 0;

            printf("\n %f %f %f %f \n", alpha_ar[0], alpha_ar[1], alpha_ar[2], alpha_ar[3]);
            alpha_adj = alpha_ar / nspec / adapt / 0.75;
            alpha_adj = ifelse(alpha_adj < adj_min, adj_min, alpha_adj);
            alpha_Jump = alpha_Jump * alpha_adj;
            adapt_count = 0;
        }

        // Sample N global mean
        TN = rtnorm(N, Jump["N"], 1);
        TVN = TN + alpha_N;
        printf("\n %f %f %f %f %f", TVN[0], TVN[1], TVN[2], TVN[3], TVN[4]);
        TrySpec = RE_model(TVN, VCab, VCw, VCm, p4data);
        TryError = SpecError_re(TrySpec, Observed);
        TryPost = Likelihood_re(TryError, rsd) + priorN(TN);
        PrevPost = Likelihood_re(PrevError, rsd) + priorN(N);
        JN = dtnorm(TN, N, Jump["N"], 1);
        JD = dtnorm(N, TN, Jump["N"], 1);
        a = exp((TryPost - JN) - (PrevPost - JD));
        if(a > runif(1)[0]){
            N = TN;
            VN = TVN;
            PrevError = TryError;
            ar["N"] = ar["N"] + 1;
        }

        // Sample Cab global mean
        TCab = rtnorm(Cab, Jump["Cab"], 0);
        TVCab = TCab + alpha_Cab;
        TrySpec = RE_model(VN, TVCab, VCw, VCm, p4data);
        TryError = SpecError_re(TrySpec, Observed);
        TryPost = Likelihood_re(TryError, rsd) + priorCab(TCab);
        PrevPost = Likelihood_re(PrevError, rsd) + priorCab(Cab);
        JN = dtnorm(TCab, Cab, Jump["Cab"], 0);
        JD = dtnorm(Cab, TCab, Jump["Cab"], 0);
        a = exp((TryPost - JN) - (PrevPost - JD));
        if(a > runif(1)[0]){
            Cab = TCab;
            VCab = TVCab;
            PrevError = TryError;
            ar["Cab"] = ar["Cab"] + 1;
        }

        // Sample Cw global mean
        TCw = rtnorm(Cw, Jump["Cw"], 0);
        TVCw = TCw + alpha_Cw;
        TrySpec = RE_model(VN, VCab, TVCw, VCm, p4data);
        TryError = SpecError_re(TrySpec, Observed);
        TryPost = Likelihood_re(TryError, rsd) + priorCw(TCw);
        PrevPost = Likelihood_re(PrevError, rsd) + priorCw(Cw);
        JN = dtnorm(TCw, Cw, Jump["Cw"], 0);
        JD = dtnorm(Cw, TCw, Jump["Cw"], 0);
        a = exp((TryPost - JN) - (PrevPost - JD));
        if(a > runif(1)[0]){
            Cw = TCw;
            VCw = TVCw;
            PrevError = TryError;
            ar["Cw"] = ar["Cw"] + 1;
        }

        // Sample Cm global mean
        TCm = rtnorm(Cm, Jump["Cm"], 0);
        TVCm = TCm + alpha_Cm;
        TrySpec = RE_model(VN, VCab, VCw, TVCm, p4data);
        TryError = SpecError_re(TrySpec, Observed);
        TryPost = Likelihood_re(TryError, rsd) + priorCm(TCm);
        PrevPost = Likelihood_re(PrevError, rsd) + priorCm(Cm);
        JN = dtnorm(TCm, Cm, Jump["Cm"], 0);
        JD = dtnorm(Cm, TCm, Jump["Cm"], 0);
        a = exp((TryPost - JN) - (PrevPost - JD));
        if(a > runif(1)[0]){
            Cm = TCm;
            VCw = TVCw;
            PrevError = TryError;
            ar["Cm"] = ar["Cm"] + 1;
        }


        // Sample random effects
        for(int i=0; i<nspec; i++){
            // Alpha N
            Talpha_N = rtnorm_c(alpha_N[i], alpha_Jump["N"], 1-N);  // CAUTION: Mean != mu (0), because it's truncated
            printf("Try N: %g, mu: %g, sd: %g, min: %g ", Talpha_N, alpha_N[i], alpha_Jump[0], 1-N);
            TrySpec_alpha = prospect4_cpp(N + Talpha_N, VCab[i], VCw[i], VCm[i], p4data, 1);
            TryError_alpha = TrySpec_alpha - Observed(_,i);
            TryPost = Likelihood_v(TryError_alpha, rsd) + dtnorm_c(Talpha_N, 0, tau_N, 1-N);   // CAUTION: As above
            PrevPost = Likelihood_v(PrevError(_,i), rsd) + dtnorm_c(alpha_N[i], 0, tau_N, 1-N);
            JN = dtnorm_c(Talpha_N, alpha_N[i], alpha_Jump["N"], 1-N);
            JD = dtnorm_c(alpha_N[i], Talpha_N, alpha_Jump["N"], 1-N);
            a = exp((TryPost - JN) - (PrevPost - JD));
            if(a > runif(1)[0]){
                alpha_N[i] = Talpha_N;
                PrevError(_,i) = TryError_alpha;
                alpha_ar["N"] = alpha_ar["N"] + 1;
                printf("ACCEPTED");
            }
            printf("\n");

            // Alpha Cab
            Talpha_Cab = rtnorm_c(alpha_Cab[i], alpha_Jump["Cab"], -Cab);  // CAUTION: Mean != mu (0), because it's truncated
            TrySpec_alpha = prospect4_cpp(VN[i], Cab + Talpha_Cab, VCw[i], VCm[i], p4data, 1);
            TryError_alpha = TrySpec_alpha - Observed(_,i);
            TryPost = Likelihood_v(TryError_alpha, rsd) + dtnorm_c(Talpha_Cab, 0, tau_Cab, -Cab);   // CAUTION: As above
            PrevPost = Likelihood_v(PrevError(_,i), rsd) + dtnorm_c(alpha_Cab[i], 0, tau_Cab, -Cab);
            JN = dtnorm_c(Talpha_Cab, alpha_Cab[i], alpha_Jump["Cab"], -Cab);
            JD = dtnorm_c(alpha_Cab[i], Talpha_Cab, alpha_Jump["Cab"], -Cab);
            a = exp((TryPost - JN) - (PrevPost - JD));
            if(a > runif(1)[0]){
                alpha_Cab[i] = Talpha_Cab;
                PrevError(_,i) = TryError_alpha;
                alpha_ar["Cab"] = alpha_ar["Cab"] + 1;
            }

            // Alpha Cw
            Talpha_Cw = rtnorm_c(alpha_Cw[i], alpha_Jump["Cw"], -Cw);  // CAUTION: Mean != mu (0), because it's truncated
            TrySpec_alpha = prospect4_cpp(VN[i], VCab[i], Cw + Talpha_Cw, VCm[i], p4data, 1);
            TryError_alpha = TrySpec_alpha - Observed(_,i);
            TryPost = Likelihood_v(TryError_alpha, rsd) + dtnorm_c(Talpha_Cw, 0, tau_Cw, -Cw);   // CAUTION: As above
            PrevPost = Likelihood_v(PrevError(_,i), rsd) + dtnorm_c(alpha_Cw[i], 0, tau_Cw, -Cw);
            JN = dtnorm_c(Talpha_Cw, alpha_Cw[i], alpha_Jump["Cw"], -Cw);
            JD = dtnorm_c(alpha_Cw[i], Talpha_Cw, alpha_Jump["Cw"], -Cw);
            a = exp((TryPost - JN) - (PrevPost - JD));
            if(a > runif(1)[0]){
                alpha_Cw[i] = Talpha_Cw;
                PrevError(_,i) = TryError_alpha;
                alpha_ar["Cw"] = alpha_ar["Cw"] + 1;
            }

            // Alpha Cm
            Talpha_Cm = rtnorm_c(alpha_Cm[i], alpha_Jump["Cm"], -Cm);  // CAUTION: Mean != mu (0), because it's truncated
            TrySpec_alpha = prospect4_cpp(VN[i], VCab[i], VCw[i], Cm + Talpha_Cm, p4data, 1);
            TryError_alpha = TrySpec_alpha - Observed(_,i);
            TryPost = Likelihood_v(TryError_alpha, rsd) + dtnorm_c(Talpha_Cm, 0, tau_Cm, -Cm);   // CAUTION: As above
            PrevPost = Likelihood_v(PrevError(_,i), rsd) + dtnorm_c(alpha_Cm[i], 0, tau_Cm, -Cm);
            JN = dtnorm_c(Talpha_Cm, alpha_Cm[i], alpha_Jump["Cm"], -Cm);
            JD = dtnorm_c(alpha_Cm[i], Talpha_Cm, alpha_Jump["Cm"], -Cm);
            a = exp((TryPost - JN) - (PrevPost - JD));
            if(a > runif(1)[0]){
                alpha_Cm[i] = Talpha_Cm;
                PrevError(_,i) = TryError_alpha;
                alpha_ar["Cm"] = alpha_ar["Cm"] + 1;
            }
        }

        // Sample random effect tau
        alpha_rp2_N = 0.001 + sum(alpha_N * alpha_N) / 2;
        tinv_N = rgamma(1, alpha_rp1, 1/alpha_rp2_N)[0];
        tau_N = 1/sqrt(tinv_N);

        alpha_rp2_Cab = 0.001 + sum(alpha_Cab * alpha_Cab) / 2;
        tinv_Cab = rgamma(1, alpha_rp1, 1/alpha_rp2_Cab)[0];
        tau_Cab = 1/sqrt(tinv_Cab);

        alpha_rp2_Cw = 0.001 + sum(alpha_Cw * alpha_Cw) / 2;
        tinv_Cw = rgamma(1, alpha_rp1, 1/alpha_rp2_Cw)[0];
       	tau_Cw = 1/sqrt(tinv_Cw);

        alpha_rp2_Cm = 0.001 + sum(alpha_Cm * alpha_Cm) / 2;
        tinv_Cm = rgamma(1, alpha_rp1, 1/alpha_rp2_Cm)[0];
        tau_Cm = 1/sqrt(tinv_Cm);

        // Sample rsd
        rp2 = 0.001 + sum(PrevError * PrevError)/2;
        rinv = rgamma(1, rp1, 1/rp2)[0];
        rsd = 1/sqrt(rinv);

        results(ng,0) = N;
        results(ng,1) = Cab;
        results(ng,2) = Cw;
        results(ng,3) = Cm;
        results(ng,4) = rsd;
        results(ng, 5) = tau_N;
        results(ng, 6) = tau_Cab;
        results(ng, 7) = tau_Cw;
        results(ng, 8) = tau_Cm;
        for(int i=0; i<nspec; i++){
        	results(ng, 9 + 4*i) = alpha_N[i];
        	results(ng, 10 + 4*i) = alpha_Cab[i];
        	results(ng, 11 + 4*i) = alpha_Cw[i];
        	results(ng, 12 + 4*i) = alpha_Cm[i];
        }

        adapt_count = adapt_count + 1;
    }
    printf("\n MCMC completed! \n");
   return results;
}


