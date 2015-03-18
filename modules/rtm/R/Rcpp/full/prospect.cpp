// Quick and dirty implementation of the 'Sample alphaN' code block in C. I have taken the C++ code
// from prospect_c.cpp to get direct access to the prospect4 function. This required minor changes
// to the supporting functions to make them C compatible, and more substatial modification to the prospect4
// function to cut out the Rcpp package types.

#include <Rcpp.h>
using namespace Rcpp;
#include <Rmath.h>
#include "global_constants.h"
#include "exp_int.h"
#include "gpm.h"
#include "support.h"
#include "priors.h"

// PROSPECT 4 model
// [[Rcpp::export]]
NumericVector prospect4(float N, float Cab, float Cw, float Cm){
    NumericVector k(wl);
    NumericVector theta(wl);
    NumericVector Refl(wl);
    int i;
    
    for(i=0; i<wl; i++){
        k[i] = (1.0/N) * (Cab * Cab_abs[i] +
                Cw * Cw_abs[i] + 
                Cm * Cm_abs[i]);
		theta[i] = exp_int(k[i]);
        Refl[i] = gpm(tao1[i], tao2[i], rho1[i], rho2[i],
                x[i], y[i], theta[i], N);
    }
    return Refl;
}

// [[Rcpp::export]]
NumericVector pinvbayes(int ngibbs, NumericMatrix Observed){
		int nspec = Observed.ncol();

		// Precompute first model
		double N = 1.4;
		double Cab = 30;
		double Cw = 0.01;
		double Cm = 0.01;
		double rsd = 0.05;
		
		double logN = log(N);
		double logCab = log(Cab);
		double logCw = log(Cw);
		double logCm = log(Cm);
		
		double JumpN = 0.01*logN;
		double JumpCab = 0.01*logCab;
		double JumpCw = 0.01*logCw;
		double JumpCm = 0.01*logCm;
		NumericVector PrevSpec(wl);
		NumericMatrix PrevError(wl, nspec);
		PrevSpec = prospect4(N, Cab, Cw, Cm);
		PrevError = SpecError(PrevSpec, Observed, nspec);
		double rp1 = 0.001 + nspec*wl/2;
		
		double TlogN, TlogCab, TlogCw, TlogCm;
		double TN, TCab, TCw, TCm;
		double JN, JD, a, u;
		NumericVector TrySpec;
		NumericMatrix TryError;
		double TryPost;
		double PrevPost;
		double rp2, rinv;
		NumericMatrix Results(ngibbs, 5);
		
		for(int ng = 0; ng<ngibbs; ng++){
			printf("%d  ", ng);
			// Sample N
			TlogN = rnorm(1, logN, JumpN)[0];
			TN = exp(logN) + 1;
			TrySpec = prospect4(TN, Cab, Cw, Cm);
			TryError = SpecError(TrySpec, Observed, nspec);
			TryPost = Likelihood(TryError, rsd, nspec) + priorN(TlogN);
			PrevPost = Likelihood(PrevError, rsd, nspec) + priorN(logN);
			JN = dnorm_simp(TlogN, logN, JumpN);
			JD = dnorm_simp(logN, TlogN, JumpN);
			a = exp(JN - JD);
			printf(" %e ", a); 
			u = runif(1)[0];
			if(a > u){
				N = TN;
				logN = TlogN;
				PrevError = TryError;
				printf("N");
			}
			
			// Sample Cab
			TlogCab = rnorm(1, logCab, JumpCab)[0];
			TCab = exp(logCab);
			TrySpec = prospect4(N, TCab, Cw, Cm);
			TryError = SpecError(TrySpec, Observed, nspec);
			TryPost = Likelihood(TryError, rsd, nspec) + priorCab(TlogCab);
			PrevPost = Likelihood(PrevError, rsd, nspec) + priorCab(logCab);
			JN = dnorm_simp(TlogCab, logCab, JumpCab);
			JD = dnorm_simp(logCab, TlogCab, JumpCab);
			a = exp(JN - JD);
			printf(" %e ", a); 
			u = runif(1)[0];
			if(a > u){
				Cab = TCab;
				logCab = TlogCab;
				PrevError = TryError;
				printf("Cab");
			}
			
			// Sample Cw
			TlogCw = rnorm(1, logCw, JumpCw)[0];
			TCw = exp(logCw);
			TrySpec = prospect4(N, Cab, TCw, Cm);
			TryError = SpecError(TrySpec, Observed, nspec);
			TryPost = Likelihood(TryError, rsd, nspec) + priorCw(TlogCw);
			PrevPost = Likelihood(PrevError, rsd, nspec) + priorCw(logCw);
			JN = dnorm_simp(TlogCw, logCw, JumpCw);
			JD = dnorm_simp(logCw, TlogCw, JumpCw);
			a = exp(JN - JD);
			printf(" %e ", a); 
			u = runif(1)[0];
			if(a > u){
				Cw = TCw;
				logCw = TlogCw;
				PrevError = TryError;
				printf("Cw");
			}
			
			// Sample Cm
			TlogCm = rnorm(1, logCm, JumpCm)[0];
			TCm = exp(logCm);
			TrySpec = prospect4(N, Cab, Cw, TCm);
			TryError = SpecError(TrySpec, Observed, nspec);
			TryPost = Likelihood(TryError, rsd, nspec) + priorCm(TlogCm);
			PrevPost = Likelihood(PrevError, rsd, nspec) + priorCm(logCm);
			JN = dnorm_simp(TlogCm, logCm, JumpCm);
			JD = dnorm_simp(logCm, TlogCm, JumpCm);
			a = exp(JN - JD);
			printf(" %e ", a); 
			u = runif(1)[0];
			if(a > u){
				Cm = TCm;
				logCm = TlogCm;
				PrevError = TryError;
				printf("Cm");
			}
			
			// Sample rsd
			rp2 = 0.001 + sum(PrevError*PrevError)/2.0;
			rinv = rgamma(1, rp1, rp2)[0];
			rsd = 1.0/sqrt(rinv);
			
			Results(ng, 1) = N;
			Results(ng, 2) = Cab;
			Results(ng, 3) = Cw;
			Results(ng, 4) = Cm;
			Results(ng, 5) = rsd;
		}
		return Results;
}
