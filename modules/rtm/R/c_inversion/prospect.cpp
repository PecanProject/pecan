// Quick and dirty implementation of the 'Sample alphaN' code block in C. I have taken the C++ code
// from prospect_c.cpp to get direct access to the prospect4 function. This required minor changes
// to the supporting functions to make them C compatible, and more substatial modification to the prospect4
// function to cut out the Rcpp package types.

#include <stdio.h>
#define MATHLIB_STANDALONE
#include <Rmath.h>
#include "exp_int.h"
#include "gpm.h"
#include "truncnorm.h"

static const int wl = 2101;
static const int nspec = 78;
double Cab_abs[wl], Cw_abs[wl], Cm_abs[wl],
       tao1[wl], tao2[wl], rho1[wl], rho2[wl],
       x[wl], y[wl];

// PROSPECT 4 model
void prospect4(double N, double Cab, double Cw, double Cm, double Refl[wl]){
    double k[wl], theta[wl];
    int i;

    for(i=0; i<wl; i++){
        k[i] = (1.0/N) * (Cab * Cab_abs[i] +
                Cw * Cw_abs[i] +
                Cm * Cm_abs[i]);
		theta[i] = exp_int(k[i]);
        Refl[i] = gpm(tao1[i], tao2[i], rho1[i], rho2[i],
                x[i], y[i], theta[i], N);
    }
    return ;
}

// SpecError
void SpecError(double Model[wl], Observed[wl][nspec], Error[wl][nspec]){
    int i,j;
    for(i=0; i<wl; i++){
        for(j=0; j<nspec; j++){
            Error[i][j] = Model[i] - Observed[i][j];
        }
    }
    return;
}

// Likelihood
double Likelihood(double Error[wl][nspec], double rsd){
    double LogL = 0.0;
    int i,j;
    for(i=0; i<wl; i++){
        for(j=0; j<nspec; j++){
            LogL += dnorm(Error[i][j], 0.0, rsd, 1);
        }
    }
    return LogL;
}

// Main inversion
int main(){
    int i,j;
    double Observed[wl][nspec];

    FILE *pdat;
    pdat = fopen("data_prospect.dat", "r");
    for(i=0; i<wl; i++){
        fscanf(pdat, "%f %g %f %f %f %f %f %f %f", 
                Cab_abs[i], Cw_abs[i], Cm_abs[i], 
                tao1[i], tao2[i], rho1[i], rho2[i],
                x[i], y[i]);
    }
    fclose(pdat);

    FILE *tobs;
    tobs = fopen("test_obs.dat", "r");
    for(i=0; i<wl; i++){
        for(j=0; j<nspec; j++){
            fscanf(tobs, "%f", Observed[i][j]);
        }
    }
    fclose(tobs);

    double N, Cab, Cw, Cm, rsd,
           TN, TCab, TCw, TCm;
    N = 1.4;
    Cab = 30;
    Cw = 0.01;
    Cm = 0.01;
    rsd = 0.5;

    double rp1 = 0.001 + nspec*wl/2.0, rp2, rinv;
    double Jump[4] = {0.1, 1, 0.005, 0.005};
    double TrySpec[wl], PrevSpec[wl];
    double TryError[wl][nspec], PrevError[wl][nspec];
    double TryPost, PrevPost;
    double JN, JD, a, u;

    prospect4(N, Cab, Cw, Cm, PrevSpec);
    SpecError(PrevSpec, Observed, PrevError);

    int ngibbs = 100, ng;
    for(ng=0; ng<ngibbs; ng++){
        TN = rtnorm(N, Jump[1], 1, 1e14);
        prospect4(TN, Cab, Cw, Cm, TrySpec);
        SpecError(TrySpec, Observed, TryError);
        TryPost = Likelihood(TryError, rsd);
        PrevPost = Likelihood(PrevError, rsd);
        JN = dtnorm(TN, N, Jump[1], 1, 1e14);
        JD = dtnorm(N, TN, Jump[1], 1, 1e14);
        a = exp(JN - JD);
        u = unif_rand();
        if(a > u){
            N = TN;
            for(i=0; i<wl; i++){
                for(j=0; j<nspec; j++){
                    PrevError[i][j] = SpecError[i][j];
                }
            }
        }

        rp2 = 0.001;
        for(i=0; i<wl;i++){
            for(j=0; j<nspec; j++){
                rp2 += PrevError[i][j] * PrevError[i][j];
            }
        }
        rp2 = rp2 / 2.0;
        rinv = rgamma(rp1, rp2);
        rsd = 1/sqrt(rinv);

        printf("%f   %f", N, rsd);
    }
}
    

