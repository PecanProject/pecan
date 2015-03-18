// Quick and dirty implementation of the 'Sample alphaN' code block in C. I have taken the C++ code
// from prospect_c.cpp to get direct access to the prospect4 function. This required minor changes
// to the supporting functions to make them C compatible, and more substatial modification to the prospect4
// function to cut out the Rcpp package types.
#include <Rmath.h>
#include <R.h>
#include <Rdefines.h>
#include <gsl.h>
// NAME: tavf
// TITLE: Transmittance of elementary layer
//
double tavf(float alpha2, double n)
{
    // Variable declarations
    double alpha = alpha2 * PI/180.0;
    double np, nm, a, k, sa;
    double b1, b2, b;
    double ts, tp1, tp2, tp3, tp4, tp5, tp;
    double out;
    // Process
    sa = sin(alpha);
    np = n*n + 1;
    nm = n*n - 1;
    a = (n + 1)*(n+1) / 2;
    k = (-((n*n - 1)*(n*n - 1))) / 4;
    if(alpha < 1e-10)
    {
        out = 4 * n / ((n+1)*(n+1));
        return out;
    }
    else if(alpha == PI/2)
    {
        b1 = 0;
    }
    else
    {
        b1 = sqrt((sa*sa - np/2) * (sa*sa - np/2) + k);
    }
    b2 = sa*sa - np/2;
    b = b1 - b2;
    ts = (k*k/(6.0*b*b*b) + k/b - b/2.0) - (k*k/(6.0*a*a*a) + k/a - a/2.0);
    tp1 = -2.0*n*n * (b - a) / (np*np);
    tp2 = -2.0*n*n * np * log(b/a) / (nm*nm);
    tp3 = n*n * (1.0/b - 1.0/a) / 2.0;
    tp4 = 16.0*n*n*n*n * (n*n*n*n + 1.0) * log((2.0*np*b - nm*nm)/(2.0*np*a - nm*nm)) / (np*np*np * nm*nm);
    tp5 = 16.0*n*n*n*n*n*n * (1.0/(2.0*np*b - nm*nm) - 1.0/(2.0*np*a - nm*nm)) / (np*np*np);
    tp = tp1 + tp2 + tp3 + tp4 + tp5;
    out = (ts + tp) / (2.0*sa*sa);
    return out;
}
// NAME: gpm
// TITLE: Generalized plate model
// DESCRIPTION: Returns reflectance of a leaf with "N" layers.
// [[Rcpp::export]]
double gpm(float alpha, float n, float theta, float N)
{
    double t90, tav, x, y;
    double tao1, tao2, rho1, rho2, rhoa, taoa, rho90, tao90;
    double d90, a90, b90, nmR, nmT, dmRT;
    double RNa;
    t90 = tavf(90.0, n);
    tav = tavf(alpha, n);
    // "x" and "y" simplifications from original PROSPECT model (Jacquemoud & Baret 1990)
    x = tav / t90;
    y = x * (t90 - 1.0) + 1.0 - tav;
    // Reflectance and transmittance of first layer (N=1)
    tao1 = tav;
    tao2 = t90 / (n*n);
    rho1 = 1 - tao1;
    rho2 = 1 - tao2;
    rhoa = rho1 + (tao1 * tao2 * rho2 * theta*theta) / (1 - rho2*rho2 * theta*theta);
    taoa = tao1 * tao2 * theta / (1 - rho2*rho2 * theta*theta);
    rho90 = (rhoa - y) / x;
    tao90 = taoa / x;
    // Reflectance and transmittance of N layers (Stokes coefficients)
    d90 = sqrt((tao90*tao90 - rho90*rho90 - 1.0)*(tao90*tao90 - rho90*rho90 - 1.0) - 4.0*rho90*rho90);
    a90 = (1.0 + rho90*rho90 - tao90*tao90 + d90) / (2.0*rho90);
    b90 = (1.0 - rho90*rho90 + tao90*tao90 + d90) / (2.0*tao90);
    nmR = taoa * tao90 * (pow(b90,(N-1.0)) - pow(b90,(1.0-N)));
    //nmT = taoa * (a90 - 1/a90);
    dmRT = a90*pow(b90, (N-1.0)) - pow(b90, (1.0-N))/a90 - rho90 * (pow(b90, (N-1.0)) - pow(b90,(1.0-N)));
    RNa = rhoa + nmR / dmRT;
    //TNa = nmT / dmRT;
    return RNa;
}
// C-version of the C++ function prospect4, which was written for use with the Rcpp package
void prospect4c( float N,
        float Cab,
        float Cw,
        float Cm,
        double* n,
        double* Cab_abs,
        double* Cw_abs,
        double* Cm_abs,
        int nwl,
        double* Refl ) {
    double k[nwl];
    double theta[nwl];
    float ALPHA = 40.0;
    int i;
    for(i=0; i<nwl; i++){
        k[i] = (1.0/N) * (Cab * Cab_abs[i] + Cw * Cw_abs[i] + Cm * Cm_abs[i]);
        theta[i] = exp_int(k[i]);
        Refl[i] = gpm(ALPHA, n[i], theta[i], N);
    }
    return;
}
// C-version of the Sample alphaN code block in inv_bayes.R
SEXP sample_alpha_n(
        SEXP nre_int, SEXP nwl_int, SEXP nspec_int,
        SEXP alphaNi_dblv, SEXP alphaCabi_dblv, SEXP alphaCwi_dblv, SEXP alphaCmi_dblv, SEXP jumpSdAlphaN_dbl,
        SEXP Ni_dbl, SEXP Cabi_dbl, SEXP Cwi_dbl, SEXP Cmi_dbl,
        SEXP Na_dblv, SEXP Caba_dblv, SEXP Cwa_dblv, SEXP Cma_dblv,
        SEXP prevError_dblm, SEXP randeff_intv, SEXP obsSpec_dblm,
        SEXP sdi_dbl, SEXP sdreN_dbl, SEXP prevErrorLike_dbl, SEXP arAlpha_int ) {
    // Cast R objects as C types
    /// note: pointer vars can modify inputs, value vars are copied and cannot
    int nre = INTEGER_VALUE(nre_int); // in, scalar
    int nwl = INTEGER_VALUE(nwl_int); // in, scalar
    int nspec = INTEGER_VALUE(nspec_int); // in, scalar
    double *alphaNi = NUMERIC_POINTER(alphaNi_dblv); // inout, dim=[nre]
    double *alphaCabi = NUMERIC_POINTER(alphaCabi_dblv); // in, dim=[nre]
    double *alphaCwi = NUMERIC_POINTER(alphaCwi_dblv); // in, dim=[nre]
    double *alphaCmi = NUMERIC_POINTER(alphaCmi_dblv); // in, dim=[nre]
    double jumpSdAlphaN = NUMERIC_VALUE(jumpSdAlphaN_dbl); // in, scalar
    double Ni = NUMERIC_VALUE(Ni_dbl); // in, scalar
    double Cabi = NUMERIC_VALUE(Cabi_dbl); // in, scalar
    double Cwi = NUMERIC_VALUE(Cwi_dbl); // in, scalar
    double Cmi = NUMERIC_VALUE(Cmi_dbl); // in, scalar
    double *Na = NUMERIC_POINTER(Na_dblv); // in, dim=[nwl]
    double *Caba = NUMERIC_POINTER(Caba_dblv); // in, dim=[nwl]
    double *Cwa = NUMERIC_POINTER(Cwa_dblv); // in, dim=[nwl]
    double *Cma = NUMERIC_POINTER(Cma_dblv); // in, dim=[nwl]
    double *prevError = NUMERIC_POINTER(prevError_dblm); // in out, dim=[nwl][nspec]
    int *randeff = INTEGER_POINTER(randeff_intv); // in, dim=[nre]
    double *obsSpec = NUMERIC_POINTER(obsSpec_dblm); // in dim=[nwl][nspec]
    double sdi = NUMERIC_VALUE(sdi_dbl); // in, scalar
    double sdreN = NUMERIC_VALUE(sdreN_dbl); // in, scalar
    double *prevErrorLike = NUMERIC_POINTER(prevErrorLike_dbl); // inout, scalar
    double *arAlpha = NUMERIC_POINTER(arAlpha_int); // inout, scalar
    // Declare local variables
    int i, j, k;
    double c, y, t, a;
    double guessAlphaN;
    double guessSpec[nwl];
    double guessError[nwl*nspec]; // 2d array treated as a 1d array
    double guessErrorLike;
    double guessPosterior;
    double prevPosterior;
    for (i=0; i<nre; i++)
    {
        // make random step
        GetRNGstate();
        guessAlphaN = rnorm(alphaNi[i], jumpSdAlphaN);
        PutRNGstate();
        // run prospect model
        prospect4c(Ni+guessAlphaN, Cabi+alphaCabi[i], Cwi+alphaCwi[i], Cmi+alphaCmi[i],
                Na, Caba, Cwa, Cma, nwl,
                guessSpec);
        // copy prevError to guessError
        for (int j=0; j<nwl*nspec; j++) {
            guessError[j] = prevError[j];
        }
        // add random effects
        for (int q=randeff[i]; q<randeff[i+1]; q++) { // sequential integers corresponding to an element of randeff.list
            for(int p=0; p<nwl; p++) { // reset values in selected columns
                guessError[p+q*nwl] = guessSpec[p]-obsSpec[p+q*nwl];
            }
        }
        // compute likelihood using Kahan summation algorithm to preserve accuray
        /// accurate summation is necessary to match R and a good idea
        guessErrorLike = 0.0;
        c = 0.0;
        y = 0.0;
        t = 0.0;
        for (j=0; j<nwl*nspec; j++) {
            y = dnorm(guessError[j], 0.0, sdi, 1)-c;
            t = guessErrorLike+y;
            c = (t-guessErrorLike)-y;
            guessErrorLike = t;
        }
        // compute posteriors
        guessPosterior = guessErrorLike + dnorm(guessAlphaN, 0.0, sdreN, 1);
        prevPosterior = *prevErrorLike + dnorm(alphaNi[i], 0.0, sdreN, 1);
        // update?
        a = exp(guessPosterior-prevPosterior);
        if (isnan(a)) {
            a = -1.0;
        }
        GetRNGstate();
        if (a>unif_rand()) {
            alphaNi[i] = guessAlphaN;
            for (int j=0; j<nwl*nspec; j++) {
                prevError[j] = guessError[j];
            }
            *prevErrorLike = guessErrorLike;
            *arAlpha += 1;
        }
        PutRNGstate();
    }
    return(R_NilValue);
}
