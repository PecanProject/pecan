#include "common_funcs.h"
#include <Rcpp.h>
using namespace Rcpp;

// Basic Metropolis Hastings
//      Sample parameters one at a time in a loop
void sampler_MH(
        NumericVector &inits,
        double rsd,
        NumericVector &Jump,
        NumericMatrix &Observed,
        NumericMatrix &PrevError,
        NumericVector &ar,
        NumericVector (*Model)(NumericVector, NumericMatrix),
        double (*Prior)(int, double),
        NumericVector &pmin,
        NumericMatrix &func_data)
{
    int npars = inits.size();
    int nwl = Observed.nrow(), nspec = Observed.ncol();
    NumericVector Tvec(npars), TrySpec(nwl);
    NumericMatrix TryError(nwl, nspec);
    double TryPost, PrevPost;
    double JN, JD, a;

    for(int p = 0; p<npars; p++){
        Tvec = clone(inits);
        Tvec[p] = rtnorm(inits[p], Jump[p], pmin[p]);
        TrySpec = Model(Tvec, func_data);
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
    }
    return ;
}
