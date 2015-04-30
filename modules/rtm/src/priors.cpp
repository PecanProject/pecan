#include <Rcpp.h>
#include <Rmath.h>
using namespace Rcpp;

// Priors for PROSPECT4 model
double prospect4_priors(int param, double value){
    double p;
    if(param == 0){         // N -- Halfnormal
        p = R::dnorm(value-1, 0.916, 2.2, 1) + log(2);
    } else if(param == 1){  // Cab -- Lognormal
        p = R::dnorm(log(value), 3.4, 0.9, 1);
    } else if(param == 2){  // Cw -- Lognormal
        p= R::dnorm(log(value), -6.377, 0.5, 1);
    } else if(param == 3){  // Cm -- Lognormal
        p = R::dnorm(log(value), -5.116, 0.9, 1);
    }
    return p;
}

// Priors for PROSPECT5 model
double prospect5_priors(int param, double value){
    double p;
    if(param == 0){         // N -- Halfnormal
        p = R::dnorm(value-1, 0.916, 2.2, 1) + log(2);
    } else if(param == 1){  // Cab -- Lognormal
        p = R::dnorm(log(value), 3.4, 0.9, 1);
    } else if(param == 2){  // Car -- Lognormal -- UNINFORMATIVE!
        p = R::dnorm(log(value), 1, 1, 1);
    } else if(param == 3){  // Cw -- Lognormal
        p= R::dnorm(log(value), -6.377, 0.5, 1);
    } else if(param == 4){  // Cm -- Lognormal
        p = R::dnorm(log(value), -5.116, 0.9, 1);
    }
    return p;
}

// Priors for PROSPECT5B model
double prospect5b_priors(int param, double value){
    double p;
    if(param == 0){         // N -- Halfnormal
        p = R::dnorm(value-1, 0.916, 2.2, 1) + log(2);
    } else if(param == 1){  // Cab -- Lognormal
        p = R::dnorm(log(value), 3.4, 0.9, 1);
    } else if(param == 2){  // Car -- Lognormal -- UNINFORMATIVE!
        p = R::dnorm(log(value), 1, 1, 1);
    } else if(param == 3){  // Cbrown -- Lognormal -- UNINFORMATIVE!
        p = R::dnorm(log(value), 1, 1, 1);
    } else if(param == 4){  // Cw -- Lognormal
        p= R::dnorm(log(value), -6.377, 0.5, 1);
    } else if(param == 5){  // Cm -- Lognormal
        p = R::dnorm(log(value), -5.116, 0.9, 1);
    }
    return p;
}