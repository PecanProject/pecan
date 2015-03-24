#include <Rcpp.h>
#include <Rmath.h>
using namespace Rcpp;

double rtnorm(double mu, double sd, double MIN){
    double x = rnorm(1, mu, sd)[0];
    if(x < MIN){
        x = R::qnorm(runif(1, R::pnorm(MIN, mu, sd, 1, 0), 1)[0], mu, sd, 1, 0);
    }
    return x;
}

double dtnorm(double X, double mu, double sd, double MIN){
    if(X < MIN){
        return -1e15;
    }
    else {
        return R::dnorm(X, mu, sd, 1) - log(1 - R::pnorm(MIN, mu, sd, 1, 0));
    }
}

double rtnorm_c(double mu, double sd, double MIN){
    double M, S, L_min;
    L_min = (R::dnorm(MIN, mu, sd, 0))/(1 - R::pnorm(MIN, mu, sd, 1, 0));
    M = mu - sd*L_min;
    S = sqrt(pow(sd,2) / (1 - L_min * (L_min - (MIN - mu)/sd)));
    return rtnorm(M, S, MIN);
}

double dtnorm_c(double X, double mu, double sd, double MIN){
	if(X < MIN){
		return -1e15;
	}
	else {	
		double M, S, L_min;
		L_min = (R::dnorm(MIN, mu, sd, 0))/(1 - R::pnorm(MIN, mu, sd, 1, 0));
		M = mu - sd*L_min;
		S = sqrt(pow(sd,2) / (1 - L_min * (L_min - (MIN - mu)/sd)));
		return dtnorm(X, M, S, MIN);
	}
}
