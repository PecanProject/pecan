#include <Rmath.h>
#include <stdio.h>

extern "C" {
    double rnorm_(double mu, double sd){
        printf("Rnorm!");
        printf("%f", rnorm(0, 1));
        printf("\n");
        return rnorm(mu, sd);
    }
}

extern "C" {
    double dnorml_(double X, double mu, double sd){
        return dnorm(X, mu, sd, 1);
    }
}

extern "C" {
    double runif_(){
        return runif(0, 1);
    }
}

extern "C" {
    double rgamma_(double shape, double scale){
        return rgamma(shape, scale);
    }
}

//double rtnorm_(double mu, double sd, double MIN){
//    double x = rnorm(mu, sd);
//    if(x < MIN){
//        x = qnorm(runif(pnorm(MIN, mu, sd, 1, 0), 1), mu, sd, 1, 0);
//    }
//    return x;
//}
//
//double dtnorm_(double X, double mu, double sd, double MIN){
//    if(X < MIN){
//        return -1e15;
//    }
//    else {
//        return dnorm(X, mu, sd, 1) - log(1 - pnorm(MIN, mu, sd, 1, 0));
//    }
//}

