// Truncated normal distribution

double rtnorm(double mu, double sd, double min, double max){
    double r;
    r = rnorm(mu, sd);
    while(r < min or r > max){
        r = rnorm(mu, sd);
    }
    return r;
}

double dtnorm(double X, double mu, double sd, double min, double max){
    double d;
    d = dnorm(X, mu, sd, 1) - 
        pnorm(max, mu, sd, 1, 1) +
        pnorm(min, mu, sd, 1, 1);
    return d;
}

