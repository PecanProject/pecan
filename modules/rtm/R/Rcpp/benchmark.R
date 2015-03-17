library(compiler)
library(microbenchmark)
library(ggplot2)

# Benchmarking
m <- prospect4(1.4, 30, 0.01, 0.01)
obs <- obs.spec

# Default R implementation
SpecError_R = function(Model, Observed) Model - Observed

# R implementation compiled
SpecError_RC = cmpfun(SpecError_R)

# Cpp - Loop by cell
cppFunction("NumericMatrix SpecError_C1(NumericVector Model, NumericMatrix Observed){
    int wl = Observed.nrow(), nspec = Observed.ncol();
    NumericMatrix E(wl, nspec);
    for(int i=0; i<wl; i++){
        for(int j=0; j<nspec; j++){
            E(i,j) = Model(i) - Observed(i,j);
        }
    }
    return E;
}")

# Cpp - Loop by column
cppFunction("NumericMatrix SpecError_C2(NumericVector Model, NumericMatrix Observed){
    int wl = Observed.nrow(), nspec = Observed.ncol();
    NumericMatrix E(wl, nspec);
    for(int i=0; i<nspec; i++){
        E(_,i) = Model - Observed(_,i);
    }
    return E;
}")


mb <- microbenchmark(SpecError_R(m, obs),
                     SpecError_RC(m,obs),
                     SpecError_C1(m,obs),
                     SpecError_C2(m,obs),
                     times = 10000)
autoplot(mb)

################

pe <- SpecError(m, obs)
rp1 <- 0.001 + 2101*78/2

sample_rsd_R <- function(E, rp1){
    rp2 <- 0.001 + sum(E*E)
    rinv <- rgamma(1, rp1, rp2)
    return(1/sqrt(rinv))
}

cppFunction("double sample_rsd_C(NumericMatrix PrevError, double rp1){
    			double rinv, rp2;
				rp2 = 0.001 + sum(PrevError*PrevError)/2;
    			rinv = rgamma(1, rp1, rp2)[0];
    			return 1/sqrt(rinv);
    			}")

mb2 <- microbenchmark(sample_rsd_R(pe, rp1), sample_rsd_C(pe, rp1), times=10000)

