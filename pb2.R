### Alternate version of PROSPECT bayes inversion

source("prospect.R")
source("timer.R")
source("truncnorm.R")
#source("specdataproc.R")
#source("mle_inversion.R")

### Gibbs parameters
ngibbs <- 1000
JumpRSD <- 0.01
n.error <- length(OBS)

### Initial conditions
N <- 1.4
Cab <- 30
Cw <- 0.017
Cm <- 0.06
stdv <- 1

## Test spectrum
OBS <- prospect4(N, Cab, Cw, Cm, n.a, cab.a, w.a, m.a) + rnorm(2101, 0, 0.01)

N.jsd <- N * JumpRSD
Cab.jsd <- Cab * JumpRSD
Cw.jsd <- Cw * JumpRSD
Cm.jsd <- Cm * JumpRSD


### Priors ###
N.prior <- function(N) dnorm(N - 1, 0, 1.5, log=TRUE) + log(2)
Cab.prior <- function(Cab) dlnorm(Cab, log(30), 0.9, log=TRUE)
Cw.prior <- function(Cw) dlnorm(Cw, log(0.017), 0.5, log=TRUE)
Cm.prior <- function(Cm) dlnorm(Cm, log(0.006), 0.9, log=TRUE)

# Error - Inverse gamma (gamma on precision)
stdv.p1 <- 0.001
stdv.p2 <- 0.001


### Sampler functions ###
mod.spec <- function(N, Cab, Cw, Cm) prospect4(N, Cab, Cw, Cm, n.a, cab.a, w.a, m.a)
residual <- function(model, observed) model - observed
likelihood <- function(residual, stdv) sum(dnorm(residual, 0, stdv, log = TRUE))


### Initial posterior ###
g.model <- mod.spec(N, Cab, Cw, Cm)
g.residual <- residual(g.model, OBS)
g.posterior <- likelihood(g.residual, stdv) +
                N.prior(N) + 
                Cab.prior(Cab) +
                Cw.prior(Cw) + 
                Cm.prior(Cm) 


### MCMC storage ###
N.store <- numeric(ngibbs)
stdv.store <- numeric(ngibbs)

dresid <- numeric(ngibbs)


### Gibbs loop ###
#plot(OBS, type='l', lwd=8)
for (g in 1:ngibbs){

        ### Sample N ###
        
        # Random N
        N.g <- rtnorm(1, N, N.jsd, Min=1)
        
        # Evaluate spectra
        N.model <- mod.spec(N.g, Cab, Cw, Cm)
        N.residual <- residual(N.model, OBS)

        dresid[g] <- (sum(g.residual - N.residual))

        # Calculate posterior
        N.posterior <- likelihood(N.residual, stdv) + N.prior(N.g)
        g.posterior <- likelihood(g.residual, stdv) + N.prior(N)

        # Test acceptance
        jnum <- dtnorm(N.g, N, N.jsd)
        jden <- dtnorm(N, N.g, N.jsd)
        a <- exp((N.posterior - jnum) - (g.posterior - jden))
#        print(c(N.posterior, g.posterior, jnum, jden, a))
        if(a > runif(1)){
                N <- N.g
                g.residual <- N.residual
#                print("Accepted!")
        }


        ### Sample error ###
        u1 <- stdv.p1 + n.error/2
        u2 <- stdv.p2 + sum(g.residual^2)
        prec <- rgamma(1, u1, rate = u2)
        stdv <- 1/sqrt(prec)
        print(stdv)

        ### Store values
        N.store[g] <- N
        stdv.store[g] <- stdv

}

plot(N.store, type='l')
plot(stdv.store, type='l')
