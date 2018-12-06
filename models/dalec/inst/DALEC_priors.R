## DALEC PRIORS

# t2 - autotrophic_respiration_fraction
median = 0.4733
lo = 0.2
hi = 0.7

beta_fit <- function(alpha,median,lo,hi){
  beta = (alpha-1/3)/median +2/3 -alpha
  err = abs(lo - qbeta(0.025,alpha,beta))+abs(hi-qbeta(0.975,alpha,beta))
  return(err)
}
beta_fit(1,median,lo,hi) ## test default
#optimize(beta_fit,interval = c(0.1,1000),median=median,lo=lo,hi=hi)
fit = optim(par = 1,beta_fit,median=median,lo=lo,hi=hi)
alpha = fit$par
beta = (alpha-1/3)/median +2/3 -alpha
my.dbeta = function(x){dbeta(x,alpha,beta)}
curve(my.dbeta,from=0.01,to=.99)

# t3 - proportion of NPP allocated to foliage
median = 0.3150
lo = 0.01
hi = 0.5
beta_fit(1,median,lo,hi) ## test default
fit = optim(par = 1,beta_fit,median=median,lo=lo,hi=hi)
alpha = fit$par
beta = (alpha-1/3)/median +2/3 -alpha
curve(my.dbeta,from=0.01,to=.99)

#t4 proportion of NPP allocated to roots
median = 0.4344
lo = 0.01
hi = 0.5
hi = 0.6  ## relaxed Mat's default because the posterior was narrow 
beta_fit(1,median,lo,hi) ## test default
fit = optim(par = 1,beta_fit,median=median,lo=lo,hi=hi)
alpha = fit$par
beta = (alpha-1/3)/median +2/3 -alpha
curve(my.dbeta,from=0.01,to=.99)

#t6 proportion of woody material becoming woody debris every time step
median = 2.06e-6 * 365
lo = 2e-6 *365
hi = 0.02 * 365
gamma_fit <- function(theta,median,lo,hi){
  shape = theta[1]
  rate = theta[2]
  # no closed form median, so including into error, weighting median higher (10x)
  err = abs(lo - qgamma(0.025,shape,rate))+abs(hi-qgamma(0.975,shape,rate)) +
        10*abs(median-qgamma(0.5,shape,rate))
  return(err)
}
gamma_fit(c(1,1),median,lo,hi) ## test default
fit = optim(par = c(1,1),gamma_fit,median=median,lo=lo,hi=hi)
shape = fit$par[1]
rate = fit$par[2]
my.dgamma = function(x){dgamma(x,shape,rate)}
curve(my.dgamma,from=0.001,to=.02)
abline(v=median)
qgamma(0.5,shape,rate)

# t1 - rate variable controling decomposition from litter to soil organinc matter [day-1, ref T 10C]
median = 4.41e-6
lo = 1e-6
hi = 0.01
gamma_fit(c(1,1),median,lo,hi) ## test default
fit = optim(par = c(1,1),gamma_fit,median=median,lo=lo,hi=hi)
shape = fit$par[1]
rate = fit$par[2]
my.dgamma = function(x){dgamma(x,shape,rate)}
curve(my.dgamma,from=0.001,to=.02)
abline(v=median)
qgamma(0.5,shape,rate)
## GAMMA GAVE A BAD FIT SO SWITCHING TO EXPONENTIAL
rate = log(2)/median
curve(dexp(x,rate),lo,lo*100)
abline(v=median)
qexp(0.5,rate)
qexp(0.025,rate)
qexp(0.975,rate)

# t8 - rate variable controlling respiration from litter [day-1, ref T 10C]
median = 2.28e-2
lo = 5e-5
hi = 0.5
gamma_fit(c(1,1),median,lo,hi) ## test default
fit = optim(par = c(1,1),gamma_fit,median=median,lo=lo,hi=hi)
shape = fit$par[1]
rate = fit$par[2]
my.dgamma = function(x){dgamma(x,shape,rate)}
curve(my.dgamma,from=0.001,to=.02)
abline(v=median)
qgamma(c(0.025,0.5,0.975),shape,rate)

# t9 - rate variable controlling respiration from soil organic matter and woody debris [day-1, ref T 10C]
median = 2.65e-6
lo = 1e-6
hi = 0.5
gamma_fit(c(1,1),median,lo,hi) ## test default
fit = optim(par = c(1,1),gamma_fit,median=median,lo=lo,hi=hi)
shape = fit$par[1]
rate = fit$par[2]
my.dgamma = function(x){dgamma(x,shape,rate)}
curve(my.dgamma,from=0.001,to=.02)
abline(v=median)
qgamma(c(0.025,0.5,0.975),shape,rate)
## Gamma gave a poor fit to the median, trying exponential
rate = log(2)/median
curve(dexp(x,rate),lo,lo*100)
abline(v=median)
qexp(c(0.025,0.5,0.975),rate)
