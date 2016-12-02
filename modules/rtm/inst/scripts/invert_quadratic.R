library(devtools)
#install('~/Projects/pecan/pecan/modules/rtm')
load_all('~/Projects/pecan/pecan/modules/assim.batch/')
load_all('~/Projects/pecan/pecan/modules/rtm')

true <- c(2, 3, 4)

nx <- 100
x <- seq(0, 10, length.out = nx)

model <- function(params) {
  params[1] * x^2 + params[2] * x + params[3]
}

set.seed(666)
y <- as.matrix(model(true) + generate.noise(n = nx, fw = 10, sigma = 1))
#plot(x, y, type='l')

invert.options <- list()
invert.options$model <- model
invert.options$prior.function <- function(p) sum(dnorm(p, 0, 30, TRUE)) 
invert.options$inits.function <- function() rnorm(3, 0, 30)
invert.options$inits <- invert.options$inits.function()
invert.options$nchains <- 3
#invert.options$ngibbs <- 10000
#invert.options$ngibbs <- 100

samp <- invert.auto(observed = y,
                    invert.options = invert.options)

#samp <- invert.custom(observed = y, 
                      #invert.options = invert.options)
#smcmc <- coda::as.mcmc(samp)
#plot(smcmc)
